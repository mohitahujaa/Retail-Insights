# K-Means customer segmentation ---------------------------------------------

# Build the clustering matrix: log1p-transform the RFM features (they are
# non-negative and strongly right-skewed), then standardise to mean 0 / sd 1 so
# that no feature dominates the Euclidean distance. Invalid inputs stop with a
# clear message instead of producing NaN clusters.
rp_prepare_cluster_matrix <- function(customers, features = rp_config()$cluster_features,
                                      transform = c("log1p", "none")) {
  transform <- match.arg(transform)
  missing <- setdiff(features, names(customers))
  if (length(missing) > 0) {
    stop("Clustering feature(s) not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  X <- as.matrix(customers[, features, drop = FALSE])
  if (!is.numeric(X)) stop("Clustering features must be numeric.", call. = FALSE)
  if (nrow(X) < 2) stop("At least two customers are needed for clustering.", call. = FALSE)
  if (any(!is.finite(X))) {
    stop("Clustering features contain missing or non-finite values.", call. = FALSE)
  }
  if (transform == "log1p") {
    if (any(X < 0)) stop("log1p transform requires non-negative features.", call. = FALSE)
    X <- log1p(X)
  }
  sds <- apply(X, 2, stats::sd)
  if (any(sds == 0)) {
    stop("Zero-variance clustering feature(s): ",
      paste(features[sds == 0], collapse = ", "),
      call. = FALSE
    )
  }
  X <- scale(X)
  if ("CustomerID" %in% names(customers)) rownames(X) <- customers$CustomerID
  attr(X, "transform") <- transform
  X
}

# Fit K-Means for each candidate k and score it with the mean silhouette width.
#
# Every fit uses the same seed, so results are reproducible. A candidate is
# marked invalid (and excluded from selection) if K-Means fails, if k is not
# smaller than the number of customers, or if any cluster has fewer than
# `min_cluster_size` members.
rp_fit_kmeans_candidates <- function(X, cfg = rp_config()) {
  n <- nrow(X)
  sample_idx <- seq_len(n)
  sampled <- n > cfg$silhouette_max_n
  if (sampled) {
    set.seed(cfg$seed)
    sample_idx <- sort(sample.int(n, cfg$silhouette_max_n))
  }
  d <- stats::dist(X[sample_idx, , drop = FALSE])

  fits <- list()
  rows <- lapply(cfg$k_range, function(k) {
    row <- data.frame(
      k = k, valid = FALSE, silhouette = NA_real_, tot_withinss = NA_real_,
      between_ss_ratio = NA_real_, min_cluster_size = NA_integer_,
      note = "", stringsAsFactors = FALSE
    )
    if (k >= n) {
      row$note <- "k is not smaller than the number of customers"
      return(row)
    }
    warn <- character()
    set.seed(cfg$seed)
    fit <- tryCatch(
      withCallingHandlers(
        stats::kmeans(X, centers = k, nstart = cfg$nstart, iter.max = cfg$iter_max),
        warning = function(w) {
          warn <<- c(warn, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      row$note <- paste("K-Means failed:", conditionMessage(fit))
      return(row)
    }
    fits[[as.character(k)]] <<- fit
    row$tot_withinss <- fit$tot.withinss
    row$between_ss_ratio <- fit$betweenss / fit$totss
    row$min_cluster_size <- min(fit$size)
    if (length(unique(fit$cluster)) < k || min(fit$size) < cfg$min_cluster_size) {
      row$note <- paste0("Degenerate: smallest cluster has ", min(fit$size), " customers")
      return(row)
    }
    sil <- cluster::silhouette(fit$cluster[sample_idx], d)
    row$silhouette <- mean(sil[, "sil_width"])
    row$valid <- TRUE
    row$note <- paste(c(if (sampled) "silhouette on reproducible sample", warn), collapse = "; ")
    row
  })
  scores <- do.call(rbind, rows)
  list(scores = scores, fits = fits, silhouette_sampled = sampled,
       silhouette_n = length(sample_idx))
}

# Choose k: the highest mean silhouette among valid candidates, with ties
# (within 1e-9) going to the smaller k. A configured override is honoured only
# if that candidate is valid.
rp_select_k <- function(scores, k_override = NULL) {
  valid <- scores[scores$valid & !is.na(scores$silhouette), , drop = FALSE]
  if (nrow(valid) == 0) stop("No valid K-Means candidate could be fitted.", call. = FALSE)
  if (!is.null(k_override)) {
    if (k_override %in% valid$k) {
      return(list(k = as.integer(k_override), rule = "Configured override (k_override)"))
    }
    warning("k_override = ", k_override, " is not a valid candidate; using silhouette rule.")
  }
  best <- max(valid$silhouette)
  k <- min(valid$k[valid$silhouette >= best - 1e-9])
  list(k = as.integer(k), rule = "Highest mean silhouette width (ties to smaller k)")
}

# K-Means cluster numbers are arbitrary. For display, segments are renumbered
# S1..Sk in descending order of median monetary value (then mean monetary).
# This ordering is a presentation convention, not a quality ranking.
rp_renumber_clusters <- function(cluster, monetary) {
  med <- tapply(monetary, cluster, stats::median)
  avg <- tapply(monetary, cluster, mean)
  ord <- order(-med, -avg)
  mapping <- stats::setNames(seq_along(ord), names(med)[ord])
  as.integer(mapping[as.character(cluster)])
}

# Profile each segment in original RFM units.
rp_profile_segments <- function(customers) {
  customers |>
    dplyr::group_by(segment_id) |>
    dplyr::summarise(
      customers = dplyr::n(),
      median_recency = stats::median(recency), mean_recency = mean(recency),
      median_frequency = stats::median(frequency), mean_frequency = mean(frequency),
      median_monetary = stats::median(monetary), mean_monetary = mean(monetary),
      total_value = sum(monetary),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      share_customers = customers / sum(customers),
      share_value = total_value / sum(total_value)
    ) |>
    as.data.frame()
}

# Derive a descriptive name for each segment by comparing its median R, F and M
# with the median customer. Names are rules applied to observed profiles, not
# predetermined categories.
rp_name_segments <- function(profile, customers) {
  overall <- c(
    recency = stats::median(customers$recency),
    frequency = stats::median(customers$frequency),
    monetary = stats::median(customers$monetary)
  )
  recent <- profile$median_recency <= overall["recency"]
  freq_hi <- profile$median_frequency > overall["frequency"]
  value_hi <- profile$median_monetary > overall["monetary"]
  name <- dplyr::case_when(
    recent & freq_hi & value_hi ~ "High-value active customers",
    recent & freq_hi & !value_hi ~ "Recent frequent, lower-value buyers",
    recent & !freq_hi & value_hi ~ "Recent occasional, higher-value buyers",
    recent & !freq_hi & !value_hi ~ "Recent, low-activity customers",
    !recent & (freq_hi | value_hi) ~ "Historically valuable, currently inactive",
    TRUE ~ "Infrequent, inactive customers"
  )
  # Segments sharing a name keep it but are told apart by their S-number.
  profile$segment_name <- name
  profile$segment <- paste0("S", profile$segment_id, ": ", name)
  profile$basis <- sprintf(
    "Median recency %s days (all customers: %s), median frequency %s invoices (all: %s), median value %s (all: %s)",
    format(round(profile$median_recency), big.mark = ","),
    format(round(overall["recency"]), big.mark = ","),
    format(profile$median_frequency), format(overall["frequency"]),
    rp_fmt_gbp(profile$median_monetary), rp_fmt_gbp(overall["monetary"])
  )
  profile
}

# Full segmentation workflow. Returns customers with segment assignments plus
# the candidate scores, selected k, segment profiles and interpretation notes.
rp_segment_customers <- function(customers, cfg = rp_config()) {
  X <- rp_prepare_cluster_matrix(customers, cfg$cluster_features, "log1p")
  candidates <- rp_fit_kmeans_candidates(X, cfg)
  sel <- rp_select_k(candidates$scores, cfg$k_override)
  fit <- candidates$fits[[as.character(sel$k)]]

  customers$segment_id <- rp_renumber_clusters(fit$cluster, customers$monetary)
  profile <- rp_name_segments(rp_profile_segments(customers), customers)
  customers$segment <- profile$segment[match(customers$segment_id, profile$segment_id)]

  # Cluster centres back on the transformed (log1p) and original scales.
  centers_scaled <- fit$centers
  ctr <- attr(X, "scaled:center")
  scl <- attr(X, "scaled:scale")
  centers_log <- sweep(sweep(centers_scaled, 2, scl, `*`), 2, ctr, `+`)
  old_to_new <- tapply(customers$segment_id, fit$cluster, function(x) x[1])
  centers <- data.frame(
    segment_id = as.integer(old_to_new[as.character(seq_len(nrow(centers_scaled)))]),
    expm1(centers_log), check.names = FALSE
  )
  centers <- centers[order(centers$segment_id), , drop = FALSE]
  names(centers)[-1] <- paste0("center_", cfg$cluster_features)

  # Profiles for every valid candidate k, so the trade-off between separation
  # (silhouette) and granularity can be inspected. Only the selected k is used
  # for the rest of the analysis.
  candidate_profiles <- do.call(rbind, lapply(names(candidates$fits), function(k) {
    tmp <- customers
    tmp$segment_id <- rp_renumber_clusters(candidates$fits[[k]]$cluster, customers$monetary)
    prof <- rp_name_segments(rp_profile_segments(tmp), customers)
    cbind(k = as.integer(k), prof)
  }))
  valid_k <- candidates$scores$k[candidates$scores$valid]
  candidate_profiles <- candidate_profiles[candidate_profiles$k %in% valid_k, , drop = FALSE]
  candidate_profiles$suggested_use <- rp_segment_use(candidate_profiles$segment_name)

  # Segment number of every customer under every valid k (same numbering as
  # candidate_profiles), so the dashboard can switch views without refitting.
  assignments <- data.frame(CustomerID = customers$CustomerID, stringsAsFactors = FALSE)
  for (k in valid_k) {
    assignments[[paste0("k", k)]] <- rp_renumber_clusters(
      candidates$fits[[as.character(k)]]$cluster, customers$monetary
    )
  }

  sel_row <- candidates$scores[candidates$scores$k == sel$k, ]
  warnings <- c(
    if (sel_row$silhouette < 0.25) "Mean silhouette below 0.25: clusters overlap substantially; treat segments as broad tendencies." else if (sel_row$silhouette < 0.5) "Mean silhouette between 0.25 and 0.5: moderate separation; segment boundaries are not sharp." ,
    if (candidates$silhouette_sampled) sprintf("Silhouette computed on a reproducible sample of %d customers.", candidates$silhouette_n),
    if (any(duplicated(profile$segment_name))) "Two or more segments share the same descriptive name; they differ in degree, see the profile table."
  )

  list(
    customers = customers, profile = profile, candidates = candidates$scores,
    candidate_profiles = candidate_profiles, assignments = assignments,
    k = sel$k, selection_rule = sel$rule, silhouette = sel_row$silhouette,
    centers = centers, warnings = warnings,
    preprocessing = list(
      features = cfg$cluster_features, transform = "log1p",
      center = ctr, scale = scl
    )
  )
}

# Suggested business use for each segment name. These are interpretations of
# the profile pattern, not effects measured in this data (the dataset records no
# marketing actions).
RP_SEGMENT_USES <- c(
  "High-value active customers" = "Protect and retain: the core of current purchase value; watch personalized lapse signals closely.",
  "Recent frequent, lower-value buyers" = "Grow basket size: engaged customers with low spend.",
  "Recent occasional, higher-value buyers" = "Encourage repeat purchase: valuable orders that are not yet habitual.",
  "Recent, low-activity customers" = "Nurture a second or third purchase: recently active but little history.",
  "Historically valuable, currently inactive" = "Review for win-back: meaningful past value but activity has slowed or stopped.",
  "Infrequent, inactive customers" = "Low-cost reactivation or deprioritise: little history and long inactivity."
)

rp_segment_use <- function(segment_name) unname(RP_SEGMENT_USES[segment_name])

# Customers and profile for any valid k ("segmentation view").
rp_segment_view <- function(seg, customers, k) {
  k <- as.integer(k)
  col <- paste0("k", k)
  if (!col %in% names(seg$assignments)) stop("k = ", k, " is not a valid fitted candidate.", call. = FALSE)
  profile <- seg$candidate_profiles[seg$candidate_profiles$k == k, , drop = FALSE]
  profile$k <- NULL
  rownames(profile) <- NULL
  ids <- seg$assignments[[col]][match(customers$CustomerID, seg$assignments$CustomerID)]
  customers$segment_id <- ids
  customers$segment <- profile$segment[match(ids, profile$segment_id)]
  list(
    k = k, customers = customers, profile = profile,
    silhouette = seg$candidates$silhouette[seg$candidates$k == k],
    is_selected = k == seg$k
  )
}

# How customers move between two segmentations: counts and value per cell.
rp_segment_crosswalk <- function(seg, customers, k_from, k_to) {
  a <- rp_segment_view(seg, customers, k_from)$customers
  b <- rp_segment_view(seg, customers, k_to)$customers
  d <- data.frame(from = a$segment, to = b$segment, value = a$monetary)
  d |>
    dplyr::group_by(to, from) |>
    dplyr::summarise(customers = dplyr::n(), value = sum(value), .groups = "drop") |>
    dplyr::group_by(to) |>
    dplyr::mutate(share_of_to = customers / sum(customers)) |>
    dplyr::ungroup() |>
    as.data.frame()
}

# Optional extension: rule-based RFM quintile scores --------------------------

# Score each feature 1-5 using quintile breaks of the customer distribution.
# Tied values always receive the same score, so a heavily tied feature (such as
# frequency, where many customers have one invoice) may use fewer than five
# distinct scores. Recency is reversed so that 5 = most recent.
rp_quintile_score <- function(x, reverse = FALSE) {
  breaks <- unique(stats::quantile(x, c(0.2, 0.4, 0.6, 0.8), names = FALSE))
  s <- findInterval(x, breaks, left.open = TRUE) + 1L
  if (reverse) 6L - s else s
}

rp_rfm_rule_segments <- function(customers) {
  r <- rp_quintile_score(customers$recency, reverse = TRUE)
  f <- rp_quintile_score(customers$frequency)
  m <- rp_quintile_score(customers$monetary)
  fm <- (f + m) / 2
  rule <- dplyr::case_when(
    r >= 4 & fm >= 4 ~ "R1: Recent, high frequency/value",
    r >= 3 & fm >= 3 ~ "R2: Fairly recent, mid-high frequency/value",
    r >= 4 & fm < 3 ~ "R3: Recent, low frequency/value",
    r <= 2 & fm >= 3 ~ "R4: Not recent, mid-high frequency/value",
    r <= 2 & fm < 3 ~ "R5: Not recent, low frequency/value",
    TRUE ~ "R6: Middle recency, low frequency/value"
  )
  data.frame(
    CustomerID = customers$CustomerID, r_score = r, f_score = f, m_score = m,
    rfm_rule_segment = rule, stringsAsFactors = FALSE
  )
}

# Adjusted Rand Index between two partitions (1 = identical, ~0 = chance).
rp_adjusted_rand <- function(a, b) {
  tab <- table(a, b)
  comb2 <- function(x) x * (x - 1) / 2
  sum_ij <- sum(comb2(tab))
  sum_a <- sum(comb2(rowSums(tab)))
  sum_b <- sum(comb2(colSums(tab)))
  expected <- sum_a * sum_b / comb2(sum(tab))
  max_index <- (sum_a + sum_b) / 2
  if (max_index == expected) return(1)
  (sum_ij - expected) / (max_index - expected)
}

rp_compare_rfm_rules <- function(seg_customers) {
  rules <- rp_rfm_rule_segments(seg_customers)
  joined <- dplyr::left_join(
    seg_customers[, c("CustomerID", "segment", "recency", "frequency", "monetary")],
    rules,
    by = "CustomerID"
  )
  rule_profile <- joined |>
    dplyr::group_by(rfm_rule_segment) |>
    dplyr::summarise(
      customers = dplyr::n(),
      median_recency = stats::median(recency),
      median_frequency = stats::median(frequency),
      median_monetary = stats::median(monetary),
      total_value = sum(monetary), .groups = "drop"
    ) |>
    dplyr::mutate(share_customers = customers / sum(customers)) |>
    as.data.frame()
  crosstab <- as.data.frame.matrix(table(joined$rfm_rule_segment, joined$segment))
  list(
    customers = joined, rule_profile = rule_profile, crosstab = crosstab,
    adjusted_rand = rp_adjusted_rand(joined$rfm_rule_segment, joined$segment),
    score_levels = c(
      r = length(unique(rules$r_score)), f = length(unique(rules$f_score)),
      m = length(unique(rules$m_score))
    )
  )
}
