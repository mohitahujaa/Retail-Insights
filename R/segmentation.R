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
  candidate_profiles <- candidate_profiles[
    candidate_profiles$k %in% candidates$scores$k[candidates$scores$valid], ,
    drop = FALSE
  ]

  sel_row <- candidates$scores[candidates$scores$k == sel$k, ]
  warnings <- c(
    if (sel_row$silhouette < 0.25) "Mean silhouette below 0.25: clusters overlap substantially; treat segments as broad tendencies." else if (sel_row$silhouette < 0.5) "Mean silhouette between 0.25 and 0.5: moderate separation; segment boundaries are not sharp." ,
    if (candidates$silhouette_sampled) sprintf("Silhouette computed on a reproducible sample of %d customers.", candidates$silhouette_n),
    if (any(duplicated(profile$segment_name))) "Two or more segments share the same descriptive name; they differ in degree, see the profile table."
  )

  list(
    customers = customers, profile = profile, candidates = candidates$scores,
    candidate_profiles = candidate_profiles,
    k = sel$k, selection_rule = sel$rule, silhouette = sel_row$silhouette,
    centers = centers, warnings = warnings,
    preprocessing = list(
      features = cfg$cluster_features, transform = "log1p",
      center = ctr, scale = scl
    )
  )
}
