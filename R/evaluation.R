# Evaluation summaries -------------------------------------------------------
#
# Lapse comparisons are sensitivity analyses. The dataset has no verified churn
# labels, so precision, recall or accuracy cannot be computed for any rule.

# One row per group with its customer count and associated historical value.
rp_group_row <- function(group, mask, population, value) {
  data.frame(
    group = group,
    customers = sum(mask),
    share_of_population = sum(mask) / population,
    historical_value = sum(value[mask]),
    stringsAsFactors = FALSE
  )
}

# Evaluation A: personalized rule vs fixed rule on the same population
# (customers with enough history for personalized cadence analysis).
rp_evaluate_rules_same_population <- function(lapse) {
  pop <- lapse[lapse$cadence_qualified, , drop = FALSE]
  p <- pop$personalized_flag
  f <- pop$fixed_flag
  n <- nrow(pop)
  v <- pop$monetary
  table <- rbind(
    rp_group_row("Evaluation population (qualified customers)", rep(TRUE, n), n, v),
    rp_group_row("Flagged by personalized rule", p, n, v),
    rp_group_row("Flagged by fixed rule", f, n, v),
    rp_group_row("Flagged by both", p & f, n, v),
    rp_group_row("Flagged only by personalized rule", p & !f, n, v),
    rp_group_row("Flagged only by fixed rule", !p & f, n, v),
    rp_group_row("Flagged by neither", !p & !f, n, v)
  )
  table$share_of_value <- table$historical_value / sum(v)
  either <- sum(p | f)
  list(
    table = table,
    population = n,
    jaccard = if (either == 0) NA_real_ else sum(p & f) / either
  )
}

# Evaluation B: the complete hybrid policy across all customers, with the
# fixed rule on all customers shown for reference.
rp_evaluate_hybrid_policy <- function(lapse) {
  n <- nrow(lapse)
  v <- lapse$monetary
  q <- lapse$cadence_qualified
  h <- lapse$hybrid_flag
  table <- rbind(
    rp_group_row("All customers", rep(TRUE, n), n, v),
    rp_group_row("Evaluated by personalized cadence", q, n, v),
    rp_group_row("Evaluated by fixed fallback", !q, n, v),
    rp_group_row("Flagged: personalized cadence signal", q & h, n, v),
    rp_group_row("Flagged: fallback threshold signal", !q & h, n, v),
    rp_group_row("Flagged in total (hybrid policy)", h, n, v),
    rp_group_row("Reference: fixed rule applied to all customers", lapse$fixed_flag, n, v),
    rp_group_row("Hybrid and all-customer fixed rule agree", h == lapse$fixed_flag, n, v)
  )
  table$share_of_value <- table$historical_value / sum(v)
  table
}

# Flag counts across a grid of thresholds (sensitivity of each rule).
rp_lapse_sensitivity <- function(customers, ratios = c(1.5, 2, 2.5, 3, 4),
                                 days = c(30, 45, 60, 90, 120, 180)) {
  q <- customers$cadence_qualified
  pop_ratio <- customers$recency[q] / customers$typical_interval[q]
  personalized <- data.frame(
    rule = "Personalized (qualified customers)",
    threshold = paste0("ratio >= ", ratios),
    flagged = vapply(ratios, function(r) sum(pop_ratio >= r), integer(1)),
    population = sum(q)
  )
  fixed_q <- data.frame(
    rule = "Fixed (qualified customers)",
    threshold = paste0("recency >= ", days, " days"),
    flagged = vapply(days, function(d) sum(customers$recency[q] >= d), integer(1)),
    population = sum(q)
  )
  fixed_all <- data.frame(
    rule = "Fixed (all customers)",
    threshold = paste0("recency >= ", days, " days"),
    flagged = vapply(days, function(d) sum(customers$recency >= d), integer(1)),
    population = nrow(customers)
  )
  out <- rbind(personalized, fixed_q, fixed_all)
  out$share_flagged <- out$flagged / out$population
  out
}

# Data-quality summary: raw diagnostics (overlapping) and the exclusion funnel
# (mutually exclusive), in one display table.
rp_data_quality_table <- function(audit, cleaning) {
  s <- cleaning$summary
  rows <- list(
    c("Raw input", "Transaction rows in workbook", audit$n_rows, "All rows read from the sheet"),
    c("Raw input", "Cells that failed type conversion", audit$n_read_warnings + audit$n_parse_errors, "Reported by the reader or the parser; such rows are excluded as invalid"),
    c("Raw input", "Rows missing CustomerID", audit$n_missing_customer, "Cannot be attributed to a customer"),
    c("Raw input", "Rows missing Description", audit$n_missing_description, "Description is not used in calculations"),
    c("Raw input", "Cancellation rows (invoice starts with C)", audit$n_cancellation_rows, sprintf("%s distinct cancellation invoices", rp_fmt_int(audit$n_cancellation_invoices))),
    c("Raw input", "Adjustment rows (invoice starts with A)", audit$n_adjustment_rows, "Bad-debt adjustments, not purchases"),
    c("Raw input", "Rows with negative quantity", audit$n_negative_quantity, sprintf("%s are not on C invoices", rp_fmt_int(audit$n_negative_quantity_not_cancellation))),
    c("Raw input", "Rows with non-positive unit price", audit$n_nonpositive_price, sprintf("%s negative", rp_fmt_int(audit$n_negative_price))),
    c("Raw input", "Distinct invoices (all types)", audit$n_invoices, ""),
    c("Preliminary baseline", "Eligible rows (ID present, not C/A, quantity > 0, price > 0)", s$preliminary_rows, rp_fmt_gbp(s$preliminary_value)),
    c("Preliminary baseline", "Customers", s$preliminary_customers, ""),
    c("Preliminary baseline", "Distinct invoices", s$preliminary_invoices, ""),
    c("Cancellation matching", "Cancellation lines examined", s$cancellation_lines, ""),
    c("Cancellation matching", "Matched one-to-one to an earlier purchase line", s$cancellations_matched, sprintf("%s had several candidates", rp_fmt_int(s$cancellations_ambiguous))),
    c("Cancellation matching", "Left unresolved", s$cancellations_unmatched, "No purchase removed for these"),
    c("Exclusions", "Purchase lines removed as later cancelled", s$matched_rows_removed, rp_fmt_gbp(s$matched_value_removed)),
    c("Exclusions", "Non-product service or charge lines removed", s$non_product_rows_removed, rp_fmt_gbp(s$non_product_value_removed)),
    c("Final analytical dataset", "Rows", s$final_rows, rp_fmt_gbp(s$final_value)),
    c("Final analytical dataset", "Identifiable customers", s$final_customers, sprintf("%s baseline customers had no remaining eligible lines", rp_fmt_int(length(s$customers_lost)))),
    c("Final analytical dataset", "Distinct purchase invoices", s$final_invoices, ""),
    c("Final analytical dataset", "Countries", s$final_countries, "")
  )
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(out) <- c("stage", "measure", "count", "note")
  out$count <- as.numeric(out$count)
  out
}

# Business utility of alternative segmentations --------------------------------
#
# Silhouette measures statistical separation only. These measures describe what
# each segmentation lets a user do: how finely it divides customers and value,
# whether its segments differ in lapse-signal rates, and whether it isolates
# groups with a distinct suggested use. All values come from the data; the
# suggested uses are interpretations (see RP_SEGMENT_USES).

# Per-segment table for one k, combining the profile with lapse outcomes.
rp_segment_utility_table <- function(seg, customers, k, ratio_threshold, fixed_days) {
  view <- rp_segment_view(seg, customers, k)
  lapse <- rp_apply_lapse_rules(view$customers, ratio_threshold, fixed_days)
  by_seg <- lapse |>
    dplyr::group_by(segment_id) |>
    dplyr::summarise(
      qualified_share = mean(cadence_qualified),
      flagged = sum(hybrid_flag),
      flagged_share = mean(hybrid_flag),
      flagged_value = sum(monetary[hybrid_flag]),
      .groups = "drop"
    )
  p <- dplyr::left_join(view$profile, by_seg, by = "segment_id")
  data.frame(
    k = view$k, segment_id = p$segment_id, segment = p$segment, segment_name = p$segment_name,
    customers = p$customers, share_customers = p$share_customers,
    median_recency = p$median_recency, median_frequency = p$median_frequency,
    median_monetary = p$median_monetary, total_value = p$total_value, share_value = p$share_value,
    qualified_share = p$qualified_share, flagged = p$flagged, flagged_share = p$flagged_share,
    flagged_value = p$flagged_value, suggested_use = p$suggested_use,
    stringsAsFactors = FALSE
  )
}

# Summary measures for each k, in long form (k, measure, value, display).
rp_segmentation_utility <- function(seg, customers, ks, ratio_threshold, fixed_days) {
  ks <- intersect(as.integer(ks), seg$candidates$k[seg$candidates$valid])
  segments <- do.call(rbind, lapply(ks, function(k) {
    rp_segment_utility_table(seg, customers, k, ratio_threshold, fixed_days)
  }))
  summary <- do.call(rbind, lapply(ks, function(k) {
    t <- segments[segments$k == k, ]
    top <- t[which.max(t$median_monetary), ]
    winback <- t$segment_name == "Historically valuable, currently inactive"
    m <- list(
      c("Mean silhouette (statistical separation)", seg$candidates$silhouette[seg$candidates$k == k], sprintf("%.3f", seg$candidates$silhouette[seg$candidates$k == k])),
      c("Number of segments", k, as.character(k)),
      c("Distinct suggested actions", length(unique(t$segment_name)), as.character(length(unique(t$segment_name)))),
      c("Smallest segment (share of customers)", min(t$share_customers), rp_fmt_pct(min(t$share_customers))),
      c("Largest segment (share of customers)", max(t$share_customers), rp_fmt_pct(max(t$share_customers))),
      c("Highest-value segment: customers / value", top$share_value, paste0(rp_fmt_pct(top$share_customers), " of customers hold ", rp_fmt_pct(top$share_value), " of value")),
      c("Lapse-signal rate range across segments", max(t$flagged_share) - min(t$flagged_share), paste0(rp_fmt_pct(min(t$flagged_share), 0), " to ", rp_fmt_pct(max(t$flagged_share), 0))),
      c("Customers in a 'review for win-back' segment", sum(t$customers[winback]), if (any(winback)) paste0(rp_fmt_int(sum(t$customers[winback])), " (", rp_fmt_gbp(sum(t$total_value[winback])), " historical value)") else "none isolated"),
      c("Flagged customers inside high-value active segment(s)", sum(t$flagged[t$segment_name == "High-value active customers"]), rp_fmt_int(sum(t$flagged[t$segment_name == "High-value active customers"])))
    )
    data.frame(
      k = k, measure = vapply(m, `[`, "", 1),
      value = as.numeric(vapply(m, `[`, "", 2)), display = vapply(m, `[`, "", 3),
      stringsAsFactors = FALSE
    )
  }))
  list(summary = summary, segments = segments)
}

# Plain-language findings comparing a coarser and a finer segmentation, built
# only from the computed tables.
rp_segmentation_insights <- function(seg, customers, k_a, k_b, utility) {
  ka <- min(k_a, k_b)
  kb <- max(k_a, k_b)
  if (ka == kb) return(character())
  sa <- utility$segments[utility$segments$k == ka, ]
  sb <- utility$segments[utility$segments$k == kb, ]
  sil <- function(k) seg$candidates$silhouette[seg$candidates$k == k]
  cw <- rp_segment_crosswalk(seg, customers, ka, kb)

  out <- sprintf(
    "Separation: k = %d has mean silhouette %.3f, k = %d has %.3f. The coarser view is statistically cleaner; the finer view trades %.3f of silhouette for %d more segments.",
    ka, sil(ka), kb, sil(kb), sil(ka) - sil(kb), kb - ka
  )
  for (i in seq_len(nrow(sb))) {
    s <- sb$segment[i]
    parts <- cw[cw$to == s, ]
    parts <- parts[order(-parts$customers), ]
    origin <- paste(sprintf("%s from %s", rp_fmt_pct(parts$share_of_to, 0), parts$from), collapse = ", ")
    out <- c(out, sprintf(
      "%s: %s customers (%s of value), lapse-signal rate %s; drawn %s at k = %d. Suggested use: %s",
      s, rp_fmt_int(sb$customers[i]), rp_fmt_pct(sb$share_value[i]),
      rp_fmt_pct(sb$flagged_share[i], 0), origin, ka, tolower(sb$suggested_use[i])
    ))
  }
  for (i in seq_len(nrow(sa))) {
    parts <- cw[cw$from == sa$segment[i], ]
    if (nrow(parts) < 2) next
    parts <- parts[order(-parts$customers), ]
    out <- c(out, sprintf(
      "At k = %d, %s (%s customers) is not homogeneous: at k = %d it splits into %s.",
      ka, sa$segment[i], rp_fmt_int(sa$customers[i]), kb,
      paste(sprintf("%s %s (%s)", rp_fmt_int(parts$customers), parts$to, rp_fmt_gbp(parts$value)), collapse = "; ")
    ))
  }
  wa <- sum(sa$segment_name == "Historically valuable, currently inactive")
  wb <- sum(sb$segment_name == "Historically valuable, currently inactive")
  if (wb > wa) {
    out <- c(out, sprintf(
      "Only k = %d isolates a 'historically valuable, currently inactive' group, which is the natural target list for a win-back review; at k = %d these customers are mixed into broader segments.",
      kb, ka
    ))
  }
  out
}
