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
