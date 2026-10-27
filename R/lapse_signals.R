# Personalized purchase-lapse signals ----------------------------------------
#
# Heuristic decision support, not a churn model: a signal means a customer's
# current inactivity is long relative to their own purchase history (or, for
# customers with too little history, longer than a fixed number of days).

# Apply the personalized, fixed and hybrid rules to customers that already
# carry cadence fields (see rp_add_cadence()).
#
# personalized_flag: inactivity ratio >= ratio_threshold; NA for customers
#                    without enough history (never imputed).
# fixed_flag:        recency >= fixed_days; defined for every customer.
# hybrid_flag:       personalized rule where qualified, fixed fallback otherwise.
rp_apply_lapse_rules <- function(customers, ratio_threshold = rp_config()$ratio_threshold,
                                 fixed_days = rp_config()$fixed_days) {
  if (!is.numeric(ratio_threshold) || length(ratio_threshold) != 1 || ratio_threshold <= 0) {
    stop("ratio_threshold must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(fixed_days) || length(fixed_days) != 1 || fixed_days <= 0) {
    stop("fixed_days must be a single positive number.", call. = FALSE)
  }
  q <- customers$cadence_qualified
  ratio <- rep(NA_real_, nrow(customers))
  ratio[q] <- customers$recency[q] / customers$typical_interval[q]

  out <- customers
  out$inactivity_ratio <- ratio
  out$personalized_flag <- ifelse(q, ratio >= ratio_threshold, NA)
  out$fixed_flag <- customers$recency >= fixed_days
  out$rule_applied <- ifelse(q, RP_METHOD_PERSONALIZED, RP_METHOD_FALLBACK)
  out$hybrid_flag <- ifelse(q, out$personalized_flag, out$fixed_flag)
  out$alert_category <- ifelse(!out$hybrid_flag, RP_ALERT_NONE,
    ifelse(q, RP_ALERT_PERSONALIZED, RP_ALERT_FALLBACK)
  )
  out$explanation <- rp_explain_lapse(out, ratio_threshold, fixed_days)
  out
}

# One plain-language sentence per customer, built from the values that
# produced the decision.
rp_explain_lapse <- function(x, ratio_threshold, fixed_days) {
  days <- function(n) paste0(rp_fmt_int(n), ifelse(n == 1, " day", " days"))
  thr <- formatC(ratio_threshold, format = "f", digits = 2)
  ratio <- formatC(x$inactivity_ratio, format = "f", digits = 2)
  interval <- rp_fmt_num(x$typical_interval, 1)
  history <- tolower(x$cadence_status)

  dplyr::case_when(
    x$cadence_qualified & x$hybrid_flag ~ paste0(
      "Potentially overdue: inactive for ", days(x$recency),
      " compared with a median purchase interval of ", interval,
      " days. Inactivity ratio ", ratio, " (threshold ", thr, ")."
    ),
    x$cadence_qualified ~ paste0(
      "Within usual cadence: inactive for ", days(x$recency),
      "; median purchase interval ", interval, " days. Inactivity ratio ",
      ratio, " is below the threshold of ", thr, "."
    ),
    x$hybrid_flag ~ paste0(
      "Fixed-threshold signal: inactive for ", days(x$recency),
      " (threshold ", fixed_days, " days). Insufficient purchase history for ",
      "personalized cadence analysis (", history, "); the ", fixed_days,
      "-day fallback was applied."
    ),
    TRUE ~ paste0(
      "Not flagged: inactive for ", days(x$recency), ", below the ",
      fixed_days, "-day fallback threshold. Insufficient history for ",
      "personalized cadence analysis (", history, ")."
    )
  )
}
