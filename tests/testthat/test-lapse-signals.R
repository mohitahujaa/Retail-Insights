lapse_input <- function() {
  data.frame(
    CustomerID = c("a", "b", "c", "d", "e"),
    recency = c(20, 19, 60, 59, 5),
    frequency = c(5, 5, 1, 2, 4),
    monetary = c(100, 200, 50, 25, 400),
    n_purchase_dates = c(5, 5, 1, 2, 4),
    cadence_qualified = c(TRUE, TRUE, FALSE, FALSE, TRUE),
    typical_interval = c(10, 10, NA, NA, 1),
    cadence_status = c("Sufficient history", "Sufficient history", "Fewer than 3 invoices", "Fewer than 3 invoices", "Sufficient history"),
    stringsAsFactors = FALSE
  )
}

test_that("qualified customers use cadence, others use the fallback", {
  x <- rp_apply_lapse_rules(lapse_input(), 2, 60)
  expect_equal(x$rule_applied, c(rep(RP_METHOD_PERSONALIZED, 2), rep(RP_METHOD_FALLBACK, 2), RP_METHOD_PERSONALIZED))
  expect_true(all(is.na(x$inactivity_ratio[3:4])))
  expect_true(all(is.na(x$personalized_flag[3:4])))
})

test_that("threshold boundaries are inclusive", {
  x <- rp_apply_lapse_rules(lapse_input(), 2, 60)
  expect_equal(x$inactivity_ratio[1:2], c(2.0, 1.9))
  expect_equal(x$personalized_flag[1:2], c(TRUE, FALSE)) # ratio >= 2
  expect_equal(x$fixed_flag[3:4], c(TRUE, FALSE)) # days >= 60
  expect_equal(x$hybrid_flag, c(TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(x$alert_category, c(RP_ALERT_PERSONALIZED, RP_ALERT_NONE, RP_ALERT_FALLBACK, RP_ALERT_NONE, RP_ALERT_PERSONALIZED))
})

test_that("no ratio is fabricated without a valid interval", {
  d <- lapse_input()
  d$typical_interval[3] <- 0 # would divide by zero if used
  x <- rp_apply_lapse_rules(d, 2, 60)
  expect_true(is.na(x$inactivity_ratio[3]))
  expect_false(any(is.infinite(x$inactivity_ratio)))
})

test_that("invalid thresholds are rejected", {
  expect_error(rp_apply_lapse_rules(lapse_input(), 0, 60), "ratio_threshold")
  expect_error(rp_apply_lapse_rules(lapse_input(), 2, -5), "fixed_days")
})

test_that("explanations state the values behind each decision", {
  x <- rp_apply_lapse_rules(lapse_input(), 2, 60)
  expect_equal(
    x$explanation[1],
    "Potentially overdue: inactive for 20 days compared with a median purchase interval of 10.0 days. Inactivity ratio 2.00 (threshold 2.00)."
  )
  expect_match(x$explanation[3], "^Fixed-threshold signal: inactive for 60 days \\(threshold 60 days\\).*fewer than 3 invoices.*60-day fallback was applied")
  expect_match(x$explanation[2], "^Within usual cadence")
  expect_match(x$explanation[4], "^Not flagged")
  expect_false(any(grepl("churn", x$explanation, ignore.case = TRUE)))
})

test_that("evaluation A uses only qualified customers, with consistent counts", {
  x <- rp_apply_lapse_rules(lapse_input(), 2, 60)
  a <- rp_evaluate_rules_same_population(x)
  t <- stats::setNames(a$table$customers, a$table$group)
  expect_equal(a$population, 3)
  expect_equal(unname(t["Flagged by personalized rule"]), 2)
  expect_equal(unname(t["Flagged by fixed rule"]), 0)
  expect_equal(
    unname(t["Flagged by both"] + t["Flagged only by personalized rule"] +
      t["Flagged only by fixed rule"] + t["Flagged by neither"]), 3
  )
  expect_equal(a$table$historical_value[a$table$group == "Flagged by personalized rule"], 500)
  expect_equal(a$jaccard, 0)
})

test_that("evaluation B covers all customers and splits flags by rule", {
  x <- rp_apply_lapse_rules(lapse_input(), 2, 60)
  b <- rp_evaluate_hybrid_policy(x)
  t <- stats::setNames(b$customers, b$group)
  expect_equal(unname(t["All customers"]), 5)
  expect_equal(unname(t["Evaluated by personalized cadence"] + t["Evaluated by fixed fallback"]), 5)
  expect_equal(unname(t["Flagged: personalized cadence signal"]), 2)
  expect_equal(unname(t["Flagged: fallback threshold signal"]), 1)
  expect_equal(unname(t["Flagged in total (hybrid policy)"]), 3)
})

test_that("both rules use the same reference date in the pipeline", {
  res <- rp_run_pipeline_df(rp_synthetic_raw(), rp_test_config())
  cust <- res$customers
  expect_equal(cust$recency, as.integer(res$reference_date - cust$last_purchase))
  x <- rp_apply_lapse_rules(cust, 2, 60)
  q <- x$cadence_qualified
  expect_equal(x$inactivity_ratio[q], cust$recency[q] / cust$typical_interval[q])
  expect_equal(x$fixed_flag, cust$recency >= 60)
})
