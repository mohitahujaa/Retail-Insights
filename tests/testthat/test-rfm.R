fixture_customers <- function() {
  tx <- rp_standardize(rp_fixture_raw())
  cl <- rp_clean_transactions(tx)
  cust <- rp_customer_features(cl$final, rp_reference_date(tx))
  rp_add_cadence(cust, rp_purchase_intervals(cl$final))
}

test_that("RFM values match hand calculations", {
  c <- fixture_customers()
  a <- c[c$CustomerID == "10001", ]
  expect_equal(a$recency, 10) # 2011-01-31 - 2011-01-21
  expect_equal(a$frequency, 3) # distinct invoices, not lines
  expect_equal(a$monetary, 2 * 3 + 1 * 5 + 2 * 3 + 4 * 3) # POST excluded
  expect_equal(a$n_lines, 4)

  expect_equal(c$monetary[c$CustomerID == "10003"], 30) # zero-price line excluded
  expect_equal(c$monetary[c$CustomerID == "10004"], 3) # cancelled X1 line excluded
  expect_equal(c$recency[c$CustomerID == "10002"], 6)
  expect_equal(c$recency[c$CustomerID == "10006"], 26)
})

test_that("one row per customer with no synthetic customer for missing IDs", {
  c <- fixture_customers()
  expect_equal(sort(c$CustomerID), c("10001", "10002", "10003", "10004", "10006"))
  expect_false(any(is.na(c$CustomerID)))
})

test_that("reference date must follow all purchases", {
  cl <- rp_clean_transactions(rp_standardize(rp_fixture_raw()))
  expect_error(rp_customer_features(cl$final, as.Date("2011-01-20")), "Reference date")
})

test_that("cadence uses distinct purchase dates and positive gaps", {
  c <- fixture_customers()
  a <- c[c$CustomerID == "10001", ]
  expect_true(a$cadence_qualified)
  expect_equal(a$n_intervals, 2)
  expect_equal(a$typical_interval, 10)

  # Two invoices on the same day count as one purchase date.
  raw <- rp_fixture_raw()
  raw$InvoiceDate[raw$InvoiceNo == "50002"] <- as.POSIXct("2011-01-01 15:00", tz = "UTC")
  cl <- rp_clean_transactions(rp_standardize(raw))
  iv <- rp_purchase_intervals(cl$final)
  expect_equal(iv$gap_days[iv$CustomerID == "10001"], 20)
  cust <- rp_add_cadence(rp_customer_features(cl$final, as.Date("2011-01-31")), iv)
  x <- cust[cust$CustomerID == "10001", ]
  expect_false(x$cadence_qualified)
  expect_true(is.na(x$typical_interval))
  expect_match(x$cadence_status, "distinct purchase dates")
})

test_that("descriptive statistics and skewness are computed", {
  d <- rp_describe(data.frame(recency = 1:10, frequency = c(rep(1, 9), 50), monetary = 1:10))
  expect_equal(d$median[d$feature == "recency"], 5.5)
  expect_gt(d$skewness[d$feature == "frequency"], 2)
  expect_equal(d$skewness[d$feature == "recency"], 0)
})
