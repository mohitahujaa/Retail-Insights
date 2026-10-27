fixture_clean <- function() rp_clean_transactions(rp_standardize(rp_fixture_raw()))

status_of <- function(cl, inv, stock) {
  tx <- cl$transactions
  as.character(tx$status[tx$InvoiceNo == inv & tx$StockCode == stock])
}

test_that("every row gets exactly one status and the funnel adds up", {
  cl <- fixture_clean()
  expect_equal(sum(cl$funnel$rows), 22)
  expect_equal(
    unname(stats::setNames(cl$funnel$rows, cl$funnel$status)[c(
      "Cancellation record (C invoice)", "Adjustment entry (A invoice)",
      "Missing CustomerID", "Non-positive unit price",
      "Purchase line later cancelled (matched)",
      "Non-product service or charge line", "Eligible"
    )]),
    c(5, 1, 2, 1, 3, 1, 9)
  )
  expect_equal(nrow(cl$final), 9)
})

test_that("missing-ID, cancellation, non-positive and non-product rows are excluded", {
  cl <- fixture_clean()
  expect_false(any(is.na(cl$final$CustomerID)))
  expect_false(any(startsWith(cl$final$InvoiceNo, "C")))
  expect_true(all(cl$final$Quantity > 0 & cl$final$UnitPrice > 0))
  expect_false("POST" %in% cl$final$StockCode)
  expect_equal(status_of(cl, "50003", "POST"), "Non-product service or charge line")
  expect_equal(status_of(cl, "50006", "P3"), "Non-positive unit price")
})

test_that("non-product matching is exact, not substring-based", {
  raw <- rp_fixture_raw()
  raw$StockCode[1] <- "POSTCARD"
  raw$StockCode[2] <- " post "
  cl <- rp_clean_transactions(rp_standardize(raw))
  expect_equal(as.character(cl$transactions$status[1]), "Eligible")
  expect_equal(as.character(cl$transactions$status[2]), "Non-product service or charge line")
})

test_that("a matched cancellation removes only its own purchase line", {
  cl <- fixture_clean()
  expect_equal(status_of(cl, "50007", "X1"), "Purchase line later cancelled (matched)")
  expect_equal(status_of(cl, "50007", "P1"), "Eligible")
})

test_that("ambiguous matches go to the most recent unmatched purchase, once each", {
  cl <- fixture_clean()
  a <- cl$cancellation_audit
  get <- function(inv) a[a$cancel_invoice == inv, ]
  expect_equal(get("C60002")$matched_invoice, "50009")
  expect_equal(get("C60002")$ambiguity, "Multiple candidates; most recent selected")
  expect_equal(get("C60003")$matched_invoice, "50008")
  expect_equal(get("C60003")$ambiguity, "Single candidate")
  expect_equal(get("C60004")$status, "Unmatched: no earlier line with same quantity and price")
  matched <- a$matched_row_id[a$status == "Matched"]
  expect_false(any(duplicated(matched)))
})

test_that("a cancellation cannot match a later purchase", {
  cl <- fixture_clean()
  a <- cl$cancellation_audit
  expect_equal(a$status[a$cancel_invoice == "C60005"], "Unmatched: no earlier purchase of this item by this customer")
  expect_equal(status_of(cl, "50010", "Z1"), "Eligible")
  m <- a[a$status == "Matched", ]
  expect_true(all(m$matched_time < m$cancel_time))
})

test_that("unmatched cancellations are retained in the audit", {
  cl <- fixture_clean()
  expect_equal(nrow(cl$cancellation_audit), 5)
  expect_equal(cl$summary$cancellations_matched, 3)
  expect_equal(cl$summary$cancellations_unmatched, 2)
})

test_that("matching respects the unit-price tolerance", {
  raw <- rp_fixture_raw()
  raw$UnitPrice[raw$InvoiceNo == "C60001"] <- 2.5
  cl <- rp_clean_transactions(rp_standardize(raw))
  expect_equal(status_of(cl, "50007", "X1"), "Eligible")
})

test_that("matching is deterministic when timestamps tie", {
  raw <- rp_fixture_raw()
  raw$InvoiceDate[raw$InvoiceNo == "50008"] <- raw$InvoiceDate[raw$InvoiceNo == "50009"]
  cl1 <- rp_clean_transactions(rp_standardize(raw))
  cl2 <- rp_clean_transactions(rp_standardize(raw))
  expect_identical(cl1$cancellation_audit, cl2$cancellation_audit)
  first <- cl1$cancellation_audit[cl1$cancellation_audit$cancel_invoice == "C60002", ]
  expect_equal(first$matched_invoice, "50009") # later row wins the tie
  expect_equal(first$ambiguity, "Multiple candidates; equal timestamps resolved by row order")
})

test_that("summary reports baseline and final figures", {
  s <- fixture_clean()$summary
  expect_equal(s$preliminary_rows, 13)
  expect_equal(s$matched_rows_removed, 3)
  expect_equal(s$matched_value_removed, 10 + 3 + 3)
  expect_equal(s$non_product_rows_removed, 1)
  expect_equal(s$final_customers, 5)
  expect_equal(s$customers_lost, "10005")
})
