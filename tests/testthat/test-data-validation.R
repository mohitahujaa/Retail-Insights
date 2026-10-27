test_that("missing required columns are reported by name", {
  raw <- rp_fixture_raw()
  raw$UnitPrice <- NULL
  expect_error(rp_standardize(raw), "Missing required column\\(s\\): UnitPrice")
})

test_that("a missing workbook produces an informative error", {
  expect_error(
    rp_read_transactions(file.path(tempdir(), "does_not_exist.xlsx")),
    "not found.*data/raw/online_retail.xlsx"
  )
})

test_that("identifiers stay text and customer IDs are normalised", {
  raw <- rp_fixture_raw()
  raw$CustomerID[1] <- "10001.0"
  tx <- rp_standardize(raw)
  expect_type(tx$InvoiceNo, "character")
  expect_type(tx$StockCode, "character")
  expect_true("C60001" %in% tx$InvoiceNo)
  expect_equal(tx$CustomerID[1], "10001")
  expect_equal(tx$stock_norm, toupper(tx$StockCode))
})

test_that("dates are parsed from date-time values and from text", {
  tx <- rp_standardize(rp_fixture_raw())
  expect_s3_class(tx$InvoiceDate, "POSIXct")
  expect_equal(tx$date[1], as.Date("2011-01-01"))

  raw <- rp_fixture_raw()
  raw$InvoiceDate <- format(raw$InvoiceDate, "%Y-%m-%d %H:%M:%S")
  tx2 <- rp_standardize(raw)
  expect_equal(tx2$InvoiceDate, tx$InvoiceDate)
  expect_false(any(tx2$parse_error))
})

test_that("numeric conversion errors are detected, not silently accepted", {
  raw <- rp_fixture_raw()
  raw$Quantity <- as.character(raw$Quantity)
  raw$Quantity[2] <- "two"
  raw$InvoiceDate <- format(raw$InvoiceDate, "%Y-%m-%d %H:%M:%S")
  raw$InvoiceDate[3] <- "not a date"
  tx <- rp_standardize(raw)
  expect_equal(which(tx$parse_error), c(2L, 3L))
  expect_true(all(tx$invalid_value[2:3]))
  expect_equal(rp_audit_raw(tx)$n_parse_errors, 2)

  cleaned <- rp_clean_transactions(tx)
  expect_equal(as.character(cleaned$transactions$status[2:3]), rep("Invalid or unparseable value", 2))
})

test_that("reference date is the day after the latest raw timestamp", {
  tx <- rp_standardize(rp_fixture_raw())
  # The latest row (2011-01-30) has no CustomerID and is excluded later, but it
  # still defines the reference date.
  expect_equal(rp_reference_date(tx), as.Date("2011-01-31"))
  expect_equal(rp_audit_raw(tx)$reference_date, as.Date("2011-01-31"))
})

test_that("raw audit counts overlapping conditions", {
  a <- rp_audit_raw(rp_standardize(rp_fixture_raw()))
  expect_equal(a$n_rows, 22)
  expect_equal(a$n_missing_customer, 3)
  expect_equal(a$n_cancellation_rows, 5)
  expect_equal(a$n_adjustment_rows, 1)
  expect_equal(a$n_negative_quantity, 6)
  expect_equal(a$n_negative_quantity_not_cancellation, 1)
  expect_equal(a$n_nonpositive_price, 3)
})
