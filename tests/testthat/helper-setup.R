# Locate the project root and load the project code for the tests.
rp_find_root <- function(start = getwd()) {
  dir <- normalizePath(start)
  for (i in 1:5) {
    if (file.exists(file.path(dir, "app.R"))) return(dir)
    dir <- dirname(dir)
  }
  stop("Could not find the project root (folder containing app.R).")
}

RP_ROOT <- rp_find_root()
source(file.path(RP_ROOT, "R", "pipeline.R"))
rp_source_all(RP_ROOT, modules = TRUE)

# A small, hand-verifiable transaction table. Expected outcomes are written next
# to the rows and checked in the tests.
rp_fixture_raw <- function() {
  r <- function(inv, stock, qty, time, price, cust) {
    data.frame(
      InvoiceNo = inv, StockCode = stock, Description = paste("ITEM", stock),
      Quantity = qty, InvoiceDate = as.POSIXct(time, tz = "UTC"),
      UnitPrice = price, CustomerID = cust, Country = "United Kingdom",
      stringsAsFactors = FALSE
    )
  }
  rbind(
    # Customer 10001: 3 invoices on 3 dates, gaps 10 and 10 days; POST excluded
    r("50001", "P1", 2, "2011-01-01 09:00", 3, "10001"),
    r("50001", "P2", 1, "2011-01-01 09:00", 5, "10001"),
    r("50002", "P1", 2, "2011-01-11 09:00", 3, "10001"),
    r("50003", "P1", 4, "2011-01-21 09:00", 3, "10001"),
    r("50003", "POST", 1, "2011-01-21 09:00", 18, "10001"),
    # Customer 10002: 2 invoices -> fallback rule
    r("50004", "P1", 1, "2011-01-05 10:00", 3, "10002"),
    r("50005", "P2", 1, "2011-01-25 10:00", 3, "10002"),
    # Customer 10003: one invoice; the zero-price line is excluded
    r("50006", "P1", 10, "2011-01-15 12:00", 3, "10003"),
    r("50006", "P3", 1, "2011-01-15 12:00", 0, "10003"),
    # Customer 10004: X1 line later cancelled (matched); P1 line remains
    r("50007", "X1", 5, "2011-01-10 10:00", 2, "10004"),
    r("50007", "P1", 1, "2011-01-10 10:00", 3, "10004"),
    r("C60001", "X1", -5, "2011-01-12 10:00", 2, "10004"),
    # Customer 10005: two identical purchases, three cancellations
    r("50008", "Y1", 3, "2011-01-02 10:00", 1, "10005"),
    r("50009", "Y1", 3, "2011-01-08 10:00", 1, "10005"),
    r("C60002", "Y1", -3, "2011-01-09 10:00", 1, "10005"), # -> 50009 (most recent)
    r("C60003", "Y1", -3, "2011-01-10 10:00", 1, "10005"), # -> 50008
    r("C60004", "Y1", -3, "2011-01-11 10:00", 1, "10005"), # unmatched
    # Customer 10006: cancellation precedes the purchase -> must not match
    r("C60005", "Z1", -2, "2011-01-03 10:00", 4, "10006"),
    r("50010", "Z1", 2, "2011-01-05 10:00", 4, "10006"),
    # Rows without a customer; the latest timestamp in the file is here
    r("50011", "P1", 1, "2011-01-30 15:00", 3, NA),
    r("A70001", "B", 1, "2011-01-20 10:00", -100, NA),
    r("50012", "P1", -4, "2011-01-20 11:00", 0, NA)
  )
}

# A larger synthetic dataset for integration and module tests.
rp_synthetic_raw <- function(n_customers = 150, seed = 1) {
  set.seed(seed)
  start <- as.POSIXct("2011-01-01 09:00", tz = "UTC")
  rows <- list()
  inv <- 100000
  for (i in seq_len(n_customers)) {
    n_inv <- sample(c(1, 1, 2, 3, 4, 6, 10, 15), 1)
    days <- sort(sample(0:330, n_inv))
    for (d in days) {
      inv <- inv + 1
      n_lines <- sample(1:4, 1)
      rows[[length(rows) + 1]] <- data.frame(
        InvoiceNo = as.character(inv),
        StockCode = sample(c("P1", "P2", "P3", "P4"), n_lines, replace = TRUE),
        Description = "ITEM",
        Quantity = sample(1:24, n_lines, replace = TRUE),
        InvoiceDate = start + d * 86400,
        UnitPrice = round(stats::runif(n_lines, 0.5, 20) * (1 + (i %% 5 == 0) * 5), 2),
        CustomerID = as.character(20000 + i),
        Country = "United Kingdom",
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

rp_test_config <- function() rp_config(k_range = 2:4, min_cluster_size = 5, nstart = 10)
