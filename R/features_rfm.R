# Customer-level RFM features and purchase-cadence inputs ---------------------

# Aggregate final analytical transactions to one row per customer.
#
# recency   = reference_date - last purchase date (calendar days)
# frequency = number of distinct purchase invoices
# monetary  = sum(Quantity * UnitPrice) over eligible lines
#             (eligible historical purchase value, not profit or net revenue)
rp_customer_features <- function(final, reference_date) {
  if (nrow(final) == 0) stop("No eligible transactions to aggregate.", call. = FALSE)
  if (any(is.na(final$CustomerID))) {
    stop("Customer features require a CustomerID on every row.", call. = FALSE)
  }
  reference_date <- as.Date(reference_date)
  if (any(final$date >= reference_date)) {
    stop("Reference date must be after every purchase date.", call. = FALSE)
  }
  value <- if ("line_value" %in% names(final)) final$line_value else final$Quantity * final$UnitPrice

  customers <- final |>
    dplyr::mutate(line_value = value) |>
    dplyr::group_by(CustomerID) |>
    dplyr::summarise(
      first_purchase = min(date),
      last_purchase = max(date),
      frequency = dplyr::n_distinct(InvoiceNo),
      n_purchase_dates = dplyr::n_distinct(date),
      n_lines = dplyr::n(),
      monetary = sum(line_value),
      n_countries = dplyr::n_distinct(Country),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      recency = as.integer(reference_date - last_purchase),
      tenure_days = as.integer(reference_date - first_purchase),
      avg_invoice_value = monetary / frequency
    )

  # Country: most frequent country among the customer's eligible lines (ties
  # broken alphabetically). n_countries > 1 is kept for transparency.
  country <- final |>
    dplyr::count(CustomerID, Country) |>
    dplyr::arrange(CustomerID, dplyr::desc(n), Country) |>
    dplyr::distinct(CustomerID, .keep_all = TRUE) |>
    dplyr::select(CustomerID, country = Country)

  customers |>
    dplyr::left_join(country, by = "CustomerID") |>
    dplyr::relocate(CustomerID, recency, frequency, monetary) |>
    as.data.frame()
}

# Calendar-day gaps between successive distinct purchase dates, per customer.
# Because dates are de-duplicated first, every gap is a positive integer.
rp_purchase_intervals <- function(final) {
  final |>
    dplyr::distinct(CustomerID, date) |>
    dplyr::arrange(CustomerID, date) |>
    dplyr::group_by(CustomerID) |>
    dplyr::mutate(gap_days = as.integer(date - dplyr::lag(date))) |>
    dplyr::filter(!is.na(gap_days), gap_days > 0) |>
    dplyr::ungroup() |>
    as.data.frame()
}

# Add cadence fields: number of positive intervals, their median (the typical
# purchase interval) and whether the customer has enough history for the
# personalized rule. Customers without enough history get NA, never an
# imputed interval.
rp_add_cadence <- function(customers, intervals, cfg = rp_config()) {
  cadence <- intervals |>
    dplyr::group_by(CustomerID) |>
    dplyr::summarise(
      n_intervals = dplyr::n(),
      median_interval = stats::median(gap_days),
      .groups = "drop"
    )
  out <- dplyr::left_join(customers, cadence, by = "CustomerID")
  out$n_intervals[is.na(out$n_intervals)] <- 0L
  out$cadence_qualified <- out$frequency >= cfg$min_invoices &
    out$n_purchase_dates >= cfg$min_purchase_dates &
    out$n_intervals >= cfg$min_intervals &
    !is.na(out$median_interval) & out$median_interval > 0
  out$typical_interval <- ifelse(out$cadence_qualified, out$median_interval, NA_real_)
  out$cadence_status <- dplyr::case_when(
    out$cadence_qualified ~ "Sufficient history",
    out$frequency < cfg$min_invoices ~ paste0("Fewer than ", cfg$min_invoices, " invoices"),
    out$n_purchase_dates < cfg$min_purchase_dates ~ paste0("Fewer than ", cfg$min_purchase_dates, " distinct purchase dates"),
    TRUE ~ "Insufficient valid intervals"
  )
  out$median_interval <- NULL
  out
}

# Skewness (moment-based, population form). Positive values indicate a long
# right tail.
rp_skewness <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3 || stats::sd(x) == 0) return(NA_real_)
  m <- mean(x)
  mean((x - m)^3) / mean((x - m)^2)^1.5
}

# Descriptive statistics for numeric customer features.
rp_describe <- function(customers, vars = c("recency", "frequency", "monetary")) {
  do.call(rbind, lapply(vars, function(v) {
    x <- customers[[v]]
    q <- stats::quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95), names = FALSE)
    data.frame(
      feature = v, n = length(x), mean = mean(x), sd = stats::sd(x),
      min = min(x), p05 = q[1], p25 = q[2], median = q[3], p75 = q[4],
      p95 = q[5], max = max(x), skewness = rp_skewness(x),
      skewness_log1p = rp_skewness(log1p(x)),
      stringsAsFactors = FALSE
    )
  }))
}

# Spearman rank correlations between RFM features (robust to skew).
rp_rfm_correlations <- function(customers, vars = c("recency", "frequency", "monetary")) {
  stats::cor(customers[, vars], method = "spearman")
}
