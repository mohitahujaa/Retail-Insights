# Descriptive summaries used by the dashboard ----------------------------------

# Monthly purchasing activity from the final analytical dataset. A month is
# marked partial when the data starts after its first day or ends before its
# last day.
rp_monthly_trend <- function(final, first_timestamp, last_timestamp) {
  first_day <- as.Date(first_timestamp, tz = "UTC")
  last_day <- as.Date(last_timestamp, tz = "UTC")
  out <- final |>
    dplyr::mutate(month = lubridate::floor_date(date, "month")) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      purchase_value = sum(line_value),
      invoices = dplyr::n_distinct(InvoiceNo),
      customers = dplyr::n_distinct(CustomerID),
      lines = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(month) |>
    as.data.frame()
  month_end <- lubridate::ceiling_date(out$month, "month") - 1
  out$partial <- out$month < first_day | month_end > last_day
  out$days_covered <- as.integer(pmin(month_end, last_day) - pmax(out$month, first_day)) + 1L
  out$label <- format(out$month, "%b %Y")
  out$label[out$partial] <- paste0(
    out$label[out$partial], " (",
    as.integer(format(pmax(out$month, first_day)[out$partial], "%d")), "\u2013",
    as.integer(format(pmin(month_end, last_day)[out$partial], "%d")), " ",
    format(pmin(month_end, last_day)[out$partial], "%b"), ", partial)"
  )
  out
}

# Customers and value per segment, combined with the lapse outcome.
rp_segment_lapse_summary <- function(lapse) {
  lapse |>
    dplyr::group_by(segment) |>
    dplyr::summarise(
      customers = dplyr::n(),
      qualified = sum(cadence_qualified),
      flagged = sum(hybrid_flag),
      flagged_value = sum(monetary[hybrid_flag]),
      .groups = "drop"
    ) |>
    dplyr::mutate(share_flagged = flagged / customers) |>
    as.data.frame()
}
