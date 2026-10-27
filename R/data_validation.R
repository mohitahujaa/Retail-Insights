# Schema validation, type normalisation and the raw-data audit --------------

# Stop with an understandable message if required columns are absent.
rp_validate_schema <- function(df, required = rp_config()$required_columns) {
  if (!is.data.frame(df)) {
    stop("Expected a data frame of transactions.", call. = FALSE)
  }
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      "Missing required column(s): ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Convert to numeric, counting values that were present but not numeric.
rp_parse_numeric <- function(x) {
  if (is.numeric(x)) {
    return(list(value = as.numeric(x), failed = rep(FALSE, length(x))))
  }
  chr <- trimws(as.character(x))
  value <- suppressWarnings(as.numeric(chr))
  list(value = value, failed = !is.na(chr) & chr != "" & is.na(value))
}

# Convert to POSIXct (UTC), counting values that were present but unparseable.
rp_parse_datetime <- function(x) {
  if (inherits(x, "POSIXct")) {
    value <- lubridate::with_tz(x, "UTC")
    return(list(value = value, failed = rep(FALSE, length(x))))
  }
  if (inherits(x, "Date")) {
    value <- as.POSIXct(x, tz = "UTC")
    return(list(value = value, failed = rep(FALSE, length(x))))
  }
  chr <- trimws(as.character(x))
  value <- suppressWarnings(lubridate::parse_date_time(
    chr,
    orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmY HM", "dmY"), tz = "UTC"
  ))
  list(value = value, failed = !is.na(chr) & chr != "" & is.na(value))
}

# Normalise an identifier: trimmed text, empty strings as NA. Customer IDs read
# from numeric cells may carry a trailing ".0", which is removed.
rp_normalize_id <- function(x, strip_decimal = FALSE) {
  out <- trimws(as.character(x))
  if (strip_decimal) out <- sub("\\.0+$", "", out)
  out[!is.na(out) & out == ""] <- NA_character_
  out
}

# Validate and normalise the raw transactions.
#
# The original identifier columns are kept; normalised copies used for matching
# are added (`invoice_norm`, `stock_norm`). Values that are present but cannot
# be parsed are flagged in `invalid_value` (and `parse_error`) so the cleaning
# step can exclude them explicitly rather than coercing them silently.
rp_standardize <- function(raw, cfg = rp_config()) {
  rp_validate_schema(raw, cfg$required_columns)
  tx <- as.data.frame(raw, stringsAsFactors = FALSE)
  if (!"row_id" %in% names(tx)) tx$row_id <- seq_len(nrow(tx))

  tx$InvoiceNo <- rp_normalize_id(tx$InvoiceNo)
  tx$StockCode <- rp_normalize_id(tx$StockCode)
  tx$CustomerID <- rp_normalize_id(tx$CustomerID, strip_decimal = TRUE)
  tx$Description <- as.character(tx$Description)
  tx$Country <- rp_normalize_id(tx$Country)
  tx$invoice_norm <- toupper(tx$InvoiceNo)
  tx$stock_norm <- toupper(tx$StockCode)

  qty <- rp_parse_numeric(tx$Quantity)
  price <- rp_parse_numeric(tx$UnitPrice)
  when <- rp_parse_datetime(tx$InvoiceDate)
  tx$Quantity <- qty$value
  tx$UnitPrice <- price$value
  tx$InvoiceDate <- when$value
  tx$date <- as.Date(tx$InvoiceDate, tz = "UTC")

  tx$parse_error <- qty$failed | price$failed | when$failed
  tx$invalid_value <- is.na(tx$InvoiceNo) | is.na(tx$Quantity) |
    is.na(tx$UnitPrice) | is.na(tx$InvoiceDate)
  attr(tx, "read_warnings") <- attr(raw, "read_warnings")
  tx
}

# Analysis reference date: the calendar day after the latest valid timestamp in
# the input, computed before any analytical exclusion.
rp_reference_date <- function(tx) {
  valid <- tx$InvoiceDate[!is.na(tx$InvoiceDate)]
  if (length(valid) == 0) stop("No valid InvoiceDate values.", call. = FALSE)
  as.Date(max(valid), tz = "UTC") + 1
}

# Counts describing the raw input. Indicators overlap (a row can be both a
# cancellation and missing a customer ID); they are diagnostics, not an
# exclusion funnel. The funnel is produced by rp_clean_transactions().
rp_audit_raw <- function(tx) {
  is_c <- rp_has_prefix(tx$invoice_norm, "C")
  is_a <- rp_has_prefix(tx$invoice_norm, "A")
  list(
    n_rows = nrow(tx),
    n_read_warnings = length(attr(tx, "read_warnings")),
    n_parse_errors = sum(tx$parse_error),
    n_invalid_values = sum(tx$invalid_value),
    n_missing_customer = sum(is.na(tx$CustomerID)),
    n_missing_description = sum(is.na(tx$Description) |
      trimws(tx$Description) == ""),
    n_cancellation_rows = sum(is_c),
    n_cancellation_invoices = length(unique(tx$invoice_norm[is_c])),
    n_adjustment_rows = sum(is_a),
    n_negative_quantity = sum(tx$Quantity < 0, na.rm = TRUE),
    n_nonpositive_quantity = sum(tx$Quantity <= 0, na.rm = TRUE),
    n_negative_quantity_not_cancellation = sum(tx$Quantity < 0 & !is_c,
      na.rm = TRUE
    ),
    n_nonpositive_price = sum(tx$UnitPrice <= 0, na.rm = TRUE),
    n_negative_price = sum(tx$UnitPrice < 0, na.rm = TRUE),
    n_invoices = length(unique(stats::na.omit(tx$invoice_norm))),
    n_customers = length(unique(stats::na.omit(tx$CustomerID))),
    n_countries = length(unique(stats::na.omit(tx$Country))),
    first_timestamp = min(tx$InvoiceDate, na.rm = TRUE),
    last_timestamp = max(tx$InvoiceDate, na.rm = TRUE),
    reference_date = rp_reference_date(tx)
  )
}

# TRUE where x starts with prefix; FALSE (never NA) for missing values.
rp_has_prefix <- function(x, prefix) !is.na(x) & startsWith(x, prefix)
