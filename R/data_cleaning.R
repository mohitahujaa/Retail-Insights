# Transaction classification, cancellation matching and the cleaning funnel --

# Exclusion reasons, in priority order. Every row receives exactly one status:
# the first reason that applies. Overlapping conditions are reported separately
# by rp_audit_raw().
RP_STATUS_LEVELS <- c(
  "Invalid or unparseable value",
  "Cancellation record (C invoice)",
  "Adjustment entry (A invoice)",
  "Missing CustomerID",
  "Non-positive quantity",
  "Non-positive unit price",
  "Purchase line later cancelled (matched)",
  "Non-product service or charge line",
  "Eligible"
)

# Flag each row with the conditions used by the cleaning policy.
rp_classify_rows <- function(tx, cfg = rp_config()) {
  tx$is_cancellation <- rp_has_prefix(tx$invoice_norm, "C")
  tx$is_adjustment <- rp_has_prefix(tx$invoice_norm, "A")
  tx$missing_customer <- is.na(tx$CustomerID)
  tx$is_non_product <- !is.na(tx$stock_norm) &
    tx$stock_norm %in% toupper(cfg$non_product_codes)
  tx$preliminary_eligible <- !tx$invalid_value & !tx$is_cancellation &
    !tx$is_adjustment & !tx$missing_customer &
    tx$Quantity > 0 & tx$UnitPrice > 0
  tx$preliminary_eligible[is.na(tx$preliminary_eligible)] <- FALSE
  tx
}

# One-to-one, timestamp-aware matching of cancellation lines to the original
# purchase lines they reverse.
#
# A cancellation line can match a preliminary-eligible purchase line with the
# same CustomerID, normalised StockCode and absolute quantity, a UnitPrice
# within `price_tolerance`, and a timestamp strictly earlier than the
# cancellation. Cancellations are processed in (timestamp, row_id) order. When
# several unmatched purchase lines qualify, the most recent one is chosen; equal
# timestamps are resolved by the larger row_id (the later row in the file).
# Each purchase line can be matched at most once. Cancellations without a match
# are left unresolved: no purchase is removed on their behalf.
#
# Returns one audit row per cancellation line.
rp_match_cancellations <- function(tx, cfg = rp_config()) {
  canc <- tx[tx$is_cancellation, , drop = FALSE]
  canc <- canc[order(canc$InvoiceDate, canc$row_id), , drop = FALSE]
  cand <- tx[tx$preliminary_eligible, , drop = FALSE]
  cand <- cand[order(cand$InvoiceDate, cand$row_id), , drop = FALSE]

  n <- nrow(canc)
  audit <- data.frame(
    cancel_row_id = canc$row_id,
    cancel_invoice = canc$InvoiceNo,
    CustomerID = canc$CustomerID,
    StockCode = canc$StockCode,
    cancel_quantity = canc$Quantity,
    UnitPrice = canc$UnitPrice,
    cancel_time = canc$InvoiceDate,
    status = rep(NA_character_, n),
    ambiguity = rep(NA_character_, n),
    n_candidates = rep(0L, n),
    matched_row_id = rep(NA_integer_, n),
    matched_invoice = rep(NA_character_, n),
    matched_time = as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC"),
    matched_value = rep(NA_real_, n),
    stringsAsFactors = FALSE
  )
  if (n == 0) return(audit)

  rp_key <- function(cust, stock, qty) paste(cust, stock, format(qty, scientific = FALSE, trim = TRUE), sep = "\r")
  canc_key <- rp_key(canc$CustomerID, canc$stock_norm, abs(canc$Quantity))
  cand_key <- rp_key(cand$CustomerID, cand$stock_norm, cand$Quantity)

  matchable <- !canc$invalid_value & !is.na(canc$CustomerID) & canc$Quantity < 0
  audit$status[canc$invalid_value] <- "Unmatched: invalid value"
  audit$status[!canc$invalid_value & is.na(canc$CustomerID)] <- "Unmatched: missing CustomerID"
  audit$status[!canc$invalid_value & !is.na(canc$CustomerID) &
    canc$Quantity >= 0] <- "Unmatched: non-negative quantity"

  # Hashed environment: looking names up in a long list is linear-time.
  groups <- list2env(split(seq_len(nrow(cand)), cand_key), hash = TRUE)
  used <- new.env(hash = TRUE)
  tol <- cfg$price_tolerance
  # Work on plain vectors inside the loop; modifying a data frame row by row
  # is very slow in R.
  status <- audit$status
  ambiguity <- audit$ambiguity
  n_candidates <- audit$n_candidates
  matched_j <- rep(NA_integer_, n)
  cand_time <- as.numeric(cand$InvoiceDate)
  canc_time <- as.numeric(canc$InvoiceDate)

  for (i in which(matchable)) {
    idx <- groups[[canc_key[i]]]
    if (is.null(idx)) {
      status[i] <- "Unmatched: no earlier line with same quantity and price"
      next
    }
    taken <- used[[canc_key[i]]]
    if (is.null(taken)) taken <- rep(FALSE, length(idx))
    ok <- !taken & cand_time[idx] < canc_time[i] &
      abs(cand$UnitPrice[idx] - canc$UnitPrice[i]) <= tol
    n_candidates[i] <- sum(ok)
    if (!any(ok)) {
      status[i] <- "Unmatched: no earlier line with same quantity and price"
      next
    }
    # idx is sorted by (timestamp, row_id), so the last qualifying position is
    # the most recent purchase, with ties resolved by the larger row_id.
    pos <- max(which(ok))
    taken[pos] <- TRUE
    assign(canc_key[i], taken, envir = used)
    j <- idx[pos]
    tied <- sum(ok & cand_time[idx] == cand_time[j])
    status[i] <- "Matched"
    ambiguity[i] <- if (sum(ok) == 1) {
      "Single candidate"
    } else if (tied > 1) {
      "Multiple candidates; equal timestamps resolved by row order"
    } else {
      "Multiple candidates; most recent selected"
    }
    matched_j[i] <- j
  }
  audit$status <- status
  audit$ambiguity <- ambiguity
  audit$n_candidates <- n_candidates
  hit <- !is.na(matched_j)
  audit$matched_row_id[hit] <- cand$row_id[matched_j[hit]]
  audit$matched_invoice[hit] <- cand$InvoiceNo[matched_j[hit]]
  audit$matched_time[hit] <- cand$InvoiceDate[matched_j[hit]]
  audit$matched_value[hit] <- cand$Quantity[matched_j[hit]] * cand$UnitPrice[matched_j[hit]]

  # Diagnostic for unmatched cancellations: did the customer buy this stock
  # code at all before the cancellation? If not, the original purchase most
  # likely predates the dataset (before December 2010).
  unmatched <- audit$status == "Unmatched: no earlier line with same quantity and price"
  if (any(unmatched)) {
    first_buy <- tapply(
      as.numeric(cand$InvoiceDate),
      paste(cand$CustomerID, cand$stock_norm, sep = "\r"), min
    )
    ck <- paste(canc$CustomerID, canc$stock_norm, sep = "\r")[unmatched]
    prior <- !is.na(first_buy[ck]) &
      first_buy[ck] < as.numeric(canc$InvoiceDate[unmatched])
    audit$status[which(unmatched)[!prior]] <-
      "Unmatched: no earlier purchase of this item by this customer"
  }
  audit
}

# Apply the full cleaning policy.
#
# Returns a list with the row-level classification (`transactions`), the final
# analytical dataset (`final`), the cancellation audit (`cancellation_audit`),
# the exclusion funnel (`funnel`), the non-product breakdown (`non_product`) and
# headline counts (`summary`).
rp_clean_transactions <- function(tx, cfg = rp_config()) {
  tx <- rp_classify_rows(tx, cfg)
  audit <- rp_match_cancellations(tx, cfg)
  tx$matched_cancelled <- tx$row_id %in% audit$matched_row_id[audit$status == "Matched"]

  tx$line_value <- ifelse(tx$Quantity > 0 & tx$UnitPrice > 0,
    tx$Quantity * tx$UnitPrice, NA_real_
  )
  status <- rep("Eligible", nrow(tx))
  # Assigned from lowest to highest priority so that the first applicable
  # reason in RP_STATUS_LEVELS wins.
  status[which(tx$is_non_product & tx$preliminary_eligible)] <- RP_STATUS_LEVELS[8]
  status[which(tx$matched_cancelled)] <- RP_STATUS_LEVELS[7]
  status[which(tx$UnitPrice <= 0)] <- RP_STATUS_LEVELS[6]
  status[which(tx$Quantity <= 0)] <- RP_STATUS_LEVELS[5]
  status[which(tx$missing_customer)] <- RP_STATUS_LEVELS[4]
  status[which(tx$is_adjustment)] <- RP_STATUS_LEVELS[3]
  status[which(tx$is_cancellation)] <- RP_STATUS_LEVELS[2]
  status[which(tx$invalid_value)] <- RP_STATUS_LEVELS[1]
  tx$status <- factor(status, levels = RP_STATUS_LEVELS)

  final <- tx[tx$status == "Eligible", c(
    "row_id", "InvoiceNo", "StockCode", "Description", "Quantity",
    "UnitPrice", "InvoiceDate", "date", "CustomerID", "Country", "line_value"
  ), drop = FALSE]
  rownames(final) <- NULL

  prelim <- tx[tx$preliminary_eligible, , drop = FALSE]
  funnel <- data.frame(
    step = seq_along(RP_STATUS_LEVELS),
    status = RP_STATUS_LEVELS,
    rows = as.integer(table(tx$status)[RP_STATUS_LEVELS]),
    stringsAsFactors = FALSE
  )
  funnel$share_of_rows <- funnel$rows / nrow(tx)
  funnel$purchase_value <- vapply(RP_STATUS_LEVELS, function(s) {
    if (s %in% RP_STATUS_LEVELS[7:9]) sum(tx$line_value[tx$status == s]) else NA_real_
  }, numeric(1))

  np <- tx[tx$is_non_product, , drop = FALSE]
  non_product <- if (nrow(np) == 0) {
    data.frame(
      StockCode = character(), description = character(), all_rows = integer(),
      eligible_rows = integer(), excluded_rows = integer(),
      excluded_value = numeric(), matched_cancelled_rows = integer()
    )
  } else {
    do.call(rbind, lapply(split(np, np$stock_norm), function(d) {
      excl <- d$status == RP_STATUS_LEVELS[8]
      data.frame(
        StockCode = d$stock_norm[1],
        description = names(sort(table(d$Description), decreasing = TRUE))[1] %||% NA_character_,
        all_rows = nrow(d),
        eligible_rows = sum(d$preliminary_eligible),
        excluded_rows = sum(excl),
        excluded_value = sum(d$line_value[excl]),
        matched_cancelled_rows = sum(d$matched_cancelled),
        stringsAsFactors = FALSE
      )
    }))
  }
  non_product <- non_product[order(-non_product$excluded_value), , drop = FALSE]
  rownames(non_product) <- NULL

  prelim_freq <- tapply(prelim$invoice_norm, prelim$CustomerID, function(x) length(unique(x)))
  matched_mask <- audit$status == "Matched"
  summary <- list(
    preliminary_rows = nrow(prelim),
    preliminary_value = sum(prelim$line_value),
    preliminary_customers = length(unique(prelim$CustomerID)),
    preliminary_invoices = length(unique(prelim$invoice_norm)),
    preliminary_countries = length(unique(prelim$Country)),
    preliminary_single_invoice_customers = sum(prelim_freq == 1),
    cancellation_lines = nrow(audit),
    cancellations_matched = sum(matched_mask),
    cancellations_unmatched = sum(!matched_mask),
    cancellations_ambiguous = sum(matched_mask & audit$ambiguity != "Single candidate"),
    matched_rows_removed = sum(tx$status == RP_STATUS_LEVELS[7]),
    matched_value_removed = sum(tx$line_value[tx$status == RP_STATUS_LEVELS[7]]),
    matched_rows_non_product = sum(tx$matched_cancelled & tx$is_non_product),
    non_product_rows_removed = sum(tx$status == RP_STATUS_LEVELS[8]),
    non_product_value_removed = sum(tx$line_value[tx$status == RP_STATUS_LEVELS[8]]),
    final_rows = nrow(final),
    final_value = sum(final$line_value),
    final_customers = length(unique(final$CustomerID)),
    final_invoices = length(unique(final$InvoiceNo)),
    final_countries = length(unique(final$Country)),
    customers_lost = setdiff(unique(prelim$CustomerID), unique(final$CustomerID))
  )

  list(
    transactions = tx, final = final, cancellation_audit = audit,
    funnel = funnel, non_product = non_product, summary = summary
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
