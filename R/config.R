# RetailPulse configuration -------------------------------------------------
#
# Every analytical setting lives here so that the pipeline, the standalone
# scripts and the Shiny app all use the same values. Paths are relative to the
# project root (the folder containing app.R).

rp_config <- function(...) {
  cfg <- list(
    # Input -------------------------------------------------------------
    data_path = file.path("data", "raw", "online_retail.xlsx"),
    sheet = 1,
    processed_dir = file.path("data", "processed"),
    required_columns = c(
      "InvoiceNo", "StockCode", "Description", "Quantity",
      "InvoiceDate", "UnitPrice", "CustomerID", "Country"
    ),

    # Cleaning ----------------------------------------------------------
    # Exact, normalised StockCode values that identify service or charge lines
    # rather than products (postage, manual entries, carriage, bank charges...).
    non_product_codes = c("POST", "M", "DOT", "C2", "BANK CHARGES", "PADS"),
    # Absolute tolerance when comparing a cancellation's UnitPrice with the
    # original purchase line's UnitPrice (prices are recorded to 2-3 decimals).
    price_tolerance = 1e-6,

    # Segmentation ------------------------------------------------------
    cluster_features = c("recency", "frequency", "monetary"),
    k_range = 2:6,
    k_override = NULL, # set to an integer to force a cluster count
    seed = 42,
    nstart = 25,
    iter_max = 100,
    # A candidate solution is treated as degenerate if any cluster holds fewer
    # customers than this.
    min_cluster_size = 10,
    # Silhouette needs a full distance matrix; above this many customers a
    # reproducible random sample is used instead.
    silhouette_max_n = 8000,

    # Purchase-lapse rules ----------------------------------------------
    ratio_threshold = 2.0,
    fixed_days = 60,
    min_invoices = 3,
    min_purchase_dates = 3,
    min_intervals = 2
  )
  overrides <- list(...)
  cfg[names(overrides)] <- overrides
  cfg
}

# Fixed display labels shared by the pipeline and the dashboard.
RP_METHOD_PERSONALIZED <- "Personalized cadence"
RP_METHOD_FALLBACK <- "Fixed fallback"
RP_ALERT_PERSONALIZED <- "Personalized cadence signal"
RP_ALERT_FALLBACK <- "Fallback threshold signal"
RP_ALERT_NONE <- "Not flagged"
