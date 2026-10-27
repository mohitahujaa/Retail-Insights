# End-to-end analytical pipeline ---------------------------------------------
#
# Used by scripts/run_analysis.R, the Shiny app and the tests, so every entry
# point produces identical results.

# Run the pipeline on an in-memory raw transaction table.
rp_run_pipeline_df <- function(raw, cfg = rp_config(), verbose = FALSE) {
  say <- function(...) if (verbose) message(format(Sys.time(), "%H:%M:%S "), ...)

  say("Validating and normalising transactions")
  tx <- rp_standardize(raw, cfg)
  audit <- rp_audit_raw(tx)
  reference_date <- audit$reference_date

  say("Classifying rows and matching cancellations")
  cleaning <- rp_clean_transactions(tx, cfg)
  final <- cleaning$final

  say("Building customer RFM and cadence features")
  customers <- rp_customer_features(final, reference_date)
  intervals <- rp_purchase_intervals(final)
  customers <- rp_add_cadence(customers, intervals, cfg)

  say("Fitting K-Means candidates (k = ", min(cfg$k_range), "-", max(cfg$k_range), ")")
  seg <- rp_segment_customers(customers, cfg)

  rule_comparison <- tryCatch(rp_compare_rfm_rules(seg$customers), error = function(e) {
    warning("RFM rule comparison skipped: ", conditionMessage(e))
    NULL
  })

  say("Applying default lapse rules and evaluations")
  lapse <- rp_apply_lapse_rules(seg$customers, cfg$ratio_threshold, cfg$fixed_days)

  list(
    config = cfg,
    reference_date = reference_date,
    raw_audit = audit,
    cleaning = cleaning[c("cancellation_audit", "funnel", "non_product", "summary")],
    data_quality = rp_data_quality_table(audit, cleaning),
    monthly = rp_monthly_trend(final, audit$first_timestamp, audit$last_timestamp),
    rfm_summary = rp_describe(customers),
    rfm_correlations = rp_rfm_correlations(customers),
    interval_summary = rp_describe(
      customers[customers$cadence_qualified, , drop = FALSE], "typical_interval"
    ),
    segmentation = seg[setdiff(names(seg), "customers")],
    customers = seg$customers,
    rule_comparison = rule_comparison,
    default_lapse = list(
      same_population = rp_evaluate_rules_same_population(lapse),
      hybrid = rp_evaluate_hybrid_policy(lapse),
      sensitivity = rp_lapse_sensitivity(seg$customers)
    ),
    generated_at = Sys.time(),
    r_version = R.version.string
  )
}

# Read the configured workbook and run the pipeline.
rp_run_pipeline <- function(cfg = rp_config(), root = ".", verbose = TRUE) {
  path <- rp_path(cfg$data_path, root)
  if (verbose) message("Reading ", path)
  raw <- rp_read_transactions(path, cfg$sheet)
  res <- rp_run_pipeline_df(raw, cfg, verbose = verbose)
  res$source <- list(
    path = cfg$data_path,
    md5 = unname(tools::md5sum(path)),
    size_bytes = file.size(path)
  )
  res
}

# Cache key: changes when the workbook, the analytical code or the
# configuration changes, so stale results are never reused.
rp_cache_key <- function(cfg = rp_config(), root = ".") {
  path <- rp_path(cfg$data_path, root)
  code <- sort(list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE))
  paste(
    unname(tools::md5sum(path)),
    paste(unname(tools::md5sum(code)), collapse = ""),
    paste(deparse(cfg), collapse = ""),
    sep = "|"
  )
}

rp_cache_path <- function(cfg = rp_config(), root = ".") {
  file.path(rp_path(cfg$processed_dir, root), "pipeline_results.rds")
}

# Load cached results if they match the current inputs; otherwise run the
# pipeline and try to save the results (failure to save is not fatal).
rp_load_results <- function(cfg = rp_config(), root = ".", use_cache = TRUE, verbose = TRUE) {
  path <- rp_path(cfg$data_path, root)
  if (!file.exists(path)) {
    stop(
      "Transaction workbook not found at '", cfg$data_path, "' (relative to ",
      normalizePath(root), "). Place the UCI Online Retail workbook there; ",
      "see README.md.",
      call. = FALSE
    )
  }
  key <- rp_cache_key(cfg, root)
  cache <- rp_cache_path(cfg, root)
  if (use_cache && file.exists(cache)) {
    cached <- tryCatch(readRDS(cache), error = function(e) NULL)
    if (!is.null(cached) && identical(cached$cache_key, key)) {
      if (verbose) message("Loaded cached results from ", cache)
      return(cached)
    }
    if (verbose) message("Cached results are out of date; re-running the pipeline")
  }
  res <- rp_run_pipeline(cfg, root, verbose = verbose)
  res$cache_key <- key
  saved <- tryCatch({
    dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
    saveRDS(res, cache)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (verbose) message(if (saved) paste("Saved results to", cache) else "Could not save the results cache (continuing)")
  res
}

# Source every analytical file under R/ (not the Shiny modules).
rp_source_all <- function(root = ".", modules = FALSE) {
  files <- sort(list.files(file.path(root, "R"), pattern = "^[^_].*\\.R$", full.names = TRUE))
  if (modules) {
    files <- c(files, sort(list.files(file.path(root, "R", "modules"),
      pattern = "\\.R$", full.names = TRUE
    )))
  }
  for (f in files) source(f, local = globalenv())
  invisible(files)
}
