# Validate the supplied workbook and print a concise audit.
#
# Usage, from the project root:  Rscript scripts/validate_data.R
#
# Uses the same reading, validation and cleaning functions as the app. The
# expected figures below are the audit findings recorded before implementation;
# they are compared, never substituted, so any difference is reported.

root <- if (file.exists("app.R")) "." else if (file.exists(file.path("..", "app.R"))) ".." else stop("Run from the project root.")
source(file.path(root, "R", "pipeline.R"))
rp_source_all(root)
cfg <- rp_config()

path <- rp_path(cfg$data_path, root)
cat("Workbook:", path, "\n")
cat("MD5:     ", unname(tools::md5sum(path)), "\n\n")

raw <- rp_read_transactions(path, cfg$sheet)
tx <- rp_standardize(raw, cfg)
audit <- rp_audit_raw(tx)
cleaning <- rp_clean_transactions(tx, cfg)
s <- cleaning$summary

expected <- list(
  n_rows = 541909, n_missing_customer = 135080, n_missing_description = 1454,
  n_cancellation_rows = 9288, n_adjustment_rows = 3, n_negative_quantity = 10624,
  n_nonpositive_price = 2517, preliminary_rows = 397884,
  preliminary_customers = 4338, preliminary_invoices = 18532,
  preliminary_countries = 37, preliminary_single_invoice_customers = 1493
)
observed <- c(audit, s)
check <- data.frame(
  measure = names(expected),
  expected = unlist(expected),
  observed = vapply(names(expected), function(n) as.numeric(observed[[n]]), numeric(1))
)
check$result <- ifelse(check$expected == check$observed, "match", "DIFFERENT")
rownames(check) <- NULL

cat("Columns found:", paste(setdiff(names(raw), "row_id"), collapse = ", "), "\n")
cat("Type-conversion warnings:", audit$n_read_warnings, "| unparseable values:", audit$n_parse_errors, "\n")
cat("Date range:", format(audit$first_timestamp), "to", format(audit$last_timestamp), "\n")
cat("Reference date:", format(audit$reference_date), "\n")
cat(sprintf("Preliminary eligible value: %s (audit noted about \u00a38,911,408)\n\n", rp_fmt_gbp(s$preliminary_value, 2)))
print(check, row.names = FALSE)

cat("\nExclusion funnel (each row counted once, first applicable reason):\n")
print(cleaning$funnel[, c("step", "status", "rows", "purchase_value")], row.names = FALSE)

cat("\nFinal analytical dataset:", rp_fmt_int(s$final_rows), "rows,",
    rp_fmt_int(s$final_customers), "customers,", rp_fmt_int(s$final_invoices), "invoices,",
    rp_fmt_gbp(s$final_value), "\n")

if (any(check$result != "match")) {
  cat("\nSome figures differ from the recorded audit; investigate before relying on results.\n")
} else {
  cat("\nAll recorded audit figures reproduced.\n")
}
