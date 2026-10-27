# Run the complete RetailPulse analysis without the dashboard.
#
# Usage, from the project root:  Rscript scripts/run_analysis.R
#
# Re-runs the pipeline from the raw workbook (ignoring any cache), saves the
# results cache used by the app, and writes derived CSV tables to
# data/processed/. The raw workbook is only read, never modified.

root <- if (file.exists("app.R")) "." else if (file.exists(file.path("..", "app.R"))) ".." else stop("Run from the project root.")
source(file.path(root, "R", "pipeline.R"))
rp_source_all(root)
cfg <- rp_config()

res <- rp_load_results(cfg, root, use_cache = FALSE)
out_dir <- rp_path(cfg$processed_dir, root)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
write_out <- function(df, name) {
  utils::write.csv(df, file.path(out_dir, name), row.names = FALSE, na = "")
}

lapse <- rp_apply_lapse_rules(res$customers, cfg$ratio_threshold, cfg$fixed_days)
write_out(lapse[, c(
  "CustomerID", "segment", "country", "recency", "frequency", "monetary",
  "first_purchase", "last_purchase", "n_purchase_dates", "n_intervals",
  "cadence_status", "typical_interval", "inactivity_ratio", "personalized_flag",
  "fixed_flag", "rule_applied", "hybrid_flag", "alert_category", "explanation"
)], "customers.csv")
write_out(res$data_quality, "data_quality.csv")
write_out(res$cleaning$funnel, "exclusion_funnel.csv")
write_out(res$cleaning$non_product, "non_product_lines.csv")
write_out(res$cleaning$cancellation_audit, "cancellation_audit.csv")
write_out(res$monthly, "monthly_trend.csv")
write_out(res$rfm_summary, "rfm_summary.csv")
write_out(res$segmentation$candidates, "cluster_candidates.csv")
write_out(res$segmentation$profile, "segment_profiles.csv")
write_out(res$segmentation$candidate_profiles, "segment_profiles_all_k.csv")
write_out(res$default_lapse$same_population$table, "lapse_comparison_A_same_population.csv")
write_out(res$default_lapse$hybrid, "lapse_comparison_B_hybrid.csv")
write_out(res$default_lapse$sensitivity, "lapse_sensitivity.csv")
if (!is.null(res$rule_comparison)) {
  write_out(res$rule_comparison$rule_profile, "rfm_rule_profiles.csv")
  write_out(data.frame(rule_group = rownames(res$rule_comparison$crosstab), res$rule_comparison$crosstab, check.names = FALSE), "rfm_rule_vs_kmeans_crosstab.csv")
}

s <- res$cleaning$summary
seg <- res$segmentation
a <- res$default_lapse$same_population
cat("\n=== RetailPulse results ===\n")
cat("Reference date:", format(res$reference_date), "\n")
cat(sprintf("Final dataset: %s rows, %s customers, %s invoices, %s\n",
  rp_fmt_int(s$final_rows), rp_fmt_int(s$final_customers), rp_fmt_int(s$final_invoices), rp_fmt_gbp(s$final_value)))
cat(sprintf("Cancellation matching: %s of %s lines matched; %s purchase lines (%s) excluded\n",
  rp_fmt_int(s$cancellations_matched), rp_fmt_int(s$cancellation_lines), rp_fmt_int(s$matched_rows_removed), rp_fmt_gbp(s$matched_value_removed)))
cat(sprintf("Non-product lines excluded: %s (%s)\n", rp_fmt_int(s$non_product_rows_removed), rp_fmt_gbp(s$non_product_value_removed)))
cat("\nCluster candidates:\n")
print(seg$candidates[, c("k", "valid", "silhouette", "between_ss_ratio", "min_cluster_size")], row.names = FALSE)
cat(sprintf("Selected k = %d (%s)\n\n", seg$k, seg$selection_rule))
print(seg$profile[, c("segment", "customers", "median_recency", "median_frequency", "median_monetary", "share_value")], row.names = FALSE)
cat(sprintf("\nComparison A (%s qualified customers), Jaccard = %.3f\n", rp_fmt_int(a$population), a$jaccard))
print(a$table, row.names = FALSE)
cat("\nComparison B (hybrid policy, all customers)\n")
print(res$default_lapse$hybrid, row.names = FALSE)
if (!is.null(res$rule_comparison)) {
  cat(sprintf("\nRFM rules vs K-Means: Adjusted Rand Index = %.3f\n", res$rule_comparison$adjusted_rand))
}
cat("\nDerived tables written to", normalizePath(out_dir), "\n")
