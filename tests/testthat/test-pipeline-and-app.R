synthetic_results <- function() rp_run_pipeline_df(rp_synthetic_raw(), rp_test_config())

test_that("the full pipeline runs end to end with consistent denominators", {
  res <- synthetic_results()
  expect_equal(nrow(res$customers), res$cleaning$summary$final_customers)
  expect_equal(sum(res$cleaning$funnel$rows), nrow(rp_synthetic_raw()))
  a <- res$default_lapse$same_population
  expect_equal(a$population, sum(res$customers$cadence_qualified))
  b <- stats::setNames(res$default_lapse$hybrid$customers, res$default_lapse$hybrid$group)
  expect_equal(unname(b["All customers"]), nrow(res$customers))
  expect_equal(
    unname(b["Flagged: personalized cadence signal"] + b["Flagged: fallback threshold signal"]),
    unname(b["Flagged in total (hybrid policy)"])
  )
  expect_true(all(res$monthly$purchase_value > 0))
})

test_that("results cache is reused only when inputs are unchanged", {
  root <- file.path(tempdir(), "rp_cache_test")
  dir.create(file.path(root, "R"), recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(file.path(RP_ROOT, "R"), full.names = TRUE, pattern = "\\.R$"), file.path(root, "R"))
  cfg <- rp_config(data_path = "data.xlsx")
  expect_error(rp_load_results(cfg, root, verbose = FALSE), "not found")
  writeLines("x", file.path(root, "data.xlsx"))
  k1 <- rp_cache_key(cfg, root)
  expect_identical(k1, rp_cache_key(cfg, root))
  expect_false(identical(k1, rp_cache_key(rp_config(data_path = "data.xlsx", fixed_days = 30), root)))
  writeLines("y", file.path(root, "data.xlsx"))
  expect_false(identical(k1, rp_cache_key(cfg, root)))
})

test_that("project code uses no absolute paths", {
  files <- c(
    list.files(file.path(RP_ROOT, "R"), pattern = "\\.R$", full.names = TRUE, recursive = TRUE),
    file.path(RP_ROOT, "app.R"),
    list.files(file.path(RP_ROOT, "scripts"), full.names = TRUE)
  )
  code <- unlist(lapply(files, readLines, warn = FALSE))
  expect_false(any(grepl("/home/|/work/|/tmp/|C:\\\\", code)))
})

test_that("all four dashboard sections build their UI", {
  res <- synthetic_results()
  for (ui in list(
    mod_overview_ui("o", res), mod_segments_ui("s", res),
    mod_lapse_ui("l", res), mod_methodology_ui("m", res)
  )) {
    expect_true(nchar(as.character(ui)) > 500)
  }
})

test_that("lapse module recalculates on threshold changes and exports the filtered list", {
  res <- synthetic_results()
  all_segments <- unique(res$customers$segment)
  shiny::testServer(mod_lapse_server, args = list(res = res), {
    session$setInputs(
      ratio_threshold = 2, fixed_days = 60, segment = all_segments,
      category = c(RP_ALERT_PERSONALIZED, RP_ALERT_FALLBACK)
    )
    session$elapse(1000)
    base <- lapse()
    expect_equal(base$hybrid_flag, rp_apply_lapse_rules(res$customers, 2, 60)$hybrid_flag)
    n_base <- sum(base$hybrid_flag)

    session$setInputs(ratio_threshold = 4, fixed_days = 150)
    session$elapse(1000)
    expect_equal(ratio(), 4)
    expect_equal(lapse()$hybrid_flag, rp_apply_lapse_rules(res$customers, 4, 150)$hybrid_flag)
    expect_lte(sum(lapse()$hybrid_flag), n_base)

    expect_true(all(filtered()$alert_category != RP_ALERT_NONE))
    csv <- utils::read.csv(output$download, stringsAsFactors = FALSE)
    expect_equal(nrow(csv), nrow(filtered()))
    expect_true(all(csv$ratio_threshold == 4) && all(csv$fixed_days_threshold == 150))
    expect_true(all(c("explanation", "inactivity_ratio", "rule_applied") %in% names(csv)))

    session$setInputs(category = RP_ALERT_FALLBACK, segment = all_segments[1])
    expect_true(all(filtered()$alert_category == RP_ALERT_FALLBACK))
    expect_true(all(filtered()$segment == all_segments[1]))
  })
})

test_that("overview, segments and methodology servers render without error", {
  res <- synthetic_results()
  lapse_state <- list(
    lapse = shiny::reactive(rp_apply_lapse_rules(res$customers, 2, 60)),
    ratio = shiny::reactive(2), days = shiny::reactive(60)
  )
  shiny::testServer(mod_overview_server, args = list(res = res, lapse_state = lapse_state), {
    expect_match(output$kpi_flagged, "%")
    expect_true(nchar(output$quality) > 0)
    expect_true(nchar(output$segment_lapse) > 0)
    expect_true(nchar(output$monthly$src) > 0)
  })
  shiny::testServer(mod_segments_server, args = list(res = res), {
    session$setInputs(alt_k = as.character(res$segmentation$k))
    expect_true(nchar(output$profile) > 0)
    expect_true(nchar(output$alt_profile) > 0)
    expect_true(nchar(output$silhouette$src) > 0)
    expect_true(nchar(output$boxplots$src) > 0)
    csv <- utils::read.csv(output$download_customers)
    expect_equal(nrow(csv), nrow(res$customers))
  })
  shiny::testServer(mod_methodology_server, args = list(res = res), {
    expect_true(nchar(output$funnel) > 0)
    expect_true(nchar(output$matching) > 0)
    expect_equal(nrow(utils::read.csv(output$download_matching)), nrow(res$cleaning$cancellation_audit))
  })
})
