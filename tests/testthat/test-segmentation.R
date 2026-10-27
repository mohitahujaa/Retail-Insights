three_groups <- function() {
  set.seed(7)
  mk <- function(n, r, f, m) {
    data.frame(
      recency = pmax(1, round(stats::rnorm(n, r, r / 10))),
      frequency = pmax(1, round(stats::rnorm(n, f, f / 10))),
      monetary = pmax(1, stats::rnorm(n, m, m / 10))
    )
  }
  d <- rbind(mk(40, 5, 20, 5000), mk(40, 60, 4, 800), mk(40, 300, 1, 100))
  d$CustomerID <- as.character(seq_len(nrow(d)))
  d
}

test_that("preprocessing rejects invalid features", {
  d <- three_groups()
  bad <- d
  bad$monetary[1] <- NA
  expect_error(rp_prepare_cluster_matrix(bad), "missing or non-finite")
  bad <- d
  bad$recency[1] <- -1
  expect_error(rp_prepare_cluster_matrix(bad), "non-negative")
  bad <- d
  bad$frequency <- 3
  expect_error(rp_prepare_cluster_matrix(bad), "Zero-variance.*frequency")
  expect_error(rp_prepare_cluster_matrix(d, c("recency", "nope")), "not found: nope")
})

test_that("preprocessing standardises log1p features", {
  X <- rp_prepare_cluster_matrix(three_groups())
  expect_equal(unname(colMeans(X)), c(0, 0, 0), tolerance = 1e-12)
  expect_equal(unname(apply(X, 2, stats::sd)), c(1, 1, 1), tolerance = 1e-12)
})

test_that("fixed seed gives reproducible segmentation", {
  cfg <- rp_test_config()
  s1 <- rp_segment_customers(three_groups(), cfg)
  s <- rp_segment_customers(three_groups(), cfg)
  expect_identical(s1$customers$segment_id, s$customers$segment_id)
  expect_identical(s1$candidates, s$candidates)
})

test_that("well-separated groups are recovered and profiled", {
  s <- rp_segment_customers(three_groups(), rp_test_config())
  expect_equal(s$k, 3)
  expect_equal(nrow(s$customers), 120)
  expect_equal(sort(s$profile$customers), c(40, 40, 40))
  expect_equal(sum(s$profile$share_value), 1)
  # S1 has the highest median value by construction of the numbering.
  expect_equal(s$profile$segment_id[which.max(s$profile$median_monetary)], 1)
  expect_match(s$profile$segment[1], "^S1: High-value active customers")
  expect_true(all(c(2, 3, 4) %in% s$candidate_profiles$k))
})

test_that("invalid and degenerate candidates are handled", {
  d <- three_groups()[1:6, ]
  cfg <- rp_config(k_range = c(2, 6, 7), min_cluster_size = 1, nstart = 5)
  X <- rp_prepare_cluster_matrix(d)
  fits <- rp_fit_kmeans_candidates(X, cfg)
  expect_false(fits$scores$valid[fits$scores$k == 6])
  expect_match(fits$scores$note[fits$scores$k == 7], "not smaller")

  strict <- rp_config(k_range = 2:3, min_cluster_size = 1000)
  sc <- rp_fit_kmeans_candidates(rp_prepare_cluster_matrix(three_groups()), strict)$scores
  expect_true(all(!sc$valid))
  expect_error(rp_select_k(sc), "No valid K-Means candidate")
})

test_that("k selection prefers the smaller k on ties and honours valid overrides", {
  sc <- data.frame(k = 2:4, valid = TRUE, silhouette = c(0.4, 0.4, 0.3))
  expect_equal(rp_select_k(sc)$k, 2)
  expect_equal(rp_select_k(sc, k_override = 4)$k, 4)
  expect_warning(k <- rp_select_k(sc, k_override = 9)$k, "not a valid candidate")
  expect_equal(k, 2)
})

test_that("rule-based RFM comparison produces scores and agreement", {
  s <- rp_segment_customers(three_groups(), rp_test_config())
  rc <- rp_compare_rfm_rules(s$customers)
  expect_true(all(rc$customers$r_score %in% 1:5))
  expect_equal(sum(rc$rule_profile$customers), 120)
  expect_equal(rp_adjusted_rand(c(1, 1, 2, 2), c("a", "a", "b", "b")), 1)
  expect_lt(abs(rp_adjusted_rand(rep(1:2, 50), rep(1:2, each = 50))), 0.05)
})

test_that("every valid k has stored assignments and a consistent view", {
  s <- rp_segment_customers(three_groups(), rp_test_config())
  expect_setequal(names(s$assignments), c("CustomerID", "k2", "k3", "k4"))
  for (k in 2:4) {
    v <- rp_segment_view(s, s$customers, k)
    expect_equal(v$k, k)
    expect_equal(length(unique(v$customers$segment)), k)
    expect_equal(sort(v$profile$customers), sort(as.integer(table(v$customers$segment_id))))
    expect_false(any(is.na(v$profile$suggested_use)))
  }
  expect_identical(rp_segment_view(s, s$customers, s$k)$customers$segment, s$customers$segment)
  expect_error(rp_segment_view(s, s$customers, 9), "not a valid")
})

test_that("crosswalk and utility measures add up", {
  s <- rp_segment_customers(three_groups(), rp_test_config())
  cust <- rp_add_cadence(
    transform(s$customers, frequency = pmax(frequency, 1), n_purchase_dates = frequency),
    data.frame(CustomerID = character(), gap_days = integer())
  )
  cw <- rp_segment_crosswalk(s, cust, 2, 4)
  expect_equal(sum(cw$customers), nrow(cust))
  expect_equal(sum(cw$value), sum(cust$monetary))
  expect_equal(as.numeric(tapply(cw$share_of_to, cw$to, sum)), rep(1, 4))

  u <- rp_segmentation_utility(s, cust, c(2, 4), 2, 60)
  expect_equal(sort(unique(u$segments$k)), c(2, 4))
  expect_equal(nrow(u$segments), 6)
  expect_equal(as.numeric(tapply(u$segments$share_value, u$segments$k, sum)), c(1, 1))
  expect_equal(sum(u$segments$flagged[u$segments$k == 2]), sum(u$segments$flagged[u$segments$k == 4]))
  expect_true(all(c("Mean silhouette (statistical separation)", "Lapse-signal rate range across segments") %in% u$summary$measure))

  ins <- rp_segmentation_insights(s, cust, 2, 4, u)
  expect_match(ins[1], "^Separation: k = 2 has mean silhouette")
  expect_length(grep("Suggested use", ins), 4)
})
