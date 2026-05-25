# ── Helpers ──────────────────────────────────────────────────────────────────
make_data <- function(n = 100L) {
  set.seed(42L)
  data.frame(
    X1 = rnorm(n),
    X2 = rnorm(n),
    A  = rbinom(n, 1L, 0.5),
    Y  = rnorm(n)
  )
}

lrn <- enfold::lrn_ranger("ranger", num.trees = 50L)
lrn_glm <- enfold::lrn_glm("glm", family = "auto")
mtl <- enfold::mtl_selector("sel")

make_task <- function(d = NULL) {
  if (is.null(d)) d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  add(task, A = treatment(A), Y = outcome(Y))
}

make_fitted_task <- function(d = NULL, inner_cv = 2L, outer_cv = 2L) {
  task <- make_task(d)
  task <- add_cv_folds(task, inner_cv = inner_cv, outer_cv = outer_cv, verbose = FALSE)
  task <- define_interventions(
    task,
    static_intervention(1, label = "treat"),
    static_intervention(0, label = "control")
  )
  task <- add_models(
    task,
    treatments(learners = lrn_glm, metalearners = mtl),
    outcomes(learners = lrn_glm, metalearners = mtl)
  )
  task <- fit_interventions(task)
  task <- fit_outcomes(task)
  task
}


# ══════════════════════════════════════════════════════════════════════════════
# all_outcomes()
# ══════════════════════════════════════════════════════════════════════════════

test_that("all_outcomes() returns NULL", {
  expect_null(all_outcomes())
})


# ══════════════════════════════════════════════════════════════════════════════
# generate_fwb_weights()
# ══════════════════════════════════════════════════════════════════════════════

test_that("generate_fwb_weights() produces correct dimensions", {
  w <- generate_fwb_weights(50L, 100L)
  expect_equal(dim(w), c(100L, 50L))
})

test_that("generate_fwb_weights() rows sum to 1", {
  w <- generate_fwb_weights(30L, 200L)
  row_sums <- rowSums(w)
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("generate_fwb_weights() all values are positive", {
  w <- generate_fwb_weights(30L, 200L)
  expect_true(all(w > 0))
})

test_that("generate_fwb_weights() with clustering produces correct dimensions", {
  cluster <- rep(1:5, each = 10L)
  w <- generate_fwb_weights(50L, 100L, cluster = cluster)
  expect_equal(dim(w), c(100L, 50L))
})

test_that("generate_fwb_weights() clustered weights rows sum to 1", {
  cluster <- rep(1:4, each = 25L)
  w <- generate_fwb_weights(100L, 200L, cluster = cluster)
  row_sums <- rowSums(w)
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("generate_fwb_weights() clustered: obs in same cluster get same weight", {
  cluster <- c("a", "a", "b", "b", "c")
  w <- generate_fwb_weights(5L, 50L, cluster = cluster)
  # obs 1 and 2 share cluster "a"
  expect_equal(w[, 1], w[, 2])
  # obs 3 and 4 share cluster "b"
  expect_equal(w[, 3], w[, 4])
})


# ══════════════════════════════════════════════════════════════════════════════
# fwb_targeting()
# ══════════════════════════════════════════════════════════════════════════════

test_that("fwb_targeting() returns correct dimensions", {
  set.seed(1L)
  n <- 80L; n_b <- 50L; n_iv <- 2L
  y <- rnorm(n)
  Q <- matrix(runif(n * n_iv, 0.2, 0.8), ncol = n_iv)
  H <- matrix(runif(n * n_iv, 0.5, 2), ncol = n_iv)
  colnames(Q) <- colnames(H) <- c("iv1", "iv2")
  wb <- generate_fwb_weights(n, n_b)

  tsm <- fwb_targeting(y, Q, H, gaussian(), wb, seq_len(n))
  expect_equal(dim(tsm), c(n_b, n_iv))
  expect_identical(colnames(tsm), c("iv1", "iv2"))
})

test_that("fwb_targeting() produces finite values for binomial family", {
  set.seed(2L)
  n <- 100L; n_b <- 30L; n_iv <- 2L
  y <- rbinom(n, 1L, 0.5)
  Q <- matrix(runif(n * n_iv, 0.1, 0.9), ncol = n_iv)
  H <- matrix(runif(n * n_iv, 0.5, 3), ncol = n_iv)
  colnames(Q) <- colnames(H) <- c("a", "b")
  wb <- generate_fwb_weights(n, n_b)

  tsm <- fwb_targeting(y, Q, H, binomial(), wb, seq_len(n))
  expect_true(all(is.finite(tsm)))
  expect_true(all(tsm > 0 & tsm < 1))
})

test_that("fwb_targeting() respects obs_idx for censoring", {
  set.seed(3L)
  n <- 80L; n_b <- 30L
  y <- rnorm(n)
  y[71:80] <- NA
  Q <- matrix(runif(n, 0.2, 0.8), ncol = 1L)
  H <- matrix(runif(n, 0.5, 2), ncol = 1L)
  colnames(Q) <- colnames(H) <- "iv"
  wb <- generate_fwb_weights(n, n_b)
  obs_idx <- 1:70

  tsm <- fwb_targeting(y, Q, H, gaussian(), wb, obs_idx)
  expect_equal(dim(tsm), c(n_b, 1L))
  expect_true(all(is.finite(tsm)))
})


# ══════════════════════════════════════════════════════════════════════════════
# summarize_fwb()
# ══════════════════════════════════════════════════════════════════════════════

test_that("summarize_fwb() produces correct structure", {
  set.seed(4L)
  n_b <- 200L
  tsm_mat <- matrix(rnorm(n_b * 2, mean = c(0.5, 0.7), sd = 0.1), ncol = 2)
  colnames(tsm_mat) <- c("control", "treat")

  df <- summarize_fwb(
    tsm_mat, contrasts = list(ate()), ref_idx = 1L,
    intervention_labels = c("control", "treat"), ci_method = "nonparametric"
  )

  expect_s3_class(df, "data.frame")
  expect_true(all(c("Contrast", "Estimate", "SE", "Lower_CI", "Upper_CI") %in% names(df)))
  # 2 TSMs + 1 contrast (treat vs control)
  expect_equal(nrow(df), 3L)
})

test_that("summarize_fwb() includes contrast labels", {
  set.seed(5L)
  n_b <- 200L
  tsm_mat <- matrix(rnorm(n_b * 2, mean = c(0.5, 0.7), sd = 0.1), ncol = 2)
  colnames(tsm_mat) <- c("ctrl", "tx")

  df <- summarize_fwb(
    tsm_mat, contrasts = list(ate()), ref_idx = 1L,
    intervention_labels = c("ctrl", "tx"), ci_method = "nonparametric"
  )

  expect_true(any(grepl("TSM - ctrl", df$Contrast)))
  expect_true(any(grepl("TSM - tx", df$Contrast)))
  expect_true(any(grepl("ATE", df$Contrast)))
})

test_that("summarize_fwb() parametric CI includes SE", {
  set.seed(6L)
  n_b <- 300L
  tsm_mat <- matrix(rnorm(n_b * 2, mean = c(0.4, 0.6), sd = 0.05), ncol = 2)
  colnames(tsm_mat) <- c("c", "t")

  df <- summarize_fwb(
    tsm_mat, contrasts = list(ate()), ref_idx = 1L,
    intervention_labels = c("c", "t"), ci_method = "parametric"
  )

  expect_true(all(df$SE > 0))
  # Parametric CI: estimate ± 1.96 * SE (tolerance for floating-point)
  expect_true(all(abs(df$Upper_CI - (df$Estimate + 1.96 * df$SE)) < 1e-8))
  expect_true(all(abs(df$Lower_CI - (df$Estimate - 1.96 * df$SE)) < 1e-8))
})

test_that("summarize_fwb() handles multiple contrasts", {
  set.seed(7L)
  n_b <- 200L
  tsm_mat <- matrix(rnorm(n_b * 2, mean = c(0.5, 0.7), sd = 0.1), ncol = 2)
  colnames(tsm_mat) <- c("ref", "tx")

  df <- summarize_fwb(
    tsm_mat, contrasts = list(ate(), log_relative_ate()), ref_idx = 1L,
    intervention_labels = c("ref", "tx"), ci_method = "nonparametric"
  )

  # 2 TSMs + 2 contrasts (ATE + Log Relative ATE)
  expect_equal(nrow(df), 4L)
  expect_true(any(grepl("ATE", df$Contrast)))
  expect_true(any(grepl("Log Relative ATE", df$Contrast)))
})


# ══════════════════════════════════════════════════════════════════════════════
# do_tmle() — input validation
# ══════════════════════════════════════════════════════════════════════════════

test_that("do_tmle() rejects task without clever covariates", {
  task <- make_task()
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  task <- define_interventions(task, static_intervention(1, label = "iv"))
  task <- add_models(task, outcomes(learners = lrn_glm, metalearners = mtl))
  task <- fit_outcomes(task)

  expect_error(do_tmle(task, "Y", gaussian()), "clever covariates")
})

test_that("do_tmle() rejects task without outcome predictions", {
  task <- make_task()
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  task <- define_interventions(task, static_intervention(1, label = "iv"))
  task <- add_models(task, treatments(learners = lrn_glm, metalearners = mtl), outcomes(learners = lrn_glm, metalearners = mtl))
  task <- fit_interventions(task)

  expect_error(do_tmle(task, "Y", gaussian()), "outcome predictions")
})

test_that("do_tmle() rejects non-family object", {
  task <- make_fitted_task()
  expect_error(do_tmle(task, "Y", "gaussian"), "family")
})

test_that("do_tmle() rejects invalid outcome names", {
  task <- make_fitted_task()
  expect_error(do_tmle(task, "nonexistent", gaussian()), "not found")
})

test_that("do_tmle() rejects invalid ci_method", {
  task <- make_fitted_task()
  expect_error(do_tmle(task, "Y", gaussian(), ci_method = "boot"), "arg")
})


# ══════════════════════════════════════════════════════════════════════════════
# do_tmle() — end-to-end
# ══════════════════════════════════════════════════════════════════════════════

test_that("do_tmle() produces tmle_results", {
  task <- make_fitted_task()
  task <- do_tmle(task, "Y", gaussian(), n_bstrap = 50L)

  expect_true(!is.null(task$tmle_results))
  expect_true("Y" %in% names(task$tmle_results))
  expect_s3_class(task$tmle_results$Y, "data.frame")
  expect_true(all(c("Contrast", "Estimate", "SE", "Lower_CI", "Upper_CI") %in% names(task$tmle_results$Y)))
})

test_that("do_tmle() produces correct number of rows", {
  task <- make_fitted_task()
  task <- do_tmle(task, "Y", gaussian(), n_bstrap = 50L)

  # 2 TSMs (treat, control) + 1 ATE contrast
  expect_equal(nrow(task$tmle_results$Y), 3L)
})

test_that("do_tmle() with all_outcomes() targets all outcomes", {
  set.seed(42L)
  n <- 100L
  d <- data.frame(
    X1 = rnorm(n), X2 = rnorm(n),
    A = rbinom(n, 1L, 0.5),
    Y1 = rnorm(n), Y2 = rnorm(n)
  )
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add(task, A = treatment(A), Y1 = outcome(Y1), Y2 = outcome(Y2))
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  task <- define_interventions(
    task,
    static_intervention(1, label = "treat"),
    static_intervention(0, label = "control")
  )
  task <- add_models(
    task,
    treatments(learners = lrn_glm, metalearners = mtl),
    outcomes(learners = lrn_glm, metalearners = mtl)
  )
  task <- fit_interventions(task)
  task <- fit_outcomes(task)
  task <- do_tmle(task, all_outcomes(), gaussian(), n_bstrap = 50L)

  expect_true(all(c("Y1", "Y2") %in% names(task$tmle_results)))
})

test_that("do_tmle() nonparametric CIs are quantile-based", {
  task <- make_fitted_task()
  task <- do_tmle(task, "Y", gaussian(), n_bstrap = 200L, ci_method = "nonparametric")

  df <- task$tmle_results$Y
  # Lower < Estimate < Upper for TSMs
  tsm_rows <- grepl("TSM", df$Contrast)
  expect_true(all(df$Lower_CI[tsm_rows] < df$Estimate[tsm_rows]))
  expect_true(all(df$Upper_CI[tsm_rows] > df$Estimate[tsm_rows]))
})

test_that("do_tmle() parametric CIs are symmetric around estimate", {
  task <- make_fitted_task()
  task <- do_tmle(task, "Y", gaussian(), n_bstrap = 200L, ci_method = "parametric")

  df <- task$tmle_results$Y
  # Upper - Estimate ≈ Estimate - Lower ≈ 1.96 * SE
  half_width <- df$Upper_CI - df$Estimate
  expect_true(all(abs(half_width - 1.96 * df$SE) < 1e-10))
})


# ══════════════════════════════════════════════════════════════════════════════
# do_tmle() — keep_bootstrap_samples
# ══════════════════════════════════════════════════════════════════════════════

test_that("do_tmle() does not store bootstrap by default", {
  task <- make_fitted_task()
  task <- do_tmle(task, "Y", gaussian(), n_bstrap = 50L)

  expect_null(task$tmle_bootstrap)
})

test_that("do_tmle() stores bootstrap when requested", {
  task <- make_fitted_task()
  task <- do_tmle(task, "Y", gaussian(), n_bstrap = 50L, keep_bootstrap_samples = TRUE)

  expect_true(!is.null(task$tmle_bootstrap))
  expect_true("Y" %in% names(task$tmle_bootstrap))
  expect_equal(nrow(task$tmle_bootstrap$Y), 50L)
  expect_equal(ncol(task$tmle_bootstrap$Y), 2L)
})


# ══════════════════════════════════════════════════════════════════════════════
# do_tmle() — contrast labels
# ══════════════════════════════════════════════════════════════════════════════

test_that("do_tmle() includes ATE label from ate() contrast", {
  task <- make_fitted_task()
  task <- do_tmle(task, "Y", gaussian(), n_bstrap = 50L)

  df <- task$tmle_results$Y
  expect_true(any(grepl("ATE", df$Contrast)))
})

test_that("do_tmle() works with multiple contrasts", {
  task <- make_fitted_task()
  # Override outcome contrasts to include multiple
  task$outcome_contrasts$Y <- list(ate(), log_relative_ate())
  task <- do_tmle(task, "Y", gaussian(), n_bstrap = 50L)

  df <- task$tmle_results$Y
  # 2 TSMs + 2 contrasts = 4 rows
  expect_equal(nrow(df), 4L)
  expect_true(any(grepl("Log Relative ATE", df$Contrast)))
})


# ══════════════════════════════════════════════════════════════════════════════
# do_tmle() — clustering
# ══════════════════════════════════════════════════════════════════════════════

test_that("do_tmle() works with cluster variable", {
  set.seed(42L)
  n <- 100L
  d <- data.frame(
    X1 = rnorm(n), X2 = rnorm(n),
    A = rbinom(n, 1L, 0.5),
    Y = rnorm(n),
    cl = sample(letters[1:4], n, replace = TRUE)
  )
  task <- initiate_study(d, confounders = c(X1, X2), cluster = cl, verbose = FALSE)
  task <- add(task, A = treatment(A), Y = outcome(Y))
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  task <- define_interventions(
    task,
    static_intervention(1, label = "treat"),
    static_intervention(0, label = "control")
  )
  task <- add_models(
    task,
    treatments(learners = lrn_glm, metalearners = mtl),
    outcomes(learners = lrn_glm, metalearners = mtl)
  )
  task <- fit_interventions(task)
  task <- fit_outcomes(task)
  task <- do_tmle(task, "Y", gaussian(), n_bstrap = 50L)

  expect_true(!is.null(task$tmle_results$Y))
  expect_equal(nrow(task$tmle_results$Y), 3L)
})


# ══════════════════════════════════════════════════════════════════════════════
# do_tmle() — binomial family
# ══════════════════════════════════════════════════════════════════════════════

test_that("do_tmle() works with binomial outcome", {
  set.seed(42L)
  n <- 200L
  d <- data.frame(
    X1 = rnorm(n), X2 = rnorm(n),
    A = rbinom(n, 1L, 0.5),
    Y = rbinom(n, 1L, 0.4)
  )
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add(task, A = treatment(A), Y = outcome(Y))
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  task <- define_interventions(
    task,
    static_intervention(1, label = "treat"),
    static_intervention(0, label = "control")
  )
  task <- add_models(
    task,
    treatments(learners = lrn_glm, metalearners = mtl),
    outcomes(learners = lrn_glm, metalearners = mtl)
  )
  task <- fit_interventions(task)
  task <- fit_outcomes(task)
  task <- do_tmle(task, "Y", binomial(), n_bstrap = 50L)

  df <- task$tmle_results$Y
  # Results should exist and have finite SE
  expect_true(nrow(df) == 3L)
  expect_true(all(is.finite(df$SE)))
  # TSM rows should have estimates in plausible range
  tsm_rows <- grepl("TSM", df$Contrast)
  expect_true(all(df$Estimate[tsm_rows] > 0))
})
