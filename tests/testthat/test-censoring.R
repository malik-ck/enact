# ── Helper ──────────────────────────────────────────────────────────────────
make_data <- function(n = 100L) {
  set.seed(42L)
  data.frame(
    X1 = rnorm(n),
    X2 = rnorm(n),
    A  = rbinom(n, 1L, 0.5),
    Y  = rnorm(n),
    Z  = rnorm(n),
    cl = sample(letters[1:4], n, replace = TRUE)
  )
}

# ── Minimal enfold learner specs for testing ────────────────────────────────
lrn <- enfold::lrn_mean("mean")
mtl <- enfold::mtl_superlearner("sl", loss_fun = enfold::loss_logistic())

# ══════════════════════════════════════════════════════════════════════════════
# Auto-detect censoring from NAs
# ══════════════════════════════════════════════════════════════════════════════

test_that("auto-detect creates censoring indicator from outcome NAs", {
  d <- make_data(50L)
  d$Y[c(2, 5, 10)] <- NA

  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add(
    task,
    A = treatment(A, learners = lrn, metalearner = mtl),
    Y = outcome(Y, learners = lrn, metalearner = mtl, label = "Y")
  )

  expect_true("Y" %in% names(task$censoring))
  expect_length(task$censoring$Y, nrow(d))
  expect_equal(task$censoring$Y[c(2, 5, 10)], c(0L, 0L, 0L))
  expect_equal(task$censoring$Y[1], 1L)
})

test_that("no censoring created when outcome has no NAs", {
  d <- make_data(50L)
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add(task,
    A = treatment(A, learners = lrn, metalearner = mtl),
    Y = outcome(Y, learners = lrn, metalearner = mtl)
  )

  expect_length(task$censoring, 0L)
  expect_false("Y" %in% names(task$censoring))
})

# ══════════════════════════════════════════════════════════════════════════════
# Manual censoring via outcome(censoring = ...)
# ══════════════════════════════════════════════════════════════════════════════

test_that("manual censoring column is picked up by add()", {
  d <- make_data(50L)
  d$C <- 1L
  d$C[c(3, 7)] <- 0L

  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add(
    task,
    A = treatment(A, learners = lrn, metalearner = mtl),
    Y = outcome(Y, learners = lrn, metalearner = mtl, censoring = C, label = "Y")
  )

  expect_true("Y" %in% names(task$censoring))
  expect_equal(task$censoring$Y[c(3, 7)], c(0L, 0L))
  expect_equal(task$censoring$Y[1], 1L)
})

test_that("manual censoring suppresses auto-detect (no remaining NAs)", {
  d <- make_data(50L)
  d$C <- 1L
  d$C[c(2, 4)] <- 0L
  # No NAs in Y

  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add(
    task,
    A = treatment(A, learners = lrn, metalearner = mtl),
    Y = outcome(Y, learners = lrn, metalearner = mtl, censoring = C, label = "Y")
  )

  # Censoring should exist (from manual) and be length nrow
  expect_true("Y" %in% names(task$censoring))
  expect_length(task$censoring$Y, nrow(d))
  expect_equal(task$censoring$Y[c(2, 4)], c(0L, 0L))
  expect_equal(sum(task$censoring$Y == 0L), 2L)
})

test_that("auto-detect fills gaps when manual censoring misses NAs", {
  d <- make_data(50L)
  d$C <- 1L
  d$C[3] <- 0L
  # Also put an NA at row 8 — manual C doesn't cover it
  d$Y[8] <- NA

  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add(
    task,
    A = treatment(A, learners = lrn, metalearner = mtl),
    Y = outcome(Y, learners = lrn, metalearner = mtl, censoring = C, label = "Y")
  )

  # Both manual (row 3) and auto-detected (row 8) should be censored
  expect_true("Y" %in% names(task$censoring))
  expect_equal(task$censoring$Y[3], 0L)
  expect_equal(task$censoring$Y[8], 0L)
  expect_equal(task$censoring$Y[1], 1L)
  expect_equal(sum(task$censoring$Y == 0L), 2L)
})

test_that("manual censoring: non-existent column name errors", {
  d <- make_data(50L)

  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  expect_error(
    add(
      task,
      A = treatment(A, learners = lrn, metalearner = mtl),
      Y = outcome(Y, learners = lrn, metalearner = mtl, censoring = NonExistent)
    ),
    "Cannot resolve|not found|doesn't exist"
  )
})

test_that("manual censoring via column index", {
  d <- make_data(50L)
  d$C <- 1L
  d$C[c(3, 7)] <- 0L

  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  # C is column 7 in d (X1, X2, A, Y, Z, cl, C)
  task <- add(
    task,
    A = treatment(A, learners = lrn, metalearner = mtl),
    Y = outcome(Y, learners = lrn, metalearner = mtl, censoring = 7, label = "Y")
  )

  expect_true("Y" %in% names(task$censoring))
  expect_equal(task$censoring$Y[c(3, 7)], c(0L, 0L))
  expect_equal(task$censoring$Y[1], 1L)
})

test_that("censoring stored as integer vector, not data.frame", {
  d <- make_data(50L)
  d$C <- 1L
  d$C[5] <- 0L

  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add(
    task,
    A = treatment(A, learners = lrn, metalearner = mtl),
    Y = outcome(Y, learners = lrn, metalearner = mtl, censoring = C, label = "Y")
  )

  expect_true(is.integer(task$censoring$Y))
  expect_false(is.data.frame(task$censoring$Y))
})

test_that("censoring values outside {0, 1} produce an error", {
  d <- make_data(50L)
  d$C <- 1L
  d$C[1] <- 2L

  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  expect_error(
    add(
      task,
      A = treatment(A, learners = lrn, metalearner = mtl),
      Y = outcome(Y, learners = lrn, metalearner = mtl, censoring = C)
    ),
    "must be 0 or 1"
  )
})

test_that("print shows censored count", {
  d <- make_data(50L)
  d$Y[c(1, 2)] <- NA

  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add(
    task,
    A = treatment(A, learners = lrn, metalearner = mtl),
    Y = outcome(Y, learners = lrn, metalearner = mtl)
  )
  expect_output(print(task), "censored")
})