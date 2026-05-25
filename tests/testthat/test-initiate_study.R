# ── Helper: build a minimal data frame ──────────────────────────────────────
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

# ══════════════════════════════════════════════════════════════════════════════
# initiate_study()
# ══════════════════════════════════════════════════════════════════════════════

test_that("initiate_study returns a enact_task with correct structure", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)

  expect_s3_class(task, "enact_task")
  expect_equal(task$n_obs, nrow(d))
  expect_true(is.environment(task$data_env))
  expect_identical(task$data_env$data, d)
  expect_equal(task$confounder_cols, c("X1", "X2"))
  expect_true(is.null(task$cluster_col))
  expect_true(is.null(task$outcomes))
  expect_true(is.null(task$treatment_meta))
})

test_that("initiate_study works with matrix input", {
  d <- as.matrix(make_data())
  task <- initiate_study(d, confounders = c("X1", "X2"), verbose = FALSE)

  expect_s3_class(task, "enact_task")
  expect_equal(task$n_obs, nrow(d))
  expect_true(is.matrix(task$data_env$data))
})

test_that("initiate_study accepts cluster", {
  d <- make_data()
  task <- initiate_study(d, confounders = c("X1", "X2"),
                         cluster = cl, verbose = FALSE)

  expect_equal(task$cluster_col, "cl")
})

test_that("initiate_study stores confounder labels", {
  d <- make_data()
  task <- initiate_study(
    d, confounders = c(X1, X2),
    confounder_labels = c(X1 = "Covariate 1", X2 = "Covariate 2"),
    verbose = FALSE
  )
  expect_equal(task$confounder_labels[["X1"]], "Covariate 1")
  expect_equal(task$confounder_labels[["X2"]], "Covariate 2")
})

test_that("initiate_study rejects non-data input", {
  expect_error(initiate_study("not_a_df", confounders = X1),
               "must be a data.frame or matrix")
})

test_that("initiate_study rejects unnamed extra_vars", {
  d <- make_data()
  expect_error(
    initiate_study(d, confounders = X1, extra_vars = list(1, 2), verbose = FALSE),
    "fully named list"
  )
})

test_that("data_env is locked — no modification after creation", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  expect_error(
    task$data_env$data <- d[, 1],
    "cannot change value"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# add_treatment()
# ══════════════════════════════════════════════════════════════════════════════

test_that("add_treatment() attaches treatment to task", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add_treatment(task, "A", A, label = "Arm A")

  expect_true(!is.null(task$treatment_meta))
  expect_true("A" %in% names(task$treatment_meta))
  expect_true("A" %in% names(task$treatment_labels))
  expect_equal(task$treatment_labels[["A"]], "Arm A")
})

test_that("add_treatment() rejects non-string label", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  expect_error(add_treatment(task, "A", A, label = 42), "character string")
  expect_error(add_treatment(task, "A", A, label = c("a", "b")), "character string")
})

test_that("add_treatment() rejects bad name argument", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  expect_error(add_treatment(task, "", A), "non-empty character string")
  expect_error(add_treatment(task, c("A", "B"), A), "non-empty character string")
})

test_that("add_treatment() errors on duplicate name", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add_treatment(task, "A", A)
  expect_error(add_treatment(task, "A", A), "already exists")
})

# ══════════════════════════════════════════════════════════════════════════════
# add_outcome()
# ══════════════════════════════════════════════════════════════════════════════

test_that("add_outcome() attaches outcome to task", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- task |>
    add_treatment("A", A) |>
    add_outcome("Y", Y, label = "My Y")

  expect_true("Y" %in% names(task$outcomes))
  expect_true("Y" %in% names(task$outcome_labels))
  expect_equal(task$outcome_labels[["Y"]], "My Y")
})

test_that("add_outcome() with adjustment_set", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add_outcome(task, "Y", Y, adjustment_set = "X1")

  expect_equal(task$adjustment_sets[["Y"]], 1L)
})

test_that("add_outcome() NULL adjustment_set inherits all confounders", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add_outcome(task, "Y", Y)

  expect_true(is.null(task$adjustment_sets[["Y"]]))
})

test_that("add_outcome() rejects non-string label", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  expect_error(add_outcome(task, "Y", Y, label = 99), "character string")
})

test_that("add_outcome() errors on duplicate outcome name", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add_outcome(task, "Y", Y)
  expect_error(add_outcome(task, "Y", Y), "already exists")
})

test_that("add_outcome() errors on treatment-outcome column overlap", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add_treatment(task, "A", A)
  expect_error(add_outcome(task, "Y", A), "also appear in treatment")
})

test_that("incremental: treatment first, outcome later", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add_treatment(task, "A", A)
  task <- add_outcome(task, "Y", Y)

  expect_true("A" %in% names(task$treatment_meta))
  expect_true("Y" %in% names(task$outcomes))
})

test_that("piped: treatment and outcome together", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE) |>
    add_treatment("A", A, label = "Arm") |>
    add_outcome("Y", Y, label = "Response")
  expect_true("A" %in% names(task$treatment_meta))
  expect_true("Y" %in% names(task$outcomes))
})

# ══════════════════════════════════════════════════════════════════════════════
# print method
# ══════════════════════════════════════════════════════════════════════════════

test_that("print.enact_task works without error", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE) |>
    add_treatment("A", A) |>
    add_outcome("Y", Y)
  expect_output(print(task), "enact_task")
  expect_output(print(task), "Confounders")
  expect_output(print(task), "Treatment")
  expect_output(print(task), "Outcome")
})
