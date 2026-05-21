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

# ── Minimal enfold learner specs for testing ────────────────────────────────
lrn <- enfold::lrn_mean("mean")
mtl <- enfold::mtl_superlearner("sl", loss_fun = enfold::loss_logistic())

# ══════════════════════════════════════════════════════════════════════════════
# initiate_study()
# ══════════════════════════════════════════════════════════════════════════════

test_that("initiate_study returns a nana_task with correct structure", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)

  expect_s3_class(task, "nana_task")
  expect_equal(task$n_obs, nrow(d))
  expect_true(is.environment(task$data_env))
  expect_identical(task$data_env$data, d)
  expect_equal(task$confounder_cols, c("X1", "X2"))
  expect_true(is.null(task$cluster_cols))
  expect_true(is.null(task$outcomes))
  expect_true(is.null(task$treatment_meta))
})

test_that("initiate_study works with matrix input", {
  d <- as.matrix(make_data())
  task <- initiate_study(d, confounders = c("X1", "X2"), verbose = FALSE)

  expect_s3_class(task, "nana_task")
  expect_equal(task$n_obs, nrow(d))
  expect_true(is.matrix(task$data_env$data))
})

test_that("initiate_study accepts cluster", {
  d <- make_data()
  task <- initiate_study(d, confounders = c("X1", "X2"),
                         cluster = cl, verbose = FALSE)

  expect_equal(task$cluster_cols, "cl")
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
# treatment() constructor
# ══════════════════════════════════════════════════════════════════════════════

test_that("treatment() creates enact_treatment object", {
  trt <- treatment(A, learners = lrn, metalearner = mtl, label = "Treatment A")
  expect_s3_class(trt, "enact_treatment")
  expect_equal(trt$label, "Treatment A")
  expect_true(rlang::quo_is_call(trt$which) || rlang::quo_is_symbol(trt$which))
})

test_that("treatment() accepts learners list", {
  trt <- treatment(A, learners = list(lrn), metalearner = mtl, label = "A")
  expect_true(is.list(trt$learners))
})

test_that("treatment() rejects non-string label", {
  expect_error(treatment(A, learners = lrn, metalearner = mtl, label = 42), "character string")
  expect_error(treatment(A, learners = lrn, metalearner = mtl, label = c("a", "b")), "character string")
})

# ══════════════════════════════════════════════════════════════════════════════
# outcome() constructor
# ══════════════════════════════════════════════════════════════════════════════

test_that("outcome() creates enact_outcome object", {
  oc <- outcome(Y, learners = lrn, metalearner = mtl,
                label = "Outcome Y", adjustment_set = c("X1"))
  expect_s3_class(oc, "enact_outcome")
  expect_equal(oc$label, "Outcome Y")
  expect_equal(oc$adjustment_set, "X1")
  expect_true(rlang::quo_is_null(oc$censoring))
})

test_that("outcome() captures censoring quosure", {
  oc <- outcome(Y, learners = lrn, metalearner = mtl,
                censoring = C, label = "Y with censoring")
  expect_false(rlang::quo_is_null(oc$censoring))
})

test_that("outcome() rejects non-string label", {
  expect_error(outcome(Y, learners = lrn, metalearner = mtl, label = 99), "character string")
})

# ══════════════════════════════════════════════════════════════════════════════
# add()
# ══════════════════════════════════════════════════════════════════════════════

test_that("add() attaches treatment to task", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add(task, A = treatment(A, learners = lrn, metalearner = mtl, label = "Arm A"))

  expect_true(!is.null(task$treatment_meta))
  expect_true("A" %in% names(task$treatment_meta))
  expect_true("A" %in% names(task$treatment_labels))
  expect_true("A" %in% names(task$treatment_tasks))
  expect_true("A" %in% names(task$treatment_specs))
  expect_equal(task$treatment_labels[["A"]], "Arm A")
})

test_that("add() attaches outcome to task", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add(task,
    A = treatment(A, learners = lrn, metalearner = mtl),
    Y = outcome(Y, learners = lrn, metalearner = mtl, label = "My Y")
  )

  expect_true("Y" %in% names(task$outcomes))
  expect_true("Y" %in% names(task$outcome_labels))
  expect_equal(task$outcome_labels[["Y"]], "My Y")
  expect_true("Y" %in% names(task$outcome_specs))
})

test_that("add() with adjustment_set", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add(task, Y = outcome(Y, learners = lrn, metalearner = mtl, adjustment_set = "X1"))

  expect_equal(task$adjustment_sets[["Y"]], 1L)
})

test_that("add() NULL adjustment_set inherits all confounders", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add(task, Y = outcome(Y, learners = lrn, metalearner = mtl))

  expect_true(is.null(task$adjustment_sets[["Y"]]))
})

test_that("add() errors on unnamed arguments", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  expect_error(add(task, treatment(A, learners = lrn, metalearner = mtl)), "must be named")
})

test_that("add() errors on duplicate treatment names", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add(task, A = treatment(A, learners = lrn, metalearner = mtl))
  expect_error(add(task, A = treatment(A, learners = lrn, metalearner = mtl)), "already exist")
})

test_that("add() errors on duplicate outcome names", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add(task, Y = outcome(Y, learners = lrn, metalearner = mtl))
  expect_error(add(task, Y = outcome(Y, learners = lrn, metalearner = mtl)), "already exist")
})

test_that("add() errors on non-treatment/outcome objects", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  expect_error(add(task, A = "not_a_constructor"), "not treatment")
})

test_that("add() errors on treatment-outcome column overlap", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add(task, A = treatment(A, learners = lrn, metalearner = mtl))
  expect_error(
    add(task, Y = outcome(A, learners = lrn, metalearner = mtl)),
    "also appear in treatment"
  )
})

test_that("add() with both treatment and outcome in one call", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add(
    task,
    A = treatment(A, learners = lrn, metalearner = mtl, label = "Arm"),
    Y = outcome(Y, learners = lrn, metalearner = mtl, label = "Response")
  )
  expect_true("A" %in% names(task$treatment_meta))
  expect_true("Y" %in% names(task$outcomes))
})

test_that("add() incremental: treatment first, outcomes later", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add(task, A = treatment(A, learners = lrn, metalearner = mtl))
  task <- add(task, Y = outcome(Y, learners = lrn, metalearner = mtl))

  expect_true("A" %in% names(task$treatment_meta))
  expect_true("Y" %in% names(task$outcomes))
})

# ══════════════════════════════════════════════════════════════════════════════
# print method
# ══════════════════════════════════════════════════════════════════════════════

test_that("print.nana_task works without error", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add(task,
    A = treatment(A, learners = lrn, metalearner = mtl),
    Y = outcome(Y, learners = lrn, metalearner = mtl)
  )
  expect_output(print(task), "nana_task")
  expect_output(print(task), "Confounders")
  expect_output(print(task), "Treatment")
  expect_output(print(task), "Outcome")
})