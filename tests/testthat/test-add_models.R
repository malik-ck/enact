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

lrn <- enfold::lrn_mean("mean")
mtl <- enfold::mtl_superlearner("sl", loss_fun = enfold::loss_logistic())

make_task <- function(d = NULL) {
  if (is.null(d)) d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  add(task, A = treatment(A), Y = outcome(Y))
}

make_task_with_folds <- function(d = NULL, inner_cv = 2L, outer_cv = 2L) {
  task <- make_task(d)
  add_cv_folds(task, inner_cv = inner_cv, outer_cv = outer_cv, verbose = FALSE)
}

# ══════════════════════════════════════════════════════════════════════════════
# Selector constructors
# ══════════════════════════════════════════════════════════════════════════════

test_that("treatments() creates selector with correct class", {
  sel <- treatments(learners = lrn)
  expect_s3_class(sel, "enact_selector_treatments")
  expect_null(sel$names)
  expect_identical(sel$learners, lrn)
  expect_null(sel$metalearners)
})

test_that("treatments() with names stores them", {
  sel <- treatments("A", learners = lrn)
  expect_equal(sel$names, "A")
})

test_that("outcomes() creates selector with correct class", {
  sel <- outcomes(learners = lrn, metalearners = mtl)
  expect_s3_class(sel, "enact_selector_outcomes")
  expect_null(sel$names)
  expect_identical(sel$metalearners, mtl)
})

test_that("censoring() creates selector with correct class", {
  sel <- censoring(learners = lrn)
  expect_s3_class(sel, "enact_selector_censoring")
})

test_that("mtp() creates selector with correct class", {
  sel <- mtp(learners = lrn)
  expect_s3_class(sel, "enact_selector_mtp")
})

test_that("treatments() errors without learners", {
  expect_error(treatments(), "required")
})

test_that("treatments() errors with multiple learners but no metalearners", {
  expect_error(treatments(learners = list(lrn, lrn)), "metalearners")
})

# ══════════════════════════════════════════════════════════════════════════════
# add_models() validation
# ══════════════════════════════════════════════════════════════════════════════

test_that("add_models() rejects calls without selectors", {
  task <- make_task_with_folds()
  expect_error(add_models(task), "At least one selector")
})

test_that("add_models() rejects non-selector objects", {
  task <- make_task_with_folds()
  expect_error(add_models(task, "not_a_selector"), "not a valid selector")
})

test_that("add_models() rejects calls before add()", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  expect_error(add_models(task, treatments(learners = lrn)), "Call add\\(\\)")
})

test_that("add_models() rejects calls without CV folds", {
  task <- make_task()
  expect_error(add_models(task, treatments(learners = lrn)), "Call add_cv_folds\\(\\)")
})

test_that("add_models() rejects multiple learners without outer CV", {
  task <- make_task()
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = NULL, verbose = FALSE)
  expect_error(
    add_models(task, treatments(learners = list(lrn, lrn), metalearners = mtp)),
    "outer CV"
  )
})

test_that("add_models() rejects duplicate selector types", {
  task <- make_task_with_folds()
  expect_error(
    add_models(task, treatments(learners = lrn), treatments(learners = lrn)),
    "Duplicate selector"
  )
})

test_that("add_models() rejects duplicate calls", {
  task <- make_task_with_folds()
  task <- add_models(task, treatments(learners = lrn))
  expect_error(
    add_models(task, outcomes(learners = lrn)),
    "already been added"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# add_models() with treatments()
# ══════════════════════════════════════════════════════════════════════════════

test_that("add_models() with treatments() creates treatment tasks and specs", {
  task <- make_task_with_folds()
  task <- add_models(task, treatments(learners = lrn))

  expect_true("A" %in% names(task$treatment_tasks))
})

test_that("add_models() with named treatments() targets specific treatment", {
  d <- make_data()
  d$A2 <- rbinom(nrow(d), 1L, 0.5)
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add(task, A = treatment(A), A2 = treatment(A2), Y = outcome(Y))
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  task <- add_models(task, treatments("A", learners = lrn))

  expect_true("A" %in% names(task$treatment_tasks))
  expect_false("A2" %in% names(task$treatment_tasks))
})

test_that("add_models() with treatments() errors on unknown name", {
  task <- make_task_with_folds()
  expect_error(
    add_models(task, treatments("nonexistent", learners = lrn)),
    "not found in task"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# add_models() with outcomes()
# ══════════════════════════════════════════════════════════════════════════════

test_that("add_models() with outcomes() creates outcome tasks and specs", {
  task <- make_task_with_folds()
  task <- add_models(task, outcomes(learners = lrn, metalearners = mtl))

  expect_true("Y" %in% names(task$outcome_tasks))
})

test_that("add_models() with named outcomes() targets specific outcome", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add(task, A = treatment(A), Y = outcome(Y), Z = outcome(Z))
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  task <- add_models(task, outcomes("Y", learners = lrn))

  expect_true("Y" %in% names(task$outcome_tasks))
  expect_false("Z" %in% names(task$outcome_tasks))
})

# ══════════════════════════════════════════════════════════════════════════════
# add_models() with censoring()
# ══════════════════════════════════════════════════════════════════════════════

test_that("add_models() with censoring() creates tasks for censored outcomes", {
  d <- make_data()
  d$C <- 1L
  d$C[c(2, 5)] <- 0L
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  task <- add(task, A = treatment(A), Y = outcome(Y, censoring = C))
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  task <- add_models(task, censoring(learners = lrn))

  expect_true("Y" %in% names(task$censoring_tasks))
})

test_that("add_models() with censoring() warns when no outcomes have censoring", {
  task <- make_task_with_folds()
  expect_warning(
    add_models(task, censoring(learners = lrn)),
    "No outcomes have censoring"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# add_models() with mtp()
# ══════════════════════════════════════════════════════════════════════════════

test_that("add_models() with mtp() creates MTP tasks", {
  task <- make_task_with_folds()
  task <- define_interventions(
    task,
    mtp_intervention(function(a, l) a + 0.5, label = "shift"),
    static_intervention(1, label = "static")
  )
  task <- add_models(task, mtp(learners = lrn, metalearners = mtl), treatments(learners = lrn))

  expect_true("shift" %in% names(task$mtp_tasks))
  expect_s3_class(task$mtp_tasks[["shift"]], "enfold_task")
  expect_false("static" %in% names(task$mtp_tasks))
})

test_that("add_models() with mtp() requires MTP interventions", {
  task <- make_task_with_folds()
  task <- define_interventions(
    task,
    static_intervention(1, label = "static")
  )
  expect_error(
    add_models(task, mtp(learners = lrn)),
    "No MTP interventions"
  )
})

test_that("add_models() with mtp() requires interventions to be defined", {
  task <- make_task_with_folds()
  expect_error(
    add_models(task, mtp(learners = lrn)),
    "No interventions found"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# Combined selectors
# ══════════════════════════════════════════════════════════════════════════════

test_that("add_models() with multiple selectors in one call", {
  task <- make_task_with_folds()
  task <- add_models(task,
    treatments(learners = lrn),
    outcomes(learners = lrn, metalearners = mtl)
  )

  expect_true("A" %in% names(task$treatment_tasks))
  expect_true("Y" %in% names(task$outcome_tasks))
})

test_that("full pipeline: initiate -> add -> folds -> interventions -> add_models", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE) |>
    add(A = treatment(A), Y = outcome(Y, censoring = NULL)) |>
    add_cv_folds(inner_cv = 2L, outer_cv = 2L, verbose = FALSE) |>
    define_interventions(static_intervention(1, label = "static"))

  task <- add_models(task, treatments(learners = lrn), outcomes(learners = lrn))

  expect_true("A" %in% names(task$treatment_tasks))
  expect_true("Y" %in% names(task$outcome_tasks))
  expect_true(is.null(task$mtp_tasks))
})
