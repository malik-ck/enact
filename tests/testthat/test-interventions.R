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

make_task <- function(d = NULL) {
  if (is.null(d)) d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  add(task,
    A = treatment(A, learners = lrn, metalearner = mtl),
    Y = outcome(Y, learners = lrn, metalearner = mtl)
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# intervention() constructor
# ══════════════════════════════════════════════════════════════════════════════

test_that("intervention() creates nana_intervention object", {
  iv <- intervention(
    intervene  = function(a, l) { a[] <- 1; a },
    mtp        = FALSE,
    stochastic = FALSE,
    label      = "All treated"
  )
  expect_s3_class(iv, "nana_intervention")
  expect_equal(iv$label, "All treated")
  expect_false(iv$mtp)
  expect_false(iv$stochastic)
  expect_true(is.function(iv$intervene))
})

test_that("intervention() rejects non-function intervene", {
  expect_error(
    intervention(intervene = "not_a_fn", mtp = FALSE, stochastic = FALSE),
    "must be a function"
  )
})

test_that("intervention() rejects non-logical mtp/stochastic", {
  expect_error(
    intervention(intervene = function(a, l) a, mtp = "yes", stochastic = FALSE),
    "must be a single logical"
  )
  expect_error(
    intervention(intervene = function(a, l) a, mtp = FALSE, stochastic = 1),
    "must be a single logical"
  )
})

test_that("print.nana_intervention works", {
  iv <- intervention(
    intervene = function(a, l) { a[] <- 1; a },
    mtp = FALSE, stochastic = FALSE, label = "Test"
  )
  expect_output(print(iv), "Test")
})

# ══════════════════════════════════════════════════════════════════════════════
# Wrapper constructors
# ══════════════════════════════════════════════════════════════════════════════

test_that("intervention_arm() works", {
  iv <- intervention_arm("A", label = "Arm A")
  expect_s3_class(iv, "nana_intervention")
  expect_false(iv$mtp)
  expect_false(iv$stochastic)
})

test_that("static_intervention() works", {
  iv <- static_intervention(1, label = "Set to 1")
  expect_s3_class(iv, "nana_intervention")
  expect_false(iv$mtp)
  expect_false(iv$stochastic)
})

test_that("static_intervention() rejects non-scalar", {
  expect_error(static_intervention(c(1, 2)), "single numeric scalar")
  expect_error(static_intervention("a"), "single numeric scalar")
})

test_that("mtp_intervention() works", {
  iv <- mtp_intervention(function(a, l) a + 0.5, label = "Shift")
  expect_s3_class(iv, "nana_intervention")
  expect_true(iv$mtp)
  expect_false(iv$stochastic)
})

test_that("mtp_intervention() rejects non-function", {
  expect_error(mtp_intervention("not_fn"), "must be a function")
})

test_that("stochastic_intervention() works", {
  iv <- stochastic_intervention(function(a, l) a + rnorm(nrow(a)), n_draws = 50L)
  expect_s3_class(iv, "nana_intervention")
  expect_true(iv$mtp)
  expect_true(iv$stochastic)
  expect_equal(iv$n_draws, 50L)
})

test_that("pure_stochastic_intervention() works", {
  iv <- pure_stochastic_intervention(
    function(a, l) rbinom(nrow(a), 1, 0.5),
    n_draws = 25L
  )
  expect_s3_class(iv, "nana_intervention")
  expect_false(iv$mtp)
  expect_true(iv$stochastic)
})

# ══════════════════════════════════════════════════════════════════════════════
# define_interventions()
# ══════════════════════════════════════════════════════════════════════════════

test_that("define_interventions() stores interventions on task", {
  task <- make_task()
  task <- define_interventions(
    task,
    intervention_arm("A", label = "Control"),
    intervention_arm("A", label = "Treat")
  )

  expect_true(!is.null(task$interventions))
  expect_true("Control" %in% names(task$interventions))
  expect_true("Treat" %in% names(task$interventions))
})

test_that("define_interventions() errors on missing label", {
  task <- make_task()
  expect_error(
    intervention(intervene = function(a, l) a, mtp = FALSE, stochastic = FALSE),
    "label"
  )
})

test_that("define_interventions() errors on non-intervention objects", {
  task <- make_task()
  expect_error(
    define_interventions(task, "not_an_intervention"),
    "not valid intervention"
  )
})

test_that("define_interventions() errors on no treatment", {
  d <- make_data()
  task <- initiate_study(d, confounders = X1, verbose = FALSE)
  # No add() => no treatment_meta
  task <- add(task, Y = outcome(Y, learners = lrn, metalearner = mtl))
  expect_error(
    define_interventions(task, intervention_arm("A", label = "Arm")),
    "No treatment variables"
  )
})

test_that("define_interventions() warns on unknown columns", {
  task <- make_task()
  expect_warning(
    define_interventions(
      task,
      intervention(intervene = function(a, l) {
        a[, "nonexistent"] <- 1
        a
      }, mtp = FALSE, stochastic = FALSE, label = "Bad col")
    ),
    "not found in the treatment block"
  )
})

test_that("define_interventions() errors on duplicate labels", {
  task <- make_task()
  task <- define_interventions(task, intervention_arm("A", label = "Arm"))
  expect_error(
    define_interventions(task, intervention_arm("A", label = "Arm")),
    "already exist"
  )
})

# ══════════════════════════════════════════════════════════════════════════════
# intervene()
# ══════════════════════════════════════════════════════════════════════════════

test_that("intervene() returns modified treatment block", {
  task <- make_task()
  task <- define_interventions(task, intervention_arm("A", label = "Arm A"))
  result <- intervene(task, "Arm A")

  expect_true(is.matrix(result) || is.data.frame(result))
  expect_equal(nrow(result), task$n_obs)
  expect_true(all(result[, "A"] == 1L))
})

test_that("intervene() errors without interventions", {
  task <- make_task()
  expect_error(intervene(task, "Arm A"), "No interventions found")
})

test_that("intervene() errors on missing intervention name", {
  task <- make_task()
  task <- define_interventions(task, intervention_arm("A", label = "Arm A"))
  expect_error(intervene(task, "nonexistent"), "Intervention not found")
})