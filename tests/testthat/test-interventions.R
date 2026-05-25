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


make_task <- function(d = NULL) {
  if (is.null(d)) d <- make_data()
  initiate_study(d, confounders = c(X1, X2), verbose = FALSE) |>
    add_treatment("A", A) |>
    add_outcome("Y", Y)
}

# ══════════════════════════════════════════════════════════════════════════════
# intervention() constructor
# ══════════════════════════════════════════════════════════════════════════════

test_that("intervention() creates nana_intervention object", {
  iv <- intervention(
    intervene = function(a, l) { a[] <- 1; a },
    mtp       = FALSE,
    label     = "All treated"
  )
  expect_s3_class(iv, "nana_intervention")
  expect_equal(iv$label, "All treated")
  expect_false(iv$mtp)
  expect_true(is.function(iv$intervene))
})

test_that("intervention() rejects non-function intervene", {
  expect_error(
    intervention(intervene = "not_a_fn", mtp = FALSE),
    "must be a function"
  )
})

test_that("intervention() rejects non-logical mtp", {
  expect_error(
    intervention(intervene = function(a, l) a, mtp = "yes"),
    "must be a single logical"
  )
})

test_that("print.nana_intervention works", {
  iv <- intervention(
    intervene = function(a, l) { a[] <- 1; a },
    mtp = FALSE, label = "Test"
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
})

test_that("static_intervention() works", {
  iv <- static_intervention(1, label = "Set to 1")
  expect_s3_class(iv, "nana_intervention")
  expect_false(iv$mtp)
})

test_that("static_intervention() rejects non-scalar", {
  expect_error(static_intervention(c(1, 2)), "single numeric scalar")
  expect_error(static_intervention("a"), "single numeric scalar")
})

test_that("mtp_intervention() works", {
  iv <- mtp_intervention(function(a, l) a + 0.5, label = "Shift")
  expect_s3_class(iv, "nana_intervention")
  expect_true(iv$mtp)
})

test_that("mtp_intervention() rejects non-function", {
  expect_error(mtp_intervention("not_fn"), "must be a function")
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
    intervention(intervene = function(a, l) a, mtp = FALSE),
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
  # No add_treatment() => no treatment_meta
  task <- add_outcome(task, "Y", Y)
  expect_error(
    define_interventions(task, intervention_arm("A", label = "Arm")),
    "No treatment variables"
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

test_that("intervene() errors when intervention returns wrong type", {
  task <- make_task()
  bad_iv <- intervention(
    intervene = function(a, l) 1,
    mtp = FALSE, label = "Bad"
  )
  # Inject directly to bypass define_interventions validation
  task$interventions <- list(Bad = bad_iv)
  expect_error(intervene(task, "Bad"), "data frame or matrix")
})