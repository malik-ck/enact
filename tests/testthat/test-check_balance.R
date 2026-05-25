# ── Helpers ──────────────────────────────────────────────────────────────────
future::plan(future::sequential)

make_data <- function(n = 200L) {
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

make_ready_task <- function(d = NULL) {
  if (is.null(d)) d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add(task, A = treatment(A), Y = outcome(Y))
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  task <- define_interventions(
    task,
    static_intervention(1, label = "treat"),
    static_intervention(0, label = "control"),
    reference = "control"
  )
  task <- add_models(
    task,
    treatments(learners = lrn, metalearners = mtl),
    outcomes(learners = lrn, metalearners = mtl)
  )
  fit_interventions(task)
}


# ══════════════════════════════════════════════════════════════════════════════
# Input validation
# ══════════════════════════════════════════════════════════════════════════════

test_that("check_balance() rejects non-task", {
  expect_error(check_balance("not_a_task"), "enact_task")
})

test_that("check_balance() rejects task without clever covariates", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add(task, A = treatment(A), Y = outcome(Y))
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  task <- define_interventions(task, static_intervention(1, label = "treat"))
  task <- add_models(task, treatments(learners = lrn, metalearners = mtl))
  # fit_interventions NOT called
  expect_error(check_balance(task), "fit_interventions")
})

test_that("check_balance() rejects invalid threshold", {
  task <- make_ready_task()
  expect_error(check_balance(task, threshold = 0), "threshold")
  expect_error(check_balance(task, threshold = -1), "threshold")
  expect_error(check_balance(task, threshold = c(0.1, 0.2)), "threshold")
})


# ══════════════════════════════════════════════════════════════════════════════
# Structure of returned object
# ══════════════════════════════════════════════════════════════════════════════

test_that("check_balance() returns enact_task with balance_checks", {
  task <- make_ready_task()
  task <- check_balance(task)

  expect_true(inherits(task$balance_checks, "enact_balance_check"))
  expect_true(is.numeric(task$balance_checks$threshold))
  expect_true("Y" %in% names(task$balance_checks$outcomes))
})

test_that("balance_checks contains correct per-outcome structure", {
  task <- make_ready_task()
  task <- check_balance(task)

  oc <- task$balance_checks$outcomes$Y
  expect_true(is.list(oc$interventions))
  expect_true("treat" %in% names(oc$interventions))
  expect_true(is.numeric(oc$max_smd))
  expect_true(is.numeric(oc$total_violations))

  iv <- oc$interventions$treat
  expect_true(is.data.frame(iv$smd_df))
  expect_true(all(c("confounder", "smd", "abs_smd", "violation") %in% names(iv$smd_df)))
  expect_true("ggplot" %in% class(iv$love_plot))
  expect_true("ggplot" %in% class(iv$density_plot))
  expect_true(is.numeric(iv$n_violations))
})

test_that("SMD data.frame has one row per confounder", {
  task <- make_ready_task()
  task <- check_balance(task)

  smd_df <- task$balance_checks$outcomes$Y$interventions$treat$smd_df
  expect_equal(nrow(smd_df), 2L)  # X1, X2
  expect_true(all(smd_df$confounder %in% c("X1", "X2")))
})


# ══════════════════════════════════════════════════════════════════════════════
# Labels — no raw variable names in plots
# ══════════════════════════════════════════════════════════════════════════════

test_that("confounder labels are used instead of raw names", {
  d <- make_data()
  task <- initiate_study(
    d, confounders = c(X1, X2),
    confounder_labels = c(X1 = "Confounder A", X2 = "Confounder B"),
    verbose = FALSE
  )
  task <- add(task, A = treatment(A), Y = outcome(Y))
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  task <- define_interventions(
    task,
    static_intervention(1, label = "treat"),
    static_intervention(0, label = "control"),
    reference = "control"
  )
  task <- add_models(
    task,
    treatments(learners = lrn, metalearners = mtl),
    outcomes(learners = lrn, metalearners = mtl)
  )
  task <- fit_interventions(task)
  task <- check_balance(task)

  smd_df <- task$balance_checks$outcomes$Y$interventions$treat$smd_df
  expect_true("Confounder A" %in% smd_df$confounder)
  expect_true("Confounder B" %in% smd_df$confounder)
  expect_false("X1" %in% smd_df$confounder)
  expect_false("X2" %in% smd_df$confounder)
})


# ══════════════════════════════════════════════════════════════════════════════
# Violation counting
# ══════════════════════════════════════════════════════════════════════════════

test_that("violations match |SMD| > threshold", {
  task <- make_ready_task()
  task <- check_balance(task, threshold = 0.1)

  smd_df <- task$balance_checks$outcomes$Y$interventions$treat$smd_df
  expected <- sum(abs(smd_df$smd) > 0.1)
  expect_equal(task$balance_checks$outcomes$Y$interventions$treat$n_violations, expected)
})

test_that("threshold = Inf produces zero violations", {
  task <- make_ready_task()
  task <- check_balance(task, threshold = Inf)

  expect_equal(task$balance_checks$outcomes$Y$interventions$treat$n_violations, 0L)
  expect_equal(task$balance_checks$outcomes$Y$total_violations, 0L)
})


# ══════════════════════════════════════════════════════════════════════════════
# Multiple outcomes + censoring
# ══════════════════════════════════════════════════════════════════════════════

test_that("check_balance() handles multiple outcomes with censoring", {
  set.seed(42L)
  n <- 300L
  d <- data.frame(
    X1 = rnorm(n), X2 = rnorm(n), X3 = rnorm(n),
    A  = rbinom(n, 1L, plogis(rnorm(n))),
    C  = rbinom(n, 1L, 0.8)
  )
  d$Y1 <- rnorm(n) + d$A
  d$Y1[d$C == 0L] <- NA
  d$Y2 <- rnorm(n) + 0.5 * d$A

  task <- initiate_study(d, confounders = c(X1, X2, X3), verbose = FALSE)
  task <- add(task,
    A  = treatment(A),
    Y1 = outcome(Y1, censoring = C, adjustment_set = c("X1", "X2")),
    Y2 = outcome(Y2)
  )
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  task <- define_interventions(
    task,
    static_intervention(1, label = "treat"),
    static_intervention(0, label = "control"),
    reference = "control"
  )
  task <- add_models(
    task,
    treatments(learners = lrn, metalearners = mtl),
    outcomes(learners = lrn, metalearners = mtl),
    censoring(learners = lrn, metalearners = mtl)
  )
  task <- fit_interventions(task)
  task <- check_balance(task)

  expect_true(all(c("Y1", "Y2") %in% names(task$balance_checks$outcomes)))

  # Y1 has custom adjustment set (X1, X2 only)
  smd_y1 <- task$balance_checks$outcomes$Y1$interventions$treat$smd_df
  expect_equal(nrow(smd_y1), 2L)

  # Y2 uses full confounder set (X1, X2, X3)
  smd_y2 <- task$balance_checks$outcomes$Y2$interventions$treat$smd_df
  expect_equal(nrow(smd_y2), 3L)
})


# ══════════════════════════════════════════════════════════════════════════════
# MTP interventions
# ══════════════════════════════════════════════════════════════════════════════

test_that("check_balance() works with MTP interventions", {
  d <- make_data()
  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add(task, A = treatment(A), Y = outcome(Y))
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  task <- define_interventions(
    task,
    static_intervention(1, label = "treat"),
    mtp_intervention(function(a, l) a + 0.5, label = "shift"),
    reference = "treat"
  )
  task <- add_models(
    task,
    treatments(learners = lrn, metalearners = mtl),
    outcomes(learners = lrn, metalearners = mtl),
    mtp(learners = lrn, metalearners = mtl)
  )
  task <- fit_interventions(task)
  task <- check_balance(task)

  oc <- task$balance_checks$outcomes$Y
  expect_true("shift" %in% names(oc$interventions))
  expect_true(is.data.frame(oc$interventions$shift$smd_df))
})


# ══════════════════════════════════════════════════════════════════════════════
# print() and summary() methods
# ══════════════════════════════════════════════════════════════════════════════

test_that("print.enact_balance_check() produces output", {
  task <- make_ready_task()
  task <- check_balance(task)

  expect_output(print(task$balance_checks), "Balance check")
  expect_output(print(task$balance_checks), "threshold")
})

test_that("summary.enact_balance_check() returns data.frame", {
  task <- make_ready_task()
  task <- check_balance(task)

  s <- summary(task$balance_checks)
  expect_true(is.data.frame(s))
  expect_true(all(c("Outcome", "Intervention", "confounder", "smd", "abs_smd", "violation") %in% names(s)))
  expect_true(nrow(s) > 0)
})
