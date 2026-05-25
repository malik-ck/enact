# ── Helper ──────────────────────────────────────────────────────────────────
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

make_task_with_folds <- function(d = NULL, inner_cv = 2L, outer_cv = 2L) {
  task <- make_task(d)
  add_cv_folds(task, inner_cv = inner_cv, outer_cv = outer_cv, verbose = FALSE)
}

test_data <- make_data(n = 10000L) %>% mutate(clust = as.character(round(runif(nrow(.), 1L, 30L))))
test_task <- initiate_study(test_data, confounders = X1, verbose = TRUE, cluster = clust)

# ══════════════════════════════════════════════════════════════════════════════
# define_interventions() now populates intervened_data
# ══════════════════════════════════════════════════════════════════════════════

test_that("define_interventions() populates intervened_data", {
  task <- make_task()
  task <- define_interventions(task, static_intervention(1, label = "static"))

  expect_true(!is.null(task$intervened_data))
  expect_true("static" %in% names(task$intervened_data))
  expect_equal(nrow(task$intervened_data$static), task$n_obs)
})

test_that("define_interventions() caches multiple interventions", {
  task <- make_task()
  task <- define_interventions(
    task,
    static_intervention(1, label = "treat"),
    static_intervention(0, label = "control")
  )

  expect_true(all(names(task$intervened_data) %in% c("treat", "control")))
  expect_true(all(task$intervened_data$treat$A == 1))
  expect_true(all(task$intervened_data$control$A == 0))
})

test_that("define_interventions() appends intervened_data on second call", {
  task <- make_task()
  task <- define_interventions(task, static_intervention(1, label = "treat"))
  task <- define_interventions(task, static_intervention(0, label = "control"))

  expect_equal(length(task$intervened_data), 2L)
  expect_true(all(c("treat", "control") %in% names(task$intervened_data)))
})


# ══════════════════════════════════════════════════════════════════════════════
# fit_interventions() — input validation
# ══════════════════════════════════════════════════════════════════════════════

test_that("fit_interventions() rejects non-task", {
  expect_error(fit_interventions("not_a_task"), "enact_task")
})

test_that("fit_interventions() rejects task without interventions", {
  task <- make_task_with_folds()
  expect_error(fit_interventions(task), "define_interventions")
})

test_that("fit_interventions() rejects task without treatment models", {
  task <- make_task_with_folds()
  task <- define_interventions(task, static_intervention(1, label = "static"))
  expect_error(fit_interventions(task), "add_models")
})

test_that("fit_interventions() rejects invalid truncate", {
  task <- make_task_with_folds()
  task <- define_interventions(task, static_intervention(1, label = "static"))
  task <- add_models(task, treatments(learners = lrn, metalearners = mtl), outcomes(learners = lrn, metalearners = mtl))
  expect_error(fit_interventions(task, truncate = 0), "truncate")
  expect_error(fit_interventions(task, truncate = 1), "truncate")
  expect_error(fit_interventions(task, truncate = c(0.1, 0.2)), "truncate")
})


# ══════════════════════════════════════════════════════════════════════════════
# fit_outcomes() — input validation
# ══════════════════════════════════════════════════════════════════════════════

test_that("fit_outcomes() rejects non-task", {
  expect_error(fit_outcomes("not_a_task"), "enact_task")
})

test_that("fit_outcomes() rejects task without outcome models", {
  task <- make_task_with_folds()
  task <- define_interventions(task, static_intervention(1, label = "static"))
  expect_error(fit_outcomes(task), "add_models")
})

test_that("fit_outcomes() rejects task without interventions", {
  task <- make_task_with_folds()
  task <- add_models(task, outcomes(learners = lrn, metalearners = mtl))
  expect_error(fit_outcomes(task), "define_interventions")
})


# ══════════════════════════════════════════════════════════════════════════════
# fit_interventions() — fitting and clever covariates
# ══════════════════════════════════════════════════════════════════════════════

test_that("fit_interventions() fits treatment tasks and creates clever covariates", {
  task <- make_task_with_folds()
  task <- define_interventions(task, static_intervention(1, label = "static"))
  task <- add_models(task, treatments(learners = lrn, metalearners = mtl), outcomes(learners = lrn, metalearners = mtl))

  task <- fit_interventions(task)

  # Treatment tasks should be fitted
  expect_true(inherits(task$treatment_tasks$A, "enfold_task_fitted"))

  # Clever covariates structure: one matrix per outcome, interventions as columns
  expect_true(!is.null(task$clever_covariates))
  expect_true("Y" %in% names(task$clever_covariates))
  expect_true(is.matrix(task$clever_covariates$Y))
  expect_true("static" %in% colnames(task$clever_covariates$Y))
  expect_equal(nrow(task$clever_covariates$Y), task$n_obs)

  H <- task$clever_covariates$Y[, "static"]
  expect_true(is.numeric(H))
})

test_that("fit_interventions() MTP clever covariates are positive", {
  task <- make_task_with_folds()
  task <- define_interventions(
    task,
    mtp_intervention(function(a, l) a + 0.5, label = "shift")
  )
  task <- add_models(
    task,
    treatments(learners = lrn, metalearners = mtl),
    outcomes(learners = lrn, metalearners = mtl),
    mtp(learners = lrn, metalearners = mtl)
  )

  task <- fit_interventions(task)

  expect_true("shift" %in% colnames(task$clever_covariates$Y))
  H_mtp <- task$clever_covariates$Y[, "shift"]
  expect_true(all(H_mtp > 0))
})

test_that("fit_interventions() truncation bounds the clever covariate", {
  task <- make_task_with_folds()
  task <- define_interventions(task, static_intervention(1, label = "static"))
  task <- add_models(task, treatments(learners = lrn, metalearners = mtl), outcomes(learners = lrn, metalearners = mtl))

  task <- fit_interventions(task, truncate = 0.1)

  H <- task$clever_covariates$Y[, "static"]
  # H = 1/ps where ps >= 0.1, so H <= 10
  expect_true(all(H <= 1 / 0.1 + 1e-8))
})


# ══════════════════════════════════════════════════════════════════════════════
# fit_outcomes() — fitting and predictions
# ══════════════════════════════════════════════════════════════════════════════

test_that("fit_outcomes() fits and creates outcome predictions", {
  task <- make_task_with_folds()
  task <- define_interventions(task, static_intervention(1, label = "static"))
  task <- add_models(task, outcomes(learners = lrn, metalearners = mtl))

  task <- fit_outcomes(task)

  expect_true(!is.null(task$outcome_predictions))
  expect_true("Y" %in% names(task$outcome_predictions))
  expect_true(is.matrix(task$outcome_predictions$Y))
  expect_true("static" %in% colnames(task$outcome_predictions$Y))
  expect_equal(nrow(task$outcome_predictions$Y), task$n_obs)

  Q <- task$outcome_predictions$Y[, "static"]
  expect_true(is.numeric(Q))
})

test_that("fit_outcomes() produces predictions for all interventions", {
  task <- make_task_with_folds()
  task <- define_interventions(
    task,
    static_intervention(1, label = "treat"),
    static_intervention(0, label = "control")
  )
  task <- add_models(task, outcomes(learners = lrn, metalearners = mtl))

  task <- fit_outcomes(task)

  expect_true(all(c("treat", "control") %in% colnames(task$outcome_predictions$Y)))
  expect_equal(nrow(task$outcome_predictions$Y), task$n_obs)
  expect_equal(ncol(task$outcome_predictions$Y), 2L)
})


# ══════════════════════════════════════════════════════════════════════════════
# Independence — fit_outcomes works without fit_interventions
# ══════════════════════════════════════════════════════════════════════════════

test_that("fit_outcomes() works independently of fit_interventions()", {
  task <- make_task_with_folds()
  task <- define_interventions(task, static_intervention(1, label = "static"))
  task <- add_models(task, outcomes(learners = lrn, metalearners = mtl))

  # Do NOT call fit_interventions — go straight to fit_outcomes
  task <- fit_outcomes(task)

  expect_true(!is.null(task$outcome_predictions))
  expect_true(is.null(task$clever_covariates))
})

test_that("fit_interventions() works independently of fit_outcomes()", {
  task <- make_task_with_folds()
  task <- define_interventions(task, static_intervention(1, label = "static"))
  task <- add_models(task, treatments(learners = lrn, metalearners = mtl), outcomes(learners = lrn, metalearners = mtl))

  # Do NOT call fit_outcomes — go straight to fit_interventions
  task <- fit_interventions(task)

  expect_true(!is.null(task$clever_covariates))
  expect_true(is.null(task$outcome_predictions))
})


# ══════════════════════════════════════════════════════════════════════════════
# MTP vs non-MTP equivalence for a static intervention
# ══════════════════════════════════════════════════════════════════════════════

test_that("2N density ratio recovers analytic ratio for continuous shift MTP", {
  set.seed(42L)
  n <- 2000L
  delta <- 0.5

  X1 <- rnorm(n)
  X2 <- rnorm(n)
  mu_w <- 0.5 * X1
  A  <- rnorm(n, mean = mu_w, sd = 1)
  Y  <- rnorm(n, mean = A + X1, sd = 1)

  d <- data.frame(X1 = X1, X2 = X2, A = A, Y = Y)

  true_r <- dnorm(A, mu_w + delta, 1) / dnorm(A, mu_w, 1)

  task <- initiate_study(d, confounders = c(X1, X2), verbose = FALSE)
  task <- add(task, A = treatment(A), Y = outcome(Y))
  task <- add_cv_folds(task, inner_cv = 2L, outer_cv = 2L, verbose = FALSE)
  task <- define_interventions(
    task,
    mtp_intervention(function(a, l) {
      a[[1L]] <- a[[1L]] + delta
      a
    }, label = "shift")
  )
  task <- add_models(
    task,
    treatments(learners = lrn_glm, metalearners = mtl),
    outcomes(learners = lrn_glm, metalearners = mtl),
    mtp(learners = lrn_glm, metalearners = mtl)
  )
  task <- fit_interventions(task)

  H_mtp <- as.numeric(task$clever_covariates$Y[, "shift"])

  expect_true(cor(H_mtp, true_r) > 0.95)

  fit <- lm(H_mtp ~ true_r)
  expect_true(abs(coef(fit)[2] - 1) < 0.15)
})


# ══════════════════════════════════════════════════════════════════════════════
# Complex pipeline: two outcomes, censoring, adjustment sets
# ══════════════════════════════════════════════════════════════════════════════

test_that("full pipeline: two outcomes, censoring, adjustment sets", {
  set.seed(42L)
  n <- 300L
  X1 <- rnorm(n); X2 <- rnorm(n); X3 <- rnorm(n)
  d <- data.frame(
    X1 = X1,
    X2 = X2,
    X3 = X3,
    A  = rbinom(n, 1L, plogis(X1)),
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
    mtp_intervention(function(a, l) a + 0.5, label = "shift")
  )
  task <- add_models(
    task,
    treatments(learners = lrn, metalearners = mtl),
    outcomes(learners = lrn, metalearners = mtl),
    censoring(learners = lrn, metalearners = mtl),
    mtp(learners = lrn, metalearners = mtl)
  )

  # intervened_data should already be populated
  expect_true(!is.null(task$intervened_data))
  expect_true(all(c("treat", "shift") %in% names(task$intervened_data)))

  task <- fit_interventions(task)
  task <- fit_outcomes(task)

  # Both outcomes should have matrices with both intervention columns
  for (out in c("Y1", "Y2")) {
    expect_true(is.matrix(task$clever_covariates[[out]]))
    expect_true(is.matrix(task$outcome_predictions[[out]]))
    expect_equal(nrow(task$clever_covariates[[out]]), task$n_obs)
    expect_equal(nrow(task$outcome_predictions[[out]]), task$n_obs)
    expect_true(all(c("treat", "shift") %in% colnames(task$clever_covariates[[out]])))
    expect_true(all(c("treat", "shift") %in% colnames(task$outcome_predictions[[out]])))
  }

  # clever_covariates and outcome_predictions share the same structure
  expect_identical(
    names(task$clever_covariates),
    names(task$outcome_predictions)
  )
  for (out in names(task$clever_covariates)) {
    expect_identical(
      colnames(task$clever_covariates[[out]]),
      colnames(task$outcome_predictions[[out]])
    )
  }
})
