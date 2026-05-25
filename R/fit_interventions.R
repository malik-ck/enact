# ══════════════════════════════════════════════════════════════════════════════
# fit_interventions — fit nuisance models and build clever covariates
# ══════════════════════════════════════════════════════════════════════════════

#' Fit intervention nuisance models and compute clever covariates
#'
#' Fits all treatment, censoring, and MTP density ratio models, then computes
#' the clever covariates needed for TMLE estimation.  For each
#' outcome-intervention pair, a numeric matrix of clever covariate values is
#' stored in \code{task$clever_covariates}.
#'
#' @param task An \code{enact_task} with models already specified via
#'   \code{\link{add_models}}.
#' @param truncate Numeric.  Lower bound for the propensity score (non-MTP) or
#'   \code{1 - p} (MTP) used in the denominator of the clever covariate.
#'   Defaults to \code{1e-4}.
#'
#' @return The modified \code{enact_task} with fitted treatment, censoring, and
#'   MTP tasks, and \code{clever_covariates} populated.
#' @export
fit_interventions <- function(task, truncate = 1e-4) {
  if (!inherits(task, "enact_task")) {
    stop("`task` must be an enact_task object.", call. = FALSE)
  }
  if (is.null(task$interventions)) {
    stop("No interventions defined. Call define_interventions() first.", call. = FALSE)
  }
  if (is.null(task$treatment_tasks)) {
    stop("No treatment models found. Call add_models() with treatments() first.", call. = FALSE)
  }
  if (!is.numeric(truncate) || length(truncate) != 1L || truncate <= 0 || truncate >= 1) {
    stop("`truncate` must be a single numeric value in (0, 1).", call. = FALSE)
  }

  pred_type <- if (task$cv_ensembles) "cv" else "ensemble"
  x_data <- task$data_env$data

  # ── Fit treatment tasks ─────────────────────────────────────────────────
  for (nm in names(task$treatment_tasks)) {
    task$treatment_tasks[[nm]] <- enfold::fit(task$treatment_tasks[[nm]])
  }

  # ── Fit censoring tasks ─────────────────────────────────────────────────
  if (!is.null(task$censoring_tasks)) {
    for (nm in names(task$censoring_tasks)) {
      task$censoring_tasks[[nm]] <- enfold::fit(task$censoring_tasks[[nm]])
    }
  }

  # ── Fit MTP tasks ───────────────────────────────────────────────────────
  if (!is.null(task$mtp_tasks)) {
    for (nm in names(task$mtp_tasks)) {
      task <- fit_mtp_task(task, nm)
    }
  }

  # ── Propensity scores per treatment column (cached) ─────────────────────
  ps_cache <- list()
  for (trt_nm in names(task$treatment_tasks)) {
    raw_pred <- predict(
      task$treatment_tasks[[trt_nm]],
      newdata = x_data,
      type = pred_type
    )
    pred_mat <- as.matrix(raw_pred)
    col_nms <- vapply(
      task$treatment_meta[[trt_nm]],
      function(m) m$label_info,
      character(1L)
    )
    if (ncol(pred_mat) == length(col_nms)) {
      colnames(pred_mat) <- col_nms
    }
    for (cn in col_nms) {
      if (ncol(pred_mat) == length(col_nms)) {
        ps_cache[[cn]] <- pred_mat[, cn, drop = FALSE]
      } else {
        ps_cache[[cn]] <- pred_mat
      }
    }
  }

  # ── Clever covariates ───────────────────────────────────────────────────
  out_nms <- names(task$outcomes)
  iv_nms <- names(task$interventions)
  n <- task$n_obs
  task$clever_covariates <- vector("list", length(out_nms))
  names(task$clever_covariates) <- out_nms

  for (out_nm in out_nms) {
    # Censoring probability for this outcome (product across indicators)
    p_cens <- NULL
    if (!is.null(task$censoring_tasks[[out_nm]])) {
      raw_cens <- predict(
        task$censoring_tasks[[out_nm]],
        newdata = x_data,
        type = pred_type
      )
      cens_mat <- as.matrix(raw_cens)
      p_cens <- apply(cens_mat, 1L, prod)
    }

    mat <- matrix(NA_real_, nrow = n, ncol = length(iv_nms))
    colnames(mat) <- iv_nms

    for (iv_nm in iv_nms) {
      iv <- task$interventions[[iv_nm]]
      trt_int <- task$intervened_data[[iv_nm]]

      if (iv$mtp) {
        # MTP: density ratio → odds
        raw_dr <- predict(
          task$mtp_tasks[[iv_nm]],
          newdata = x_data,
          type = pred_type
        )
        dr <- as.numeric(raw_dr)
        H <- dr / pmax(1 - dr, truncate)
      } else {
        # Non-MTP: (censoring_prob * trt_indicator) / propensity_score
        trt_col_nms <- colnames(trt_int)
        trt_obs <- x_data[, trt_col_nms, drop = FALSE]
        trt_ind <- compute_treatment_indicator(trt_obs, trt_int, ps_cache)
        ps <- trt_ind$ps
        ind <- trt_ind$indicator

        if (!is.null(p_cens)) {
          numerator <- p_cens * ind
        } else {
          numerator <- ind
        }
        H <- numerator / pmax(ps, truncate)
      }

      mat[, iv_nm] <- H
    }

    task$clever_covariates[[out_nm]] <- mat
  }

  if (task$verbose) {
    message(sprintf(
      "fit_interventions: fitted %d treatment, %d censoring, %d MTP task(s). Clever covariates for %d outcome(s) x %d intervention(s).",
      length(task$treatment_tasks),
      if (!is.null(task$censoring_tasks)) length(task$censoring_tasks) else 0L,
      if (!is.null(task$mtp_tasks)) length(task$mtp_tasks) else 0L,
      length(out_nms),
      length(iv_nms)
    ))
  }

  task
}


# ══════════════════════════════════════════════════════════════════════════════
# fit_outcomes — fit outcome models and predict under interventions
# ══════════════════════════════════════════════════════════════════════════════

#' Fit outcome models and predict under each intervention
#'
#' Fits each outcome regression model using cross-validation folds that exclude
#' censored observations (via \code{\link{outcome_folds}}), then generates
#' predictions under every defined intervention.  Results are stored in
#' \code{task$outcome_predictions} as a list-of-lists (outcome x intervention).
#'
#' @param task An \code{enact_task} with models already specified via
#'   \code{\link{add_models}} and interventions defined via
#'   \code{\link{define_interventions}}.
#'
#' @return The modified \code{enact_task} with fitted outcome tasks and
#'   \code{outcome_predictions} populated.
#' @export
fit_outcomes <- function(task) {
  if (!inherits(task, "enact_task")) {
    stop("`task` must be an enact_task object.", call. = FALSE)
  }
  if (is.null(task$outcome_tasks)) {
    stop("No outcome models found. Call add_models() with outcomes() first.", call. = FALSE)
  }
  if (is.null(task$interventions)) {
    stop("No interventions defined. Call define_interventions() first.", call. = FALSE)
  }

  pred_type <- if (task$cv_ensembles) "cv" else "ensemble"
  cv <- task$fold_store$cv
  out_nms <- names(task$outcome_tasks)
  iv_nms <- names(task$interventions)

  # ── Fit each outcome model ──────────────────────────────────────────────
  for (nm in out_nms) {
    cv_mod <- outcome_folds(cv, task, nm)
    original_cv <- task$outcome_tasks[[nm]]$cv
    task$outcome_tasks[[nm]]$cv <- cv_mod
    task$outcome_tasks[[nm]] <- enfold::fit(task$outcome_tasks[[nm]])
    task$outcome_tasks[[nm]]$cv <- original_cv
  }

  # ── Predict under each intervention ─────────────────────────────────────
  task$outcome_predictions <- vector("list", length(out_nms))
  names(task$outcome_predictions) <- out_nms

  for (nm in out_nms) {
    conf_nms <- task$confounder_cols
    if (!is.null(task$adjustment_sets[[nm]])) {
      conf_nms <- task$confounder_cols[task$adjustment_sets[[nm]]]
    }

    mat <- matrix(NA_real_, nrow = task$n_obs, ncol = length(iv_nms))
    colnames(mat) <- iv_nms

    for (iv_nm in iv_nms) {
      conf_block <- task$data_env$data[, conf_nms, drop = FALSE]
      trt_block <- task$intervened_data[[iv_nm]]
      x_pred <- cbind(conf_block, trt_block)

      raw_q <- predict(
        task$outcome_tasks[[nm]],
        newdata = x_pred,
        type = pred_type
      )
      mat[, iv_nm] <- as.numeric(raw_q)
    }

    task$outcome_predictions[[nm]] <- mat
  }

  if (task$verbose) {
    message(sprintf(
      "fit_outcomes: fitted %d outcome task(s). Predictions for %d outcome(s) x %d intervention(s).",
      length(out_nms), length(out_nms), length(iv_nms)
    ))
  }

  task
}


# ══════════════════════════════════════════════════════════════════════════════
# Internal helpers
# ══════════════════════════════════════════════════════════════════════════════

# Compute treatment indicator and extract propensity score for non-MTP.
# For each treatment variable, checks whether the observed value matches
# the intervened value, and extracts P(A_observed | X) from the PS cache.
# Returns the product of per-treatment propensity scores and the
# intersection indicator across all treatment columns.
compute_treatment_indicator <- function(trt_observed, trt_intervened, ps_cache) {
  trt_obs <- as.matrix(trt_observed)
  trt_int <- as.matrix(trt_intervened)
  n <- nrow(trt_int)
  k <- ncol(trt_int)

  ps <- rep(1.0, n)
  indicator <- rep(1L, n)

  for (j in seq_len(k)) {
    col_nm <- colnames(trt_int)[j]
    ps_col <- ps_cache[[col_nm]]
    if (is.null(ps_col)) {
      stop(sprintf(
        "Treatment column '%s' not found in propensity score cache. Available: %s",
        col_nm, paste(names(ps_cache), collapse = ", ")
      ), call. = FALSE)
    }
    # Extract P(A_observed | X) from the PS cache
    if (ncol(ps_col) > 1L) {
      # Multi-column: map observed value to column index (0-indexed → 1-indexed)
      col_idx <- as.integer(trt_obs[, j]) + 1L
      ps_j <- ps_col[cbind(seq_len(n), col_idx)]
    } else {
      ps_j <- as.numeric(ps_col)
    }
    ps <- ps * ps_j

    # Indicator: 1 where observed value == intervened value
    indicator <- indicator & (trt_obs[, j] == trt_int[, j])
  }

  list(ps = ps, indicator = as.integer(indicator))
}
