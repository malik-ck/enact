
# ══════════════════════════════════════════════════════════════════════════════
# do_tmle — FWB-TMLE targeting step
# ══════════════════════════════════════════════════════════════════════════════

#' Perform FWB-TMLE targeting on fitted nuisance models
#'
#' Applies fractional weighted bootstrap targeting to initial outcome
#' predictions and clever covariates, producing targeted treatment-specific
#' means and contrast estimates with confidence intervals.
#'
#' @param task An \code{enact_task} with \code{clever_covariates} and
#'   \code{outcome_predictions} already populated (via \code{\link{fit_interventions}}
#'   and \code{\link{fit_outcomes}}).
#' @param which Character vector of outcome names to target, or
#'   \code{all_outcomes()} to select all outcomes.
#' @param fluctuation_family A \code{\link[stats]{family}} object used for the
#'   fluctuation model (e.g. \code{gaussian()}, \code{binomial()}).
#' @param n_bstrap Integer. Number of fractional weighted bootstrap iterations.
#' @param ci_method Confidence interval method: \code{"nonparametric"}
#'   (quantile-based, default) or \code{"parametric"} (normal-approximation,
#'   includes SE column for multiple-imputation pooling).
#' @param keep_bootstrap_samples Logical. If \code{TRUE}, stores the raw
#'   bootstrap TSM matrices in \code{task$tmle_bootstrap}.
#'
#' @return The modified \code{enact_task} with \code{tmle_results} populated
#'   (and optionally \code{tmle_bootstrap}).
#' @export
do_tmle <- function(task, which, fluctuation_family, n_bstrap = 2000L,
                    ci_method = c("nonparametric", "parametric"),
                    keep_bootstrap_samples = FALSE) {
  UseMethod("do_tmle")
}

#' @export
do_tmle.enact_task <- function(task, which, fluctuation_family, n_bstrap = 2000L,
                               ci_method = c("nonparametric", "parametric"),
                               keep_bootstrap_samples = FALSE) {

  # ── Validate inputs ─────────────────────────────────────────────────────
  if (is.null(task$clever_covariates)) {
    stop("No clever covariates found. Call fit_interventions() first.", call. = FALSE)
  }
  if (is.null(task$outcome_predictions)) {
    stop("No outcome predictions found. Call fit_outcomes() first.", call. = FALSE)
  }
  if (is.null(task$interventions)) {
    stop("No interventions defined. Call define_interventions() first.", call. = FALSE)
  }
  if (!inherits(fluctuation_family, "family")) {
    stop("`fluctuation_family` must be a family object (e.g. gaussian()).", call. = FALSE)
  }
  n_bstrap <- as.integer(n_bstrap)
  if (length(n_bstrap) != 1L || n_bstrap < 1L) {
    stop("`n_bstrap` must be a positive integer.", call. = FALSE)
  }
  ci_method <- match.arg(ci_method)

  # ── Resolve which outcomes ──────────────────────────────────────────────
  if (is.null(which)) {
    out_nms <- names(task$outcomes)
  } else {
    if (!is.character(which)) {
      stop("`which` must be a character vector of outcome names or all_outcomes().", call. = FALSE)
    }
    bad <- setdiff(which, names(task$outcomes))
    if (length(bad)) {
      stop(sprintf("Outcome(s) not found in task: %s", paste(bad, collapse = ", ")), call. = FALSE)
    }
    out_nms <- which
  }

  iv_nms <- names(task$interventions)
  ref_iv <- task$reference_intervention
  if (is.null(ref_iv) || !ref_iv %in% iv_nms) {
    stop("reference_intervention on task is NULL or not in interventions.", call. = FALSE)
  }
  ref_idx <- match(ref_iv, iv_nms)

  # ── Extract cluster variable if present ─────────────────────────────────
  cluster <- NULL
  if (!is.null(task$cluster_cols)) {
    cluster_data <- task$data_env$data[, task$cluster_cols, drop = FALSE]
    if (ncol(cluster_data) == 1L) {
      cluster <- as.character(cluster_data[[1L]])
    } else {
      cluster <- do.call(paste, c(as.data.frame(cluster_data), sep = "\r"))
    }
  }

  # ── Generate bootstrap weights once ─────────────────────────────────────
  wb <- generate_fwb_weights(task$n_obs, n_bstrap, cluster)

  # ── Process each outcome ────────────────────────────────────────────────
  for (out_nm in out_nms) {
    Q_mat <- task$outcome_predictions[[out_nm]]
    H_mat <- task$clever_covariates[[out_nm]]
    y_raw <- task$outcomes[[out_nm]]

    # Handle matrix outcomes (take first column)
    if (!is.null(dim(y_raw))) y_raw <- y_raw[, 1L]
    y <- as.numeric(y_raw)

    # Determine non-censored indices
    cens <- task$censoring[[out_nm]]
    obs_idx <- if (!is.null(cens)) which(cens == 1L) else seq_along(y)

    # Run FWB targeting
    tsm_boot <- fwb_targeting(
      y = y, Q = Q_mat, H = H_mat,
      fluctuation_family = fluctuation_family,
      wb = wb, obs_idx = obs_idx
    )

    # Get contrasts for this outcome
    contrasts <- task$outcome_contrasts[[out_nm]]
    if (is.null(contrasts)) contrasts <- list(ate())

    # Summarize
    results_df <- summarize_fwb(
      tsm_matrix = tsm_boot,
      contrasts = contrasts,
      ref_idx = ref_idx,
      intervention_labels = iv_nms,
      ci_method = ci_method
    )

    task$tmle_results[[out_nm]] <- results_df

    if (keep_bootstrap_samples) {
      task$tmle_bootstrap[[out_nm]] <- tsm_boot
    }
  }

  if (task$verbose) {
    message(sprintf(
      "do_tmle: targeted %d outcome(s) with %d bootstrap iteration(s), ci_method = '%s'.",
      length(out_nms), n_bstrap, ci_method
    ))
  }

  task
}


#' Select all outcomes for do_tmle
#'
#' Convenience function for the \code{which} argument of \code{\link{do_tmle}}.
#' Returns \code{NULL}, which is interpreted as "all outcomes".
#'
#' @return \code{NULL}
#' @export
all_outcomes <- function() NULL


# ══════════════════════════════════════════════════════════════════════════════
# Internal helpers
# ══════════════════════════════════════════════════════════════════════════════

# Generate FWB weights: n_bstrap × n matrix, each row sums to 1.
# If cluster is non-NULL, generates per-cluster Dirichlet weights and expands.
generate_fwb_weights <- function(n, n_bstrap, cluster = NULL) {
  if (is.null(cluster)) {
    w <- matrix(rgamma(n_bstrap * n, 1), nrow = n_bstrap, ncol = n)
    w <- w / rowSums(w)
  } else {
    cluster_ids <- unique(cluster)
    n_clusters <- length(cluster_ids)
    cw <- matrix(rgamma(n_bstrap * n_clusters, 1), nrow = n_bstrap, ncol = n_clusters)
    cw <- cw / rowSums(cw)
    cluster_idx <- match(cluster, cluster_ids)
    w <- cw[, cluster_idx, drop = FALSE]
    # Normalize to sum to 1 per row (observation-level weights)
    w <- w / rowSums(w)
  }
  w
}


# Core FWB targeting loop.
# Returns a matrix of TSMs: n_bstrap × n_interventions.
fwb_targeting <- function(y, Q, H, fluctuation_family, wb, obs_idx) {
  n_bstrap <- nrow(wb)
  n_iv <- ncol(Q)
  iv_nms <- colnames(Q)
  n <- length(y)

  linkfun <- fluctuation_family$linkfun
  linkinv <- fluctuation_family$linkinv

  # Pre-transform Q to link scale for all observations
  Q_link <- matrix(NA_real_, nrow = n, ncol = n_iv)
  for (j in seq_len(n_iv)) {
    Q_link[, j] <- linkfun(Q[, j])
  }

  tsm_mat <- matrix(NA_real_, nrow = n_bstrap, ncol = n_iv)
  colnames(tsm_mat) <- iv_nms

  for (b in seq_len(n_bstrap)) {
    w_b <- wb[b, ]

    for (j in seq_len(n_iv)) {
      # Fit fluctuation on non-censored obs only
      y_obs <- y[obs_idx]
      offset_obs <- Q_link[obs_idx, j]
      h_obs <- H[obs_idx, j]
      weights_obs <- h_obs * w_b[obs_idx]

      fit <- tryCatch(
        stats::glm(
          y_obs ~ 1 + offset(offset_obs),
          weights = weights_obs,
          family = fluctuation_family,
          control = stats::glm.control(maxit = 50L)
        ),
        error = function(e) NULL
      )

      if (is.null(fit)) {
        # Fallback: use untargeted predictions
        tsm_mat[b, j] <- stats::weighted.mean(Q[, j], w_b)
      } else {
        # Apply fluctuation coefficient to ALL observations
        epsilon <- as.numeric(stats::coef(fit))
        Q_targeted <- linkinv(Q_link[, j] + epsilon)
        tsm_mat[b, j] <- stats::weighted.mean(Q_targeted, w_b)
      }
    }
  }

  tsm_mat
}


# Summarize bootstrap TSM matrix into a results data.frame.
summarize_fwb <- function(tsm_matrix, contrasts, ref_idx, intervention_labels,
                          ci_method) {
  n_iv <- length(intervention_labels)

  # Build TSM rows
  tsm_labels <- paste0("TSM - ", intervention_labels)
  tsm_estimates <- colMeans(tsm_matrix)
  tsm_se <- apply(tsm_matrix, 2L, stats::sd)

  # Build contrast rows
  contrast_labels <- character(0)
  contrast_estimates <- numeric(0)
  contrast_se <- numeric(0)
  contrast_boot <- matrix(nrow = nrow(tsm_matrix), ncol = 0)

  for (fn in contrasts) {
    lab <- attr(fn, "label")
    if (is.null(lab)) lab <- "Custom contrast"

    ref_tsm <- tsm_matrix[, ref_idx, drop = TRUE]
    for (j in seq_len(n_iv)) {
      if (j == ref_idx) next
      boot_vals <- fn(reference = ref_tsm, treatment = tsm_matrix[, j, drop = TRUE])
      contrast_labels <- c(contrast_labels, paste0(lab, " (", intervention_labels[j], " vs ", intervention_labels[ref_idx], ")"))
      contrast_estimates <- c(contrast_estimates, mean(boot_vals, na.rm = TRUE))
      contrast_se <- c(contrast_se, stats::sd(boot_vals, na.rm = TRUE))
      contrast_boot <- cbind(contrast_boot, boot_vals)
    }
  }

  # Combine
  all_labels <- c(tsm_labels, contrast_labels)
  all_estimates <- c(tsm_estimates, contrast_estimates)
  all_se <- c(tsm_se, contrast_se)
  all_boot <- cbind(tsm_matrix, contrast_boot)

  if (ci_method == "nonparametric") {
    lower <- apply(all_boot, 2L, stats::quantile, probs = 0.025, na.rm = TRUE)
    upper <- apply(all_boot, 2L, stats::quantile, probs = 0.975, na.rm = TRUE)
  } else {
    lower <- all_estimates - 1.96 * all_se
    upper <- all_estimates + 1.96 * all_se
  }

  # Two-sided bootstrap p-value against 0.
  p_value <- apply(all_boot, 2L, function(col) {
    col <- col[!is.na(col)]
    if (!length(col)) return(NA_real_)
    pmin(2 * min(mean(col <= 0), mean(col >= 0)), 1)
  })

  data.frame(
    Contrast = all_labels,
    Estimate = all_estimates,
    SE = all_se,
    Lower_CI = lower,
    Upper_CI = upper,
    P_value = p_value,
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
