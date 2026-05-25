# ══════════════════════════════════════════════════════════════════════════════
# add_cv_folds — pipeline wrapper
# ══════════════════════════════════════════════════════════════════════════════

#' Add cross-validation folds to a study task
#'
#' @param task A \code{enact_task} object.
#' @param inner_cv \code{NULL}, a positive integer (number of V-folds), or a
#'  function with first argument \code{n} returning a list of fold index
#'  sets. Used for inner (ensemble-building) CV. Respects \code{cluster}
#' if specified in the task.
#' @param outer_cv As \code{inner_cv}, but for outer (performance-evaluation) CV.
#' @return The \code{enact_task} with \code{fold_store} populated.
#' @method add_cv_folds enact_task
#' @importFrom enfold add_cv_folds
#' @export
add_cv_folds.enact_task <- function(task, inner_cv = 5L, outer_cv = 5L) {
  if (!inherits(task, "enact_task")) {
    stop("`task` must be a enact_task object.")
  }

  # Check if there is a cluster variable in the task
  # If yes, use cluster-aware fold creation
  if (!is.null(task$cluster_col)) {
    cluster_ids_vec <- task$data_env$data[[task$cluster_col]]
    inner_cv_fun <- enfold::make_cv_function(
      origami::make_folds,
      V = inner_cv,
      cluster_ids = cluster_ids_vec,
      .subset = "cluster_ids"
    )
    outer_cv_fun <- enfold::make_cv_function(
      origami::make_folds,
      V = outer_cv,
      cluster_ids = cluster_ids_vec,
      .subset = "cluster_ids"
    )

    cv <- enfold::create_cv_folds(
      task$n_obs,
      inner_cv_fun,
      outer_cv_fun
    )
  } else {
    cv <- enfold::create_cv_folds(task$n_obs, inner_cv, outer_cv)
  }

  # Store in a reference environment so SL init objects can share without copy
  if (is.null(task$fold_store)) {
    task$fold_store <- new.env(parent = emptyenv())
  }
  task$fold_store$cv <- cv

  if (task$verbose) {
    perf_n <- if (!is.null(cv$performance_sets)) {
      length(cv$performance_sets)
    } else {
      0L
    }
    build_n <- if (!is.null(cv$build_sets)) {
      length(cv$build_sets[[1L]])
    } else {
      0L
    }
    message(sprintf(
      "CV folds created: %d outer (performance) fold(s), %d inner (build) fold(s).",
      perf_n,
      build_n
    ))
  }
  task$cv_ensembles <- ifelse(is.null(cv$performance_sets), FALSE, TRUE)
  task
}

# ══════════════════════════════════════════════════════════════════════════════
# outcome_folds — per-outcome fold derivation
# ══════════════════════════════════════════════════════════════════════════════

#' Derive an enfold_cv with censored observations excluded for a given outcome
#'
#' @param cv A \code{enfold_cv} object.R
#' @param task A \code{enact_task} with censoring indicators populated.
#' @param outcome_name Character. Name of the outcome in \code{task$outcomes}.
#' @return An \code{enfold_cv} with censored observations excluded from both
#'   build and performance folds.
#' @export
outcome_folds <- function(cv, task, outcome_name) {
  if (!inherits(cv, "enfold_cv")) {
    stop("`cv` must be a enfold_cv object.")
  }
  if (!inherits(task, "enact_task")) {
    stop("`task` must be a enact_task object.")
  }
  if (!outcome_name %in% names(task$outcomes)) {
    stop(sprintf("Outcome '%s' not found in task.", outcome_name))
  }

  cens <- task$censoring[[outcome_name]]
  if (is.null(cens)) {
    return(cv)
  }

  # Censoring: 0 = censored, 1 = observed.
  # May be stored as an integer vector or a data frame / matrix.
  censored_idx <- if (is.null(dim(cens))) {
    which(cens == 0L)
  } else {
    which(apply(cens == 0L, 1L, any))
  }
  if (length(censored_idx) == 0L) {
    return(cv)
  }

  if (task$verbose) {
    message(sprintf(
      "Outcome '%s': excluding %d censored observation(s) from CV folds.",
      outcome_name,
      length(censored_idx)
    ))
  }

  exclude(cv, censored_idx)
}
