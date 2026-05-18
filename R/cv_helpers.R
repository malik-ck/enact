# ══════════════════════════════════════════════════════════════════════════════
# add_cv_folds — pipeline wrapper
# ══════════════════════════════════════════════════════════════════════════════

#' Add cross-validation folds to a study task
#'
#' @param task A \code{enfold_task} object.
#' @param inner_cv \code{NULL}, a positive integer (number of V-folds), or a
#'   function with first argument \code{n} returning a list of fold index
#'   sets. Used for inner (ensemble-building) CV. If a function also declares
#'   \code{cluster} or \code{time} as named arguments, these are forwarded
#'   automatically from the task.
#' @param outer_cv As \code{inner_cv}, but for outer (performance-evaluation)
#'   CV.
#' @param ... Additional named arguments forwarded to custom fold functions.
#'   Take precedence over task-derived values.
#' @return The \code{enfold_task} with \code{fold_store} populated.
#' @export
add_cv_folds <- function(task, inner_cv = 5L, outer_cv = 5L, ...) {
  if (!inherits(task, "enfold_task"))
    stop("`task` must be a enfold_task object.")

  # Task-level extras forwarded to custom fold functions
  task_extras <- Filter(Negate(is.null), list(
    cluster = task$cluster,
    time    = task$time
  ))
  dots   <- list(...)
  extras <- c(dots, task_extras[setdiff(names(task_extras), names(dots))])

  cv <- enfold::create_cv_folds(task$n_obs, inner_cv, outer_cv, ...)

  # Store in a reference environment so SL init objects can share without copy
  if (is.null(task$fold_store))
    task$fold_store <- new.env(parent = emptyenv())
  task$fold_store$cv <- cv

  if (task$verbose) {
    perf_n  <- if (!is.null(cv$performance_sets))
      length(cv$performance_sets) else 0L
    build_n <- if (!is.null(cv$build_sets))
      length(cv$build_sets[[1L]]) else 0L
    message(sprintf(
      "CV folds created: %d outer (performance) fold(s), %d inner (build) fold(s).",
      perf_n, build_n
    ))
  }

  task
}

# ══════════════════════════════════════════════════════════════════════════════
# outcome_folds — per-outcome fold derivation
# ══════════════════════════════════════════════════════════════════════════════

#' Derive a enfold_cv with censored observations excluded for a given outcome
#'
#' @param cv A \code{enfold_cv} object.
#' @param task A \code{enfold_task} with censoring indicators populated.
#' @param outcome_name Character. Name of the outcome in \code{task$outcomes}.
#' @return A \code{enfold_cv} with censored observations excluded from both
#'   build and performance folds.
#' @export
outcome_folds <- function(cv, task, outcome_name) {
  if (!inherits(cv, "enfold_cv"))
    stop("`cv` must be a enfold_cv object.")
  if (!inherits(task, "enfold_task"))
    stop("`task` must be a enfold_task object.")
  if (!outcome_name %in% names(task$outcomes))
    stop(sprintf("Outcome '%s' not found in task.", outcome_name))

  cens <- task$censoring[[outcome_name]]
  if (is.null(cens)) return(cv)

  censored_idx <- which(cens == 0L)
  if (length(censored_idx) == 0L) return(cv)

  if (task$verbose) message(sprintf(
    "Outcome '%s': excluding %d censored observation(s) from CV folds.",
    outcome_name, length(censored_idx)
  ))

  exclude(cv, censored_idx)
}