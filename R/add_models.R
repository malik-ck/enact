# ══════════════════════════════════════════════════════════════════════════════
# Selector constructors
# ══════════════════════════════════════════════════════════════════════════════

#' Specify treatment nuisance models
#'
#' Selector for \code{\link{add_models}}.  Targets treatment mechanism models.
#'
#' @param which Treatment identifiers to target.  Either \code{NULL} (the
#'   default, targets all treatments), a character vector of treatment names,
#'   or a tidyselect expression such as \code{starts_with("A")} that is
#'   evaluated against the treatment names at \code{\link{add_models}} time.
#' @param learners A single \code{enfold_learner} (or pipeline/grid/list), or a
#'   list of such objects.  Required.
#' @param metalearner A single \code{enfold_learner} created by a metalearner
#'   constructor (e.g. \code{mtl_superlearner()}).  Required when
#'   \code{learners} is a list with multiple elements.
#'
#' @return An object of class \code{enact_selector_treatments}.
#' @export
treatments <- function(which = NULL, learners, metalearner = NULL) {
  validate_learners(learners, metalearner, "treatments")
  structure(
    list(
      which_quo = rlang::enquo(which),
      learners = learners,
      metalearner = metalearner
    ),
    class = "enact_selector_treatments"
  )
}

#' Specify outcome nuisance models
#'
#' Selector for \code{\link{add_models}}.  Targets outcome regression models.
#'
#' @param which Outcome identifiers to target.  Either \code{NULL} (the
#'   default, targets all outcomes), a character vector of outcome labels,
#'   or a tidyselect expression such as \code{starts_with("Y")} that is
#'   evaluated against the outcome labels at \code{\link{add_models}} time.
#' @param learners A single \code{enfold_learner} (or pipeline/grid/list), or a
#'   list of such objects.  Required.
#' @param metalearner A single \code{enfold_learner} created by a metalearner
#'   constructor (e.g. \code{mtl_superlearner()}).  Required when
#'   \code{learners} is a list with multiple elements.
#'
#' @return An object of class \code{enact_selector_outcomes}.
#' @export
outcomes <- function(which = NULL, learners, metalearner = NULL) {
  validate_learners(learners, metalearner, "outcomes")
  structure(
    list(
      which_quo = rlang::enquo(which),
      learners = learners,
      metalearner = metalearner
    ),
    class = "enact_selector_outcomes"
  )
}

#' Specify censoring mechanism models
#'
#' Selector for \code{\link{add_models}}.  Targets censoring mechanism models
#' for all outcomes that have censoring indicators.  The same learner
#' specification is used for each censoring mechanism.
#'
#' @param learners A single \code{enfold_learner} (or pipeline/grid/list), or a
#'   list of such objects.  Required.
#' @param metalearner A single \code{enfold_learner} created by a metalearner
#'   constructor (e.g. \code{mtl_superlearner()}).  Required when
#'   \code{learners} is a list with multiple elements.
#'
#' @return An object of class \code{enact_selector_censoring}.
#' @export
censoring <- function(learners, metalearner = NULL) {
  validate_learners(learners, metalearner, "censoring")
  structure(
    list(learners = learners, metalearner = metalearner),
    class = "enact_selector_censoring"
  )
}

#' Specify MTP density ratio models
#'
#' Selector for \code{\link{add_models}}.  Targets the 2N density ratio
#' classifier for Modified Treatment Policies.
#'
#' @param learners A single \code{enfold_learner} (or pipeline/grid/list), or a
#'   list of such objects.  Required.
#' @param metalearner A single \code{enfold_learner} created by a metalearner
#'   constructor (e.g. \code{mtl_superlearner()}).  Required when
#'   \code{learners} is a list with multiple elements.
#'
#' @return An object of class \code{enact_selector_mtp}.
#' @export
mtp <- function(learners, metalearner = NULL) {
  validate_learners(learners, metalearner, "mtp")
  structure(
    list(learners = learners, metalearner = metalearner),
    class = "enact_selector_mtp"
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# add_models() — S3 generic + enact_task method
# ══════════════════════════════════════════════════════════════════════════════

#' Add nuisance models to a study task
#'
#' Specifies the learners and metalearner for treatment mechanisms, outcome
#' regressions, censoring mechanisms, and/or MTP density ratio models.  Must be
#' called after \code{\link{add_treatment}} / \code{\link{add_outcome}} and
#' \code{\link{add_cv_folds}}.  Can only be called once per task.
#'
#' @param task A \code{enact_task} object.
#' @param \dots Selector objects created by \code{\link{treatments}},
#'   \code{\link{outcomes}}, \code{\link{censoring}}, and/or \code{\link{mtp}}.
#' @return The modified \code{enact_task}.
#' @export
add_models <- function(task, ...) {
  UseMethod("add_models")
}


#' @export
add_models.enact_task <- function(task, ...) {
  dots <- list(...)

  if (length(dots) == 0L) {
    stop("At least one selector (treatments(), outcomes(), censoring(), mtp()) must be provided.", call. = FALSE)
  }

  # ── Validate selectors ────────────────────────────────────────────────────
  valid_classes <- c(
    "enact_selector_treatments",
    "enact_selector_outcomes",
    "enact_selector_censoring",
    "enact_selector_mtp"
  )
  for (i in seq_along(dots)) {
    cls <- class(dots[[i]])[1L]
    if (!cls %in% valid_classes) {
      stop(sprintf(
        "Argument %d is not a valid selector (treatments(), outcomes(), censoring(), mtp()). Got class '%s'.",
        i, cls
      ), call. = FALSE)
    }
  }

  # At most one selector per type
  classes <- vapply(dots, function(x) class(x)[1L], character(1L))
  dupes <- unique(classes[duplicated(classes)])
  if (length(dupes)) {
    stop(sprintf(
      "Duplicate selector type(s): %s. Each selector type can appear at most once.",
      paste(dupes, collapse = ", ")
    ), call. = FALSE)
  }

  # ── Validate task state ───────────────────────────────────────────────────
  if (is.null(task$treatment_meta) && is.null(task$outcomes)) {
    stop("No treatments or outcomes found. Call add_treatment() / add_outcome() before add_models().", call. = FALSE)
  }

  if (!is.null(task$treatment_tasks) || !is.null(task$outcome_tasks) ||
      !is.null(task$censoring_tasks) || !is.null(task$mtp_tasks)) {
    stop("Models have already been added to this task. add_models() can only be called once.", call. = FALSE)
  }

  # ── Validate CV folds ─────────────────────────────────────────────────────
  if (is.null(task$fold_store) || is.null(task$fold_store$cv)) {
    stop("No CV folds found. Call add_cv_folds() before add_models().", call. = FALSE)
  }

  cv <- task$fold_store$cv
  has_outer <- !is.null(cv$performance_sets)

  # Check outer CV requirement for each selector
  for (sel in dots) {
    is_single <- inherits(sel$learners, "enfold_learner") ||
      inherits(sel$learners, "enfold_pipeline") ||
      inherits(sel$learners, "enfold_grid") ||
      inherits(sel$learners, "enfold_list")
    needs_outer <- !is.null(sel$metalearner) ||
      (!is_single && is.list(sel$learners) && length(sel$learners) > 1L)
    if (needs_outer && !has_outer) {
      stop("Multiple learners or a metalearner require outer CV. Set outer_cv in add_cv_folds().", call. = FALSE)
    }
  }

  # ── Shared helpers ────────────────────────────────────────────────────────
  data_env <- task$data_env
  if (is.null(data_env) || !exists("data", envir = data_env)) {
    stop("Task does not contain stored data.", call. = FALSE)
  }
  data <- data_env$data
  is_df <- is.data.frame(data)
  cv <- task$fold_store$cv

  wrap_learners <- function(x) {
    if (inherits(x, "enfold_learner") || inherits(x, "enfold_pipeline") ||
        inherits(x, "enfold_grid") || inherits(x, "enfold_list")) {
      list(x)
    } else {
      x
    }
  }

  build_enfold_task <- function(y, cols, learners, metalearner) {
    etask <- enfold::initialize_enfold(data, y, cols = cols)
    lrns <- wrap_learners(learners)
    for (lrn in lrns) {
      etask <- enfold::add_learners(etask, lrn)
    }
    if (!is.null(metalearner)) {
      etask <- enfold::add_metalearners(etask, metalearner)
    }
    etask <- enfold::add_cv_folds(etask, cv = cv)
    etask
  }

  # ── Process each selector ─────────────────────────────────────────────────
  for (sel in dots) {
    sel_class <- class(sel)[1L]

    if (sel_class == "enact_selector_treatments") {
      task <- process_treatments_selector(task, sel, data, is_df, build_enfold_task)
    } else if (sel_class == "enact_selector_outcomes") {
      task <- process_outcomes_selector(task, sel, data, is_df, build_enfold_task)
    } else if (sel_class == "enact_selector_censoring") {
      task <- process_censoring_selector(task, sel, data, is_df, build_enfold_task)
    } else if (sel_class == "enact_selector_mtp") {
      task <- process_mtp_selector(task, sel, data, is_df, build_enfold_task)
    }
  }

  task
}


# ══════════════════════════════════════════════════════════════════════════════
# Selector processors
# ══════════════════════════════════════════════════════════════════════════════

process_treatments_selector <- function(task, sel, data, is_df,
                                        build_enfold_task) {
  if (is.null(task$treatment_meta)) {
    stop("No treatments found in task. Call add_treatment() first.", call. = FALSE)
  }

  all_trt_names <- names(task$treatment_meta)
  target_names <- resolve_which(sel$which_quo, all_trt_names, "treatment")

  for (nm in target_names) {
    col_nms <- vapply(
      task$treatment_meta[[nm]],
      function(m) m$label_info,
      character(1L)
    )
    y_vec <- if (length(col_nms) == 1L) {
      data[, col_nms[1L]]
    } else {
      data[, col_nms, drop = FALSE]
    }

    etask <- build_enfold_task(y_vec, task$confounder_cols, sel$learners, sel$metalearner)
    task$treatment_tasks[[nm]] <- etask
  }

  if (task$verbose) {
    message(sprintf(
      "add_models: treatment models added for %s.",
      paste(target_names, collapse = ", ")
    ))
  }

  task
}

process_outcomes_selector <- function(task, sel, data, is_df,
                                      build_enfold_task) {
  if (is.null(task$outcomes)) {
    stop("No outcomes found in task. Call add_outcome() first.", call. = FALSE)
  }

  all_out_names <- names(task$outcomes)
  target_names <- resolve_which(sel$which_quo, all_out_names, "outcome")

  for (nm in target_names) {
    raw_y <- task$outcomes[[nm]]
    y_for_enfold <- if (is.data.frame(raw_y) && ncol(raw_y) == 1L) raw_y[[1L]] else raw_y

    conf_cols <- task$confounder_cols
    if (!is.null(task$adjustment_sets[[nm]])) {
      conf_cols <- conf_cols[task$adjustment_sets[[nm]]]
    }

    etask <- build_enfold_task(y_for_enfold, conf_cols, sel$learners, sel$metalearner)
    task$outcome_tasks[[nm]] <- etask
  }

  if (task$verbose) {
    message(sprintf(
      "add_models: outcome models added for %s.",
      paste(target_names, collapse = ", ")
    ))
  }

  task
}

process_censoring_selector <- function(task, sel, data, is_df,
                                       build_enfold_task) {
  if (is.null(task$censoring)) {
    stop("No censoring indicators found. Ensure outcomes have censoring defined.", call. = FALSE)
  }

  cens_names <- names(task$censoring)[
    vapply(task$censoring, Negate(is.null), logical(1L))
  ]

  if (length(cens_names) == 0L) {
    warning("No outcomes have censoring indicators. Censoring models not added.", call. = FALSE)
    return(task)
  }

  for (nm in cens_names) {
    cens_vec <- task$censoring[[nm]]
    attr(cens_vec, "cens_col") <- NULL

    etask <- build_enfold_task(cens_vec, task$confounder_cols, sel$learners, sel$metalearner)
    task$censoring_tasks[[nm]] <- etask
  }

  if (task$verbose) {
    message(sprintf(
      "add_models: censoring models added for %s.",
      paste(cens_names, collapse = ", ")
    ))
  }

  task
}

process_mtp_selector <- function(task, sel, data, is_df,
                                 build_enfold_task) {
  if (is.null(task$interventions)) {
    stop("No interventions found. Call define_interventions() before add_models() with mtp().", call. = FALSE)
  }

  mtp_nms <- names(task$interventions)[
    vapply(task$interventions, function(iv) iv$mtp, logical(1L))
  ]
  if (length(mtp_nms) == 0L) {
    stop("No MTP interventions found. mtp() requires at least one intervention with mtp = TRUE.", call. = FALSE)
  }

  # cols: confounders (L) + treatment columns (A) + censoring columns (C)
  trt_col_nms <- unlist(lapply(task$treatment_meta, function(m) {
    vapply(m, function(mi) mi$label_info, character(1L))
  }))

  cens_col_nms <- if (!is.null(task$censoring)) {
    non_null <- task$censoring[vapply(task$censoring, Negate(is.null), logical(1L))]
    unique(unlist(lapply(non_null, function(v) attr(v, "cens_col")), use.names = FALSE))
  } else {
    character(0)
  }

  mtp_cols <- c(task$confounder_cols, trt_col_nms, cens_col_nms)
  y_const <- rep(0L, nrow(data))

  # One enfold task per MTP intervention (same master x/y, reused)
  for (nm in mtp_nms) {
    etask <- build_enfold_task(y_const, mtp_cols, sel$learners, sel$metalearner)
    task$mtp_tasks[[nm]] <- etask
  }

  if (task$verbose) {
    message(sprintf(
      "add_models: MTP density ratio tasks created for %s.",
      paste(mtp_nms, collapse = ", ")
    ))
  }

  task
}


# ══════════════════════════════════════════════════════════════════════════════
# Helpers
# ══════════════════════════════════════════════════════════════════════════════

# Resolve a `which` quosure against the available identifier names.
# NULL quo -> all; character -> validated against available; otherwise
# tidyselect against a zero-row proxy whose columns are the available names.
resolve_which <- function(quo, available, type_label) {
  if (rlang::quo_is_null(quo)) return(available)

  val <- tryCatch(rlang::eval_tidy(quo), error = function(e) NULL)
  if (is.character(val)) {
    bad <- setdiff(val, available)
    if (length(bad)) {
      stop(sprintf(
        "%s(s) not found in task: %s. Available: %s",
        capitalize(type_label),
        paste(bad, collapse = ", "),
        paste(available, collapse = ", ")
      ), call. = FALSE)
    }
    return(val)
  }

  proxy <- as.data.frame(
    matrix(numeric(0L), nrow = 0L, ncol = length(available)),
    stringsAsFactors = FALSE
  )
  names(proxy) <- available
  idx <- tryCatch(
    tidyselect::eval_select(quo, proxy),
    error = function(e) {
      stop(sprintf(
        "Cannot resolve `which` for %s selector. Use NULL, a character vector of identifiers, or a tidyselect expression.\nUnderlying error: %s",
        type_label, conditionMessage(e)
      ), call. = FALSE)
    }
  )
  available[as.integer(idx)]
}

validate_learners <- function(learners, metalearner, label) {
  if (missing(learners) || is.null(learners)) {
    stop(sprintf("`%s`: `learners` is required.", label), call. = FALSE)
  }
  is_single <- inherits(learners, "enfold_learner") ||
    inherits(learners, "enfold_pipeline") ||
    inherits(learners, "enfold_grid") ||
    inherits(learners, "enfold_list")
  if (!is_single && is.list(learners) && length(learners) > 1L && is.null(metalearner)) {
    stop(sprintf(
      "`%s`: `metalearner` is required when `learners` is a list with multiple elements.",
      label
    ), call. = FALSE)
  }
}

capitalize <- function(x) {
  paste0(toupper(substring(x, 1L, 1L)), substring(x, 2L))
}
