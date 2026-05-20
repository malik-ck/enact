# ══════════════════════════════════════════════════════════════════════════════
# treatment() constructor
# ══════════════════════════════════════════════════════════════════════════════

#' Define a treatment variable
#'
#' Constructs an \code{enact_treatment} object that describes a single
#' treatment.  The column selection is captured unevaluated (via tidyselect /
#' quosure semantics) and resolved later by \code{\link{add_outcomes}}.
#'
#' @param which <[`tidyselect`][tidyselect::language]> Treatment column(s) in
#'   the study data.  Also accepts a character vector of column names or an
#'   integer index vector.
#' @param label Optional character string.  Display label for this treatment.
#' @param learners A list of \code{enfold_learner} objects (e.g.
#'   \code{lrn_glm()}), or a single learner.  Used to build the SuperLearner
#'   ensemble for the treatment mechanism model.
#' @param metalearner An \code{enfold_learner} created by a metalearner
#'   constructor (e.g. \code{mtl_superlearner()}).  Determines how base
#'   learner predictions are combined.
#'
#' @return An S3 object of class \code{enact_treatment}.
#' @export
treatment <- function(which, learners, metalearner, label = NULL) {
  which_quo <- rlang::enquo(which)

  if (!is.null(label) && (!is.character(label) || length(label) != 1L)) {
    stop("`label` must be a single character string or NULL.", call. = FALSE)
  }
  # Wrap single learner/pipeline objects in a list. We can't rely on
  # is.list() because enfold_learner objects are internally lists.
  if (!is.null(learners) && (inherits(learners, "enfold_learner") ||
                              inherits(learners, "enfold_pipeline"))) {
    learners <- list(learners)
  }

  structure(
    list(
      which       = which_quo,
      label       = label,
      learners    = learners,
      metalearner = metalearner
    ),
    class = "enact_treatment"
  )
}

#' @export
print.enact_treatment <- function(x, ...) {
  lab <- if (!is.null(x$label)) x$label else "(unlabelled)"
  which_expr <- rlang::as_label(x$which)
  n_learners <- if (!is.null(x$learners)) length(x$learners) else 0L
  cat(sprintf("enact_treatment | %s | which: %s | %d learner(s)\n",
              lab, which_expr, n_learners))
  invisible(x)
}


# ══════════════════════════════════════════════════════════════════════════════
# outcome() constructor
# ══════════════════════════════════════════════════════════════════════════════

#' Define an outcome variable
#'
#' Constructs an \code{enact_outcome} object that describes a single study
#' outcome.  Selections are captured unevaluated (via tidyselect / quosure
#' semantics) and resolved later by \code{\link{add_outcomes}}.
#'
#' @param which <[`tidyselect`][tidyselect::language]> Outcome column(s) in the
#'   study data.  Also accepts a character vector of column names or an integer
#'   index vector.
#' @param censoring <[`tidyselect`][tidyselect::language]> Optional censoring
#'   indicator column in the study data.  Values should be \code{0} (censored)
#'   or \code{1} (observed).  Also accepts a character column name or integer
#'   column index (must resolve to a single column).
#'   When \code{NULL} (default), censoring is auto-detected from \code{NA}
#'   values in the outcome.
#' @param label Optional character string.  Display label for this outcome.
#' @param adjustment_set Character vector of confounder column names or integer
#'   indices into the confounder block.  \code{NULL} (default) inherits the full
#'   global confounder set.
#' @param learners A list of \code{enfold_learner} objects (e.g.
#'   \code{lrn_glm()}), or a single learner.  Used to build the SuperLearner
#'   ensemble for the outcome regression model.  Required.
#' @param metalearner An \code{enfold_learner} created by a metalearner
#'   constructor (e.g. \code{mtl_superlearner()}).  Determines how base
#'   learner predictions are combined.  Required.
#'
#' @return An S3 object of class \code{enact_outcome}.
#' @export
outcome <- function(
  which,
  learners,
  metalearner,
  censoring = NULL,
  label = NULL,
  adjustment_set = NULL
) {
  which_quo    <- rlang::enquo(which)
  censoring_quo <- rlang::enquo(censoring)

  if (!is.null(label) && (!is.character(label) || length(label) != 1L)) {
    stop("`label` must be a single character string or NULL.", call. = FALSE)
  }
  # Wrap single learner/pipeline objects in a list.
  if (!is.null(learners) && (inherits(learners, "enfold_learner") ||
                              inherits(learners, "enfold_pipeline"))) {
    learners <- list(learners)
  }

  structure(
    list(
      which          = which_quo,
      censoring      = censoring_quo,
      label          = label,
      adjustment_set = adjustment_set,
      learners       = learners,
      metalearner    = metalearner
    ),
    class = "enact_outcome"
  )
}

#' @export
print.enact_outcome <- function(x, ...) {
  lab <- if (!is.null(x$label)) x$label else "(unlabelled)"
  which_expr <- rlang::as_label(x$which)
  cens_expr  <- if (rlang::quo_is_null(x$censoring)) {
    "auto-detect"
  } else {
    rlang::as_label(x$censoring)
  }
  n_learners <- if (!is.null(x$learners)) length(x$learners) else 0L
  cat(sprintf("enact_outcome | %s | which: %s | censoring: %s | %d learner(s)\n",
              lab, which_expr, cens_expr, n_learners))
  invisible(x)
}


# ══════════════════════════════════════════════════════════════════════════════
# censoring_learner() constructor
# ══════════════════════════════════════════════════════════════════════════════

#' Create a censoring-mechanism learner
#'
#' Wraps one or more \code{enfold_learner} objects into a metalearner ensemble
#' that models the censoring indicator (probability of being observed) for an
#' outcome.  This is conceptually identical to a treatment model, but applied
#' to the censoring variable.
#'
#' @param name Character.  Name for this censoring learner.
#' @param \dots One or more \code{enfold_learner} objects.  Will be stored as
#'   a list (even if a single learner is passed).
#' @param metalearner An \code{enfold_learner} from a metalearner constructor
#'   (e.g. \code{mtl_superlearner()}).  Determines how the base learner
#'   predictions are combined.  Defaults to
#'   \code{mtl_superlearner("sl")} with \code{loss_logistic()}.
#' @param loss_fun An \code{mtl_loss} object used for the metalearner.
#'   Defaults to \code{loss_logistic()}.
#'
#' @return An S3 object of class \code{enact_censoring_learner} (inherits
#'   \code{enfold_learner}).
#' @export
censoring_learner <- function(
  name,
  ...,
  metalearner = NULL,
  loss_fun = enfold::loss_logistic()
) {
  if (!is.character(name) || length(name) != 1L) {
    stop("`name` must be a single character string.", call. = FALSE)
  }

  learners <- list(...)
  if (length(learners) == 0L) {
    stop("At least one enfold_learner must be provided via `...`.", call. = FALSE)
  }

  if (is.null(metalearner)) {
    metalearner <- enfold::mtl_superlearner("sl", loss_fun = loss_fun)
  }

  structure(
    list(
      name        = name,
      learners    = learners,
      metalearner = metalearner,
      loss_fun    = loss_fun
    ),
    class = c("enact_censoring_learner", "enfold_learner")
  )
}

#' @export
print.enact_censoring_learner <- function(x, ...) {
  lrn_names <- vapply(x$learners, function(l) l$name %||% "?", character(1L))
  cat(sprintf("enact_censoring_learner | %s | %s\n",
              x$name, paste(lrn_names, collapse = ", ")))
  invisible(x)
}


# ══════════════════════════════════════════════════════════════════════════════
# add_outcomes() — S3 generic + nana_task method
# ══════════════════════════════════════════════════════════════════════════════

#' Add components to a study task
#'
#' Generic function for attaching treatments, outcomes, and censoring learners
#' to a \code{nana_task}.  Methods dispatch on the class of the objects passed
#' in \code{...}.
#'
#' @param task A study task object (e.g. \code{nana_task}).
#' @param \dots Named objects to add.  Each argument name becomes the
#'   treatment or outcome name in the task.
#' @return The modified task.
#' @export
add_outcomes <- function(task, ...) {
  UseMethod("add_outcomes")
}


#' Add treatments and/or outcomes to a nana_task
#'
#' Each argument must be an \code{enact_treatment} or \code{enact_outcome}
#' object.  Argument names become the treatment or outcome names in the task.
#'
#' @param task A \code{nana_task} object.
#' @param \dots Named \code{\link{treatment}} or \code{\link{outcome}} objects.
#' @return The modified \code{nana_task}.
#'
#' @details
#' \strong{Treatments} are validated (must be numeric, binary coding checked),
#' extracted from the stored data, and an \code{enfold} sub-task is created via
#' \code{initialize_enfold()}.
#'
#' \strong{Outcomes} are extracted and an \code{enfold} sub-task is created.
#' Censoring is handled as follows:
#' \enumerate{
#'   \item If the user specified censoring columns, they are extracted from the
#'     data.  Any rows where the outcome has \code{NA} but every user-supplied
#'     censoring indicator is \code{1} (observed) are flagged: an auto-generated
#'     column named \code{".auto"} is appended that captures these residual
#'     \code{NA}s.
#'   \item If no censoring was specified, pure auto-detection is applied: rows
#'     with any \code{NA} in the outcome are coded \code{0} (censored), all
#'     others \code{1}.  The result is stored as a single-column data frame
#'     with column name \code{".auto"}.
#'   \item If no \code{NA}s are found and no user-supplied censoring was given,
#'     censoring is \code{NULL} for that outcome.
#' }
#' If a \code{learners} list is provided in the outcome, an \code{enfold}
#' sub-task is also created for the outcome regression.
#'
#' @export
add_outcomes.nana_task <- function(task, ...) {
  # ── Capture and classify dots ─────────────────────────────────────────────
  dots <- list(...)
  if (length(dots) == 0L) {
    stop("At least one treatment() or outcome() object must be provided.", call. = FALSE)
  }
  if (is.null(names(dots)) || any(names(dots) == "")) {
    stop("All arguments must be named (e.g. add_outcomes(task, A = treatment(...))).", call. = FALSE)
  }

  is_trt <- vapply(dots, inherits, logical(1L), "enact_treatment")
  is_out <- vapply(dots, inherits, logical(1L), "enact_outcome")
  bad <- !is_trt & !is_out
  if (any(bad)) {
    stop(sprintf(
      "The following arguments are not treatment() or outcome() objects: %s",
      paste(names(dots)[bad], collapse = ", ")
    ), call. = FALSE)
  }

  treatments <- dots[is_trt]
  outcomes    <- dots[is_out]

  # ── Duplicate-name check ──────────────────────────────────────────────────
  if (!is.null(task$treatment_meta)) {
    dupes <- intersect(names(treatments), names(task$treatment_meta))
    if (length(dupes)) {
      stop(sprintf(
        "Treatment name(s) already exist in task: %s",
        paste(dupes, collapse = ", ")
      ), call. = FALSE)
    }
  }
  if (!is.null(task$outcomes)) {
    dupes <- intersect(names(outcomes), names(task$outcomes))
    if (length(dupes)) {
      stop(sprintf(
        "Outcome name(s) already exist in task: %s",
        paste(dupes, collapse = ", ")
      ), call. = FALSE)
    }
  }

  # ── Shared helpers ────────────────────────────────────────────────────────
  data_env <- task$data_env
  if (is.null(data_env) || !exists("data", envir = data_env)) {
    stop("Task does not contain stored data.  Re-run initiate_study().", call. = FALSE)
  }
  data <- data_env$data
  is_df <- is.data.frame(data)
  n_col <- ncol(data)

  if (is.null(colnames(data))) {
    colnames(data) <- paste0("V", seq_len(n_col))
  }

  df_proxy <- if (is_df) {
    data[0L, , drop = FALSE]
  } else {
    as.data.frame(data[0L, , drop = FALSE])
  }

  resolve_cols <- function(quo, arg_name) {
    tryCatch(
      tidyselect::eval_select(quo, df_proxy),
      error = function(ts_err) {
        val <- tryCatch(
          rlang::eval_tidy(quo),
          error = function(e) {
            stop(sprintf(
              "Cannot resolve `%s`. Use tidyselect syntax, a character vector of column names, or an integer index vector.\nUnderlying error: %s",
              arg_name,
              conditionMessage(ts_err)
            ), call. = FALSE)
          }
        )
        if (is.null(val)) return(NULL)
        if (is.character(val)) {
          idx <- match(val, colnames(data))
          bad <- val[is.na(idx)]
          if (length(bad)) {
            stop(sprintf(
              "In `%s`: column(s) not found in data: %s",
              arg_name,
              paste(bad, collapse = ", ")
            ), call. = FALSE)
          }
          setNames(as.integer(idx), val)
        } else if (is.numeric(val) || is.integer(val)) {
          idx <- as.integer(val)
          oob <- idx[idx < 1L | idx > n_col]
          if (length(oob)) {
            stop(sprintf(
              "In `%s`: index/indices out of range: %s",
              arg_name,
              paste(oob, collapse = ", ")
            ), call. = FALSE)
          }
          setNames(idx, colnames(data)[idx])
        } else {
          stop(sprintf(
            "In `%s`: selection must resolve to column names (character) or indices (integer).",
            arg_name
          ), call. = FALSE)
        }
      }
    )
  }

  extract_block <- function(idx, as_vec = FALSE) {
    if (as_vec && length(idx) == 1L) {
      return(if (is_df) data[[idx[[1L]]]] else as.vector(data[, idx[[1L]]]))
    }
    block <- data[, idx, drop = FALSE]
    if (is_df) as.data.frame(block) else as.matrix(block)
  }

  # ── Process treatments ────────────────────────────────────────────────────
  new_treatment_meta    <- list()
  new_treatment_labels  <- character(0)
  new_treatment_tasks   <- list()
  new_treatment_specs   <- list()

  for (nm in names(treatments)) {
    trt <- treatments[[nm]]

    which_idx <- resolve_cols(trt$which, paste0(nm, "$which"))
    if (is.null(which_idx) || length(which_idx) == 0L) {
      stop(sprintf("treatment `%s`: `which` resolved to no columns.", nm), call. = FALSE)
    }

    treat_block <- extract_block(which_idx)
    treat_col_names <- names(which_idx)

    # Validate each treatment column
    classify_treatment <- function(col, col_name) {
      if (!is.numeric(col) && !is.integer(col)) {
        stop(sprintf(
          "Treatment '%s': must be numeric or integer; got class '%s'.",
          col_name, class(col)[1L]
        ), call. = FALSE)
      }
      uvals <- sort(unique(col[!is.na(col)]))
      if (length(uvals) <= 1L) {
        warning(sprintf(
          "Treatment '%s': column is constant (all values = %s).",
          col_name,
          if (length(uvals) == 0L) "NA" else as.character(uvals)
        ), call. = FALSE)
        list(type = "binary", label_info = col_name)
      } else if (length(uvals) == 2L) {
        if (!identical(uvals, c(0, 1)) && !identical(uvals, c(0L, 1L))) {
          warning(sprintf(
            "Treatment '%s': binary variable with values {%s, %s} is not coded as 0/1.",
            col_name, uvals[1L], uvals[2L]
          ), call. = FALSE)
        }
        list(type = "binary", label_info = col_name)
      } else {
        list(type = "numerical", label_info = col_name)
      }
    }

    treat_results <- if (is_df) {
      lapply(treat_col_names, function(col_nm) classify_treatment(treat_block[[col_nm]], col_nm))
    } else {
      lapply(seq_len(ncol(treat_block)), function(i) classify_treatment(treat_block[, i], treat_col_names[[i]]))
    }
    names(treat_results) <- treat_col_names

    meta <- lapply(treat_results, function(x) list(type = x$type, label_info = x$label_info))

    # Resolve label
    label <- if (!is.null(trt$label)) trt$label else nm

    new_treatment_meta[[nm]]   <- meta
    new_treatment_labels[nm]   <- label

    # Build enfold sub-task
    X <- extract_block(which_idx)
    x_df <- as.data.frame(X)
    y_vec <- if (is_df) data[[names(which_idx)[1L]]] else as.vector(data[, names(which_idx)[1L]])

    etask <- enfold::initialize_enfold(x_df, y_vec)

    if (!is.null(trt$learners)) {
      for (lrn in trt$learners) {
        etask <- enfold::add_learners(etask, lrn)
      }
    }
    if (!is.null(trt$metalearner)) {
      etask <- enfold::add_metalearners(etask, trt$metalearner)
    }

    new_treatment_tasks[[nm]] <- etask
    new_treatment_specs[[nm]] <- list(
      learners    = trt$learners,
      metalearner = trt$metalearner
    )
  }

  # ── Process outcomes ──────────────────────────────────────────────────────
  # Resolve confounder data from task (stored as column names + data_env)
  if (!is.null(task$confounder_cols)) {
    conf_data <- as.data.frame(data_env$data[, task$confounder_cols, drop = FALSE])
  } else {
    conf_data <- NULL
  }
  n_conf     <- if (!is.null(conf_data)) ncol(conf_data) else 0L
  conf_names <- if (!is.null(conf_data)) colnames(conf_data) else character(0)

  new_outcomes        <- list()
  new_outcome_labels  <- character(0)
  new_adjustment_sets <- list()
  new_censoring       <- list()
  new_outcome_tasks   <- list()
  new_outcome_specs   <- list()
  new_censoring_tasks <- list()
  new_censoring_specs <- list()

  for (nm in names(outcomes)) {
    oc <- outcomes[[nm]]

    # --- Extract outcome block ---
    which_idx <- resolve_cols(oc$which, paste0(nm, "$which"))
    if (is.null(which_idx) || length(which_idx) == 0L) {
      stop(sprintf("outcome `%s`: `which` resolved to no columns.", nm), call. = FALSE)
    }

    # Check overlap with treatment columns
    if (!is.null(task$treatment_meta)) {
      treat_cols <- names(task$treatment_meta)
      overlap <- intersect(treat_cols, names(which_idx))
      if (length(overlap)) {
        stop(sprintf(
          "outcome `%s`: column(s) also appear in treatment: %s",
          nm, paste(overlap, collapse = ", ")
        ), call. = FALSE)
      }
    }

    y <- extract_block(which_idx, as_vec = TRUE)
    new_outcomes[[nm]] <- y

    # --- Label ---
    new_outcome_labels[nm] <- if (!is.null(oc$label)) oc$label else nm

    # --- Adjustment set ---
    resolve_adj_set <- function(sel, outcome_nm) {
      if (is.null(sel)) return(NULL)
      # Resolve confounder columns from task
      if (is.null(conf_data)) {
        stop(sprintf("outcome `%s`: adjustment_set specified but no confounders in task.", outcome_nm), call. = FALSE)
      }
      conf_names_local <- conf_names
      n_conf_local <- n_conf
      if (is.character(sel)) {
        idx <- match(sel, conf_names_local)
        bad <- sel[is.na(idx)]
        if (length(bad)) {
          stop(sprintf(
            "outcome `%s`: adjustment_set column(s) not found in confounders: %s",
            outcome_nm, paste(bad, collapse = ", ")
          ), call. = FALSE)
        }
        as.integer(idx)
      } else if (is.numeric(sel) || is.integer(sel)) {
        idx <- as.integer(sel)
        oob <- idx[idx < 1L | idx > n_conf_local]
        if (length(oob)) {
          stop(sprintf(
            "outcome `%s`: adjustment_set index/indices out of range (ncol = %d): %s",
            outcome_nm, n_conf_local, paste(oob, collapse = ", ")
          ), call. = FALSE)
        }
        idx
      } else {
        stop(sprintf(
          "outcome `%s`: adjustment_set must be a character vector or integer indices.",
          outcome_nm
        ), call. = FALSE)
      }
    }
    new_adjustment_sets[[nm]] <- resolve_adj_set(oc$adjustment_set, nm)

    # --- Censoring (stored as integer vector: 1 = observed, 0 = censored) ---
    cens_vec <- NULL

    if (!rlang::quo_is_null(oc$censoring)) {
      cens_idx <- resolve_cols(oc$censoring, paste0(nm, "$censoring"))
      if (is.null(cens_idx) || length(cens_idx) == 0L) {
        stop(sprintf("outcome `%s`: `censoring` resolved to no columns.", nm), call. = FALSE)
      }

      # Enforce single column for censoring
      if (length(cens_idx) > 1L) {
        stop(sprintf(
          "outcome `%s`: `censoring` must resolve to a single column, not %d.",
          nm, length(cens_idx)
        ), call. = FALSE)
      }

      cens_raw <- extract_block(cens_idx, as_vec = TRUE)
      if (!is.numeric(cens_raw) && !is.integer(cens_raw)) {
        stop(sprintf(
          "outcome `%s`: censoring column must be numeric or integer, got '%s'.",
          nm, class(cens_raw)[1L]
        ), call. = FALSE)
      }

      # Validate: all non-NA values should be 0 or 1
      non_na <- cens_raw[!is.na(cens_raw)]
      if (length(non_na) > 0L) {
        bad_vals <- setdiff(unique(non_na), c(0, 1))
        if (length(bad_vals)) {
          stop(sprintf(
            "outcome `%s`: censoring indicators must be 0 or 1 (found: %s).",
            nm, paste(bad_vals, collapse = ", ")
          ), call. = FALSE)
        }
      }

      cens_vec <- as.integer(cens_raw)

      # Check for residual NAs: rows where outcome is NA but censoring = 1 (uncensored)
      na_in_outcome <- if (is.vector(y)) {
        is.na(y)
      } else {
        apply(is.na(as.matrix(y)), 1L, any)
      }
      residual_na <- na_in_outcome & !is.na(cens_vec) & cens_vec == 1L

      if (any(residual_na)) {
        cens_vec[residual_na] <- 0L
        if (task$verbose) {
          message(sprintf(
            "outcome '%s': %d observation(s) have NA in outcome but are marked uncensored. %s",
            nm, sum(residual_na),
            "Censoring set to 0 for these rows."
          ))
        }
      }

    } else {
      # Pure auto-detection from outcome NAs
      na_in_outcome <- if (is.vector(y)) {
        is.na(y)
      } else {
        apply(is.na(as.matrix(y)), 1L, any)
      }

      if (any(na_in_outcome)) {
        cens_vec <- as.integer(!na_in_outcome)
        if (task$verbose) {
          message(sprintf(
            "outcome '%s': %d missing value(s) detected. %s",
            nm, sum(na_in_outcome),
            "Censoring indicator created (1 = observed, 0 = censored)."
          ))
        }
      }
    }

    new_censoring[[nm]] <- cens_vec

    # --- Build outcome enfold sub-task ---
    if (!is.null(oc$learners)) {
      y_for_enfold <- extract_block(which_idx, as_vec = FALSE)
      if (is.vector(y_for_enfold)) y_for_enfold <- as.data.frame(as.matrix(y_for_enfold))

      etask <- enfold::initialize_enfold(
        conf_data,
        y_for_enfold
      )
      for (lrn in oc$learners) {
        etask <- enfold::add_learners(etask, lrn)
      }
      if (!is.null(oc$metalearner)) {
        etask <- enfold::add_metalearners(etask, oc$metalearner)
      }
      new_outcome_tasks[[nm]] <- etask
    }

    new_outcome_specs[[nm]] <- list(
      learners    = oc$learners,
      metalearner = oc$metalearner
    )

    # --- Build censoring enfold sub-task (if censoring exists) ---
    if (!is.null(cens_vec)) {
      etask_cens <- enfold::initialize_enfold(
        conf_data,
        cens_vec
      )

      # Add base learners from the outcome spec (if any)
      if (!is.null(oc$learners)) {
        for (lrn in oc$learners) {
          etask_cens <- enfold::add_learners(etask_cens, lrn)
        }
      }

      # Add metalearner (default to superlearner with binomial loss)
      if (!is.null(oc$metalearner)) {
        etask_cens <- enfold::add_metalearners(etask_cens, oc$metalearner)
      } else {
        etask_cens <- enfold::add_metalearners(etask_cens,
          enfold::mtl_superlearner("sl", loss_fun = enfold::loss_logistic()))
      }

      new_censoring_tasks[[nm]] <- etask_cens
      new_censoring_specs[[nm]] <- list(
        learners    = oc$learners,
        metalearner = oc$metalearner %||% enfold::mtl_superlearner("sl", loss_fun = enfold::loss_logistic())
      )
    }
  }

  # ── Assign into task ──────────────────────────────────────────────────────
  if (length(new_treatment_meta)) {
    if (is.null(task$treatment_meta)) {
      task$treatment_meta    <- new_treatment_meta
      task$treatment_labels  <- new_treatment_labels
      task$treatment_tasks   <- new_treatment_tasks
      task$treatment_specs   <- new_treatment_specs
    } else {
      task$treatment_meta    <- c(task$treatment_meta, new_treatment_meta)
      task$treatment_labels  <- c(task$treatment_labels, new_treatment_labels)
      task$treatment_tasks   <- c(task$treatment_tasks, new_treatment_tasks)
      task$treatment_specs   <- c(task$treatment_specs, new_treatment_specs)
    }
  }

  if (length(new_outcomes)) {
    if (is.null(task$outcomes)) {
      task$outcomes          <- new_outcomes
      task$outcome_labels    <- new_outcome_labels
      task$adjustment_sets   <- new_adjustment_sets
      task$censoring         <- new_censoring
      task$outcome_tasks     <- new_outcome_tasks
      task$outcome_specs     <- new_outcome_specs
      task$censoring_tasks   <- new_censoring_tasks
      task$censoring_specs   <- new_censoring_specs
    } else {
      task$outcomes          <- c(task$outcomes, new_outcomes)
      task$outcome_labels    <- c(task$outcome_labels, new_outcome_labels)
      task$adjustment_sets   <- c(task$adjustment_sets, new_adjustment_sets)
      task$censoring         <- c(task$censoring, new_censoring)
      task$outcome_tasks     <- c(task$outcome_tasks, new_outcome_tasks)
      task$outcome_specs     <- c(task$outcome_specs, new_outcome_specs)
      task$censoring_tasks   <- c(task$censoring_tasks, new_censoring_tasks)
      task$censoring_specs   <- c(task$censoring_specs, new_censoring_specs)
    }
  }

  task
}