# ══════════════════════════════════════════════════════════════════════════════
# treatment() constructor
# ══════════════════════════════════════════════════════════════════════════════

#' Define a treatment variable
#'
#' Constructs an \code{enact_treatment} object that describes a single
#' treatment.  The column selection is captured unevaluated (via tidyselect /
#' quosure semantics) and resolved later by \code{\link{add}}.  Nuisance
#' models are specified separately via \code{\link{add_models}}.
#'
#' @param which <[`tidyselect`][tidyselect::language]> Treatment column(s) in
#'   the study data.  Also accepts a character vector of column names or an
#'   integer index vector.
#' @param label Optional character string.  Display label for this treatment.
#'
#' @return An S3 object of class \code{enact_treatment}.
#' @export
treatment <- function(which, label = NULL) {
  which_quo <- rlang::enquo(which)

  if (!is.null(label) && (!is.character(label) || length(label) != 1L)) {
    stop("`label` must be a single character string or NULL.", call. = FALSE)
  }

  structure(
    list(
      which = which_quo,
      label = label
    ),
    class = "enact_treatment"
  )
}

#' @export
print.enact_treatment <- function(x, ...) {
  lab <- if (!is.null(x$label)) x$label else "(unlabelled)"
  which_expr <- rlang::as_label(x$which)
  cat(sprintf("enact_treatment | %s | which: %s\n", lab, which_expr))
  invisible(x)
}


# ══════════════════════════════════════════════════════════════════════════════
# outcome() constructor
# ══════════════════════════════════════════════════════════════════════════════

#' Define an outcome variable
#'
#' Constructs an \code{enact_outcome} object that describes a single study
#' outcome.  Selections are captured unevaluated (via tidyselect / quosure
#' semantics) and resolved later by \code{\link{add}}.  Nuisance models are
#' specified separately via \code{\link{add_models}}.
#'
#' @param which <[`tidyselect`][tidyselect::language]> Outcome column(s) in the
#'   study data.  Also accepts a character vector of column names or an integer
#'   index vector.
#' @param censoring <[`tidyselect`][tidyselect::language]> Censoring indicator
#'   column in the study data.  Values should be \code{0} (censored)
#'   or \code{1} (observed).  Also accepts a character column name or integer
#'   column index (must resolve to a single column).
#'   Required when the outcome contains \code{NA} values.  The censoring
#'   indicator must be \code{0} wherever the outcome is \code{NA}.
#'   When \code{NULL} and the outcome has no \code{NA}s, no censoring
#'   is recorded.
#' @param label Optional character string.  Display label for this outcome.
#' @param contrasts List of contrast functions to apply to this outcome.
#'  Each function should take two arguments: \code{reference} and \code{treatment}.
#'  \code{\link{ate}}, \code{\link{log_relative_ate}}, and \code{\link{log_odds_ratio}}
#'  are provided as standard contrasts for interventions. Default is the average treatment effect.
#' @param adjustment_set Character vector of confounder column names or integer
#'   indices into the confounder block.  \code{NULL} (default) inherits the full
#'   global confounder set.
#'
#' @return An S3 object of class \code{enact_outcome}.
#' @export
outcome <- function(
  which,
  censoring = NULL,
  label = NULL,
  contrasts = list(ate()),
  adjustment_set = NULL
) {
  which_quo <- rlang::enquo(which)
  censoring_quo <- rlang::enquo(censoring)

  if (!is.null(label) && (!is.character(label) || length(label) != 1L)) {
    stop("`label` must be a single character string or NULL.", call. = FALSE)
  }

  # Validate contrasts
  if (
    !is.list(contrasts) ||
      length(contrasts) == 0L ||
      !all(vapply(contrasts, is.function, logical(1L)))
  ) {
    stop("`contrasts` must be a non-empty list of functions.", call. = FALSE)
  }
  if (
    !all(vapply(
      contrasts,
      function(f) {
        args <- names(formals(f))
        all(c("reference", "treatment") %in% args)
      },
      logical(1L)
    ))
  ) {
    stop(
      "Each contrast function must have arguments named 'reference' and 'treatment'.",
      call. = FALSE
    )
  }

  structure(
    list(
      which = which_quo,
      censoring = censoring_quo,
      label = label,
      contrasts = contrasts,
      adjustment_set = adjustment_set
    ),
    class = "enact_outcome"
  )
}

#' @export
print.enact_outcome <- function(x, ...) {
  lab <- if (!is.null(x$label)) x$label else "(unlabelled)"
  which_expr <- rlang::as_label(x$which)
  cens_expr <- if (rlang::quo_is_null(x$censoring)) {
    "none"
  } else {
    rlang::as_label(x$censoring)
  }
  cat(sprintf(
    "enact_outcome | %s | which: %s | censoring: %s\n",
    lab,
    which_expr,
    cens_expr
  ))
  
  invisible(x)
}


# ══════════════════════════════════════════════════════════════════════════════
# add() — S3 generic + enact_task method
# ══════════════════════════════════════════════════════════════════════════════

#' Add components to a study task
#'
#' Generic function for attaching treatments, outcomes, and censoring learners
#' to a \code{enact_task}.  Methods dispatch on the class of the objects passed
#' in \code{...}.
#'
#' @param task A study task object (e.g. \code{enact_task}).
#' @param \dots Named objects to add.  Each argument name becomes the
#'   treatment or outcome name in the task.
#' @return The modified task.
#' @export
add <- function(task, ...) {
  UseMethod("add")
}


#' Add treatments and outcomes to a enact_task
#'
#' Each argument must be an \code{enact_treatment} or \code{enact_outcome}
#' object.  Argument names become the treatment or outcome names in the task.
#' Nuisance models are specified separately via \code{\link{add_models}}.
#'
#' @param task A \code{enact_task} object.
#' @param \dots Named \code{\link{treatment}} or \code{\link{outcome}} objects.
#' @return The modified \code{enact_task}.
#'
#' @details
#' \strong{Treatments} are validated (must be numeric, binary coding checked)
#' and metadata is stored for downstream use.
#'
#' \strong{Outcomes} are extracted and censoring is handled as follows:
#' \enumerate{
#'   \item If the outcome contains \code{NA} values and no censoring column was
#'     specified, an error is raised.  The user must provide an explicit
#'     censoring indicator that is \code{0} wherever the outcome is \code{NA}.
#'   \item If a censoring column was specified, it is extracted and validated:
#'     any row where the outcome is \code{NA} but the censoring indicator is
#'     \code{1} (observed) triggers an error.
#'   \item If no \code{NA}s are found and no censoring was given, censoring is
#'     \code{NULL} for that outcome.
#' }
#'
#' @export
add.enact_task <- function(task, ...) {
  # ── Capture and classify dots ─────────────────────────────────────────────
  dots <- list(...)
  if (length(dots) == 0L) {
    stop(
      "At least one treatment() or outcome() object must be provided.",
      call. = FALSE
    )
  }
  if (is.null(names(dots)) || any(names(dots) == "")) {
    stop(
      "All arguments must be named (e.g. add(task, A = treatment(...))).",
      call. = FALSE
    )
  }

  is_trt <- vapply(dots, inherits, logical(1L), "enact_treatment")
  is_out <- vapply(dots, inherits, logical(1L), "enact_outcome")
  bad <- !is_trt & !is_out
  if (any(bad)) {
    stop(
      sprintf(
        "The following arguments are not treatment() or outcome() objects: %s",
        paste(names(dots)[bad], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  treatments <- dots[is_trt]
  outcomes <- dots[is_out]

  # ── Duplicate-name check ──────────────────────────────────────────────────
  if (!is.null(task$treatment_meta)) {
    dupes <- intersect(names(treatments), names(task$treatment_meta))
    if (length(dupes)) {
      stop(
        sprintf(
          "Treatment name(s) already exist in task: %s",
          paste(dupes, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  if (!is.null(task$outcomes)) {
    dupes <- intersect(names(outcomes), names(task$outcomes))
    if (length(dupes)) {
      stop(
        sprintf(
          "Outcome name(s) already exist in task: %s",
          paste(dupes, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  # ── Shared helpers ────────────────────────────────────────────────────────
  data_env <- task$data_env
  if (is.null(data_env) || !exists("data", envir = data_env)) {
    stop(
      "Task does not contain stored data.  Re-run initiate_study().",
      call. = FALSE
    )
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
            stop(
              sprintf(
                "Cannot resolve `%s`. Use tidyselect syntax, a character vector of column names, or an integer index vector.\nUnderlying error: %s",
                arg_name,
                conditionMessage(ts_err)
              ),
              call. = FALSE
            )
          }
        )
        if (is.null(val)) {
          return(NULL)
        }
        if (is.character(val)) {
          idx <- match(val, colnames(data))
          bad <- val[is.na(idx)]
          if (length(bad)) {
            stop(
              sprintf(
                "In `%s`: column(s) not found in data: %s",
                arg_name,
                paste(bad, collapse = ", ")
              ),
              call. = FALSE
            )
          }
          setNames(as.integer(idx), val)
        } else if (is.numeric(val) || is.integer(val)) {
          idx <- as.integer(val)
          oob <- idx[idx < 1L | idx > n_col]
          if (length(oob)) {
            stop(
              sprintf(
                "In `%s`: index/indices out of range: %s",
                arg_name,
                paste(oob, collapse = ", ")
              ),
              call. = FALSE
            )
          }
          setNames(idx, colnames(data)[idx])
        } else {
          stop(
            sprintf(
              "In `%s`: selection must resolve to column names (character) or indices (integer).",
              arg_name
            ),
            call. = FALSE
          )
        }
      }
    )
  }

  extract_block <- function(idx) {
    block <- data[, idx, drop = FALSE]
    if (is_df) as.data.frame(block) else as.matrix(block)
  }

  # ── Process treatments ────────────────────────────────────────────────────
  new_treatment_meta <- list()
  new_treatment_labels <- character(0)

  for (nm in names(treatments)) {
    trt <- treatments[[nm]]

    which_idx <- resolve_cols(trt$which, paste0(nm, "$which"))
    if (is.null(which_idx) || length(which_idx) == 0L) {
      stop(
        sprintf("treatment `%s`: `which` resolved to no columns.", nm),
        call. = FALSE
      )
    }

    treat_block <- extract_block(which_idx)
    treat_col_names <- names(which_idx)

    # Validate each treatment column
    classify_treatment <- function(col, col_name) {
      if (!is.numeric(col) && !is.integer(col)) {
        stop(
          sprintf(
            "Treatment '%s': must be numeric or integer; got class '%s'.",
            col_name,
            class(col)[1L]
          ),
          call. = FALSE
        )
      }
      uvals <- sort(unique(col[!is.na(col)]))
      if (length(uvals) <= 1L) {
        warning(
          sprintf(
            "Treatment '%s': column is constant (all values = %s).",
            col_name,
            if (length(uvals) == 0L) "NA" else as.character(uvals)
          ),
          call. = FALSE
        )
        list(type = "binary", label_info = col_name)
      } else if (length(uvals) == 2L) {
        if (!identical(uvals, c(0, 1)) && !identical(uvals, c(0L, 1L))) {
          warning(
            sprintf(
              "Treatment '%s': binary variable with values {%s, %s} is not coded as 0/1.",
              col_name,
              uvals[1L],
              uvals[2L]
            ),
            call. = FALSE
          )
        }
        list(type = "binary", label_info = col_name)
      } else {
        list(type = "numerical", label_info = col_name)
      }
    }

    treat_results <- if (is_df) {
      lapply(treat_col_names, function(col_nm) {
        classify_treatment(treat_block[[col_nm]], col_nm)
      })
    } else {
      lapply(seq_len(ncol(treat_block)), function(i) {
        classify_treatment(treat_block[, i], treat_col_names[[i]])
      })
    }
    names(treat_results) <- treat_col_names

    meta <- lapply(treat_results, function(x) {
      list(type = x$type, label_info = x$label_info)
    })

    label <- if (!is.null(trt$label)) trt$label else nm

    new_treatment_meta[[nm]] <- meta
    new_treatment_labels[nm] <- label
  }

  # ── Process outcomes ──────────────────────────────────────────────────────
  if (!is.null(task$confounder_cols)) {
    conf_data <- as.data.frame(data_env$data[,
      task$confounder_cols,
      drop = FALSE
    ])
  } else {
    conf_data <- NULL
  }
  n_conf <- if (!is.null(conf_data)) ncol(conf_data) else 0L
  conf_names <- if (!is.null(conf_data)) colnames(conf_data) else character(0)

  new_outcomes <- list()
  new_outcome_labels <- character(0)
  new_adjustment_sets <- list()
  new_censoring <- list()
  new_outcome_contrasts <- list()

  for (nm in names(outcomes)) {
    oc <- outcomes[[nm]]

    # --- Extract outcome block ---
    which_idx <- resolve_cols(oc$which, paste0(nm, "$which"))
    if (is.null(which_idx) || length(which_idx) == 0L) {
      stop(
        sprintf("outcome `%s`: `which` resolved to no columns.", nm),
        call. = FALSE
      )
    }

    # Check overlap with treatment columns
    if (!is.null(task$treatment_meta)) {
      treat_cols <- names(task$treatment_meta)
      overlap <- intersect(treat_cols, names(which_idx))
      if (length(overlap)) {
        stop(
          sprintf(
            "outcome `%s`: column(s) also appear in treatment: %s",
            nm,
            paste(overlap, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }

    y <- extract_block(which_idx)
    new_outcomes[[nm]] <- y

    # --- Label ---
    new_outcome_labels[nm] <- if (!is.null(oc$label)) oc$label else nm

    # --- Adjustment set ---
    resolve_adj_set <- function(sel, outcome_nm) {
      if (is.null(sel)) {
        return(NULL)
      }
      if (is.null(conf_data)) {
        stop(
          sprintf(
            "outcome `%s`: adjustment_set specified but no confounders in task.",
            outcome_nm
          ),
          call. = FALSE
        )
      }
      conf_names_local <- conf_names
      n_conf_local <- n_conf
      if (is.character(sel)) {
        idx <- match(sel, conf_names_local)
        bad <- sel[is.na(idx)]
        if (length(bad)) {
          stop(
            sprintf(
              "outcome `%s`: adjustment_set column(s) not found in confounders: %s",
              outcome_nm,
              paste(bad, collapse = ", ")
            ),
            call. = FALSE
          )
        }
        as.integer(idx)
      } else if (is.numeric(sel) || is.integer(sel)) {
        idx <- as.integer(sel)
        oob <- idx[idx < 1L | idx > n_conf_local]
        if (length(oob)) {
          stop(
            sprintf(
              "outcome `%s`: adjustment_set index/indices out of range (ncol = %d): %s",
              outcome_nm,
              n_conf_local,
              paste(oob, collapse = ", ")
            ),
            call. = FALSE
          )
        }
        idx
      } else {
        stop(
          sprintf(
            "outcome `%s`: adjustment_set must be a character vector or integer indices.",
            outcome_nm
          ),
          call. = FALSE
        )
      }
    }
    new_adjustment_sets[[nm]] <- resolve_adj_set(oc$adjustment_set, nm)

    # --- Censoring (stored as integer vector: 1 = observed, 0 = censored) ---
    cens_vec <- NULL
    na_in_outcome <- apply(is.na(y), 1L, any)
    has_na <- any(na_in_outcome)

    if (!rlang::quo_is_null(oc$censoring)) {
      cens_idx <- resolve_cols(oc$censoring, paste0(nm, "$censoring"))
      if (is.null(cens_idx) || length(cens_idx) == 0L) {
        stop(
          sprintf("outcome `%s`: `censoring` resolved to no columns.", nm),
          call. = FALSE
        )
      }
      if (length(cens_idx) > 1L) {
        stop(
          sprintf(
            "outcome `%s`: `censoring` must resolve to a single column, not %d.",
            nm,
            length(cens_idx)
          ),
          call. = FALSE
        )
      }

      cens_col_name <- names(cens_idx)[1L]
      cens_raw <- extract_block(cens_idx)[[1L]]
      if (!is.numeric(cens_raw) && !is.integer(cens_raw)) {
        stop(
          sprintf(
            "outcome `%s`: censoring column must be numeric or integer, got '%s'.",
            nm,
            class(cens_raw)[1L]
          ),
          call. = FALSE
        )
      }

      non_na <- cens_raw[!is.na(cens_raw)]
      if (length(non_na) > 0L) {
        bad_vals <- setdiff(unique(non_na), c(0, 1))
        if (length(bad_vals)) {
          stop(
            sprintf(
              "outcome `%s`: censoring indicators must be 0 or 1 (found: %s).",
              nm,
              paste(bad_vals, collapse = ", ")
            ),
            call. = FALSE
          )
        }
      }

      cens_vec <- as.integer(cens_raw)

      # Error if outcome is NA but censoring = 1 (observed)
      if (has_na) {
        uncovered <- na_in_outcome & !is.na(cens_vec) & cens_vec == 1L
        if (any(uncovered)) {
          stop(
            sprintf(
              "outcome `%s`: %d observation(s) have NA in outcome but censoring indicator '%s' is 1 (observed). %s",
              nm,
              sum(uncovered),
              cens_col_name,
              "Censoring must be 0 wherever the outcome is NA."
            ),
            call. = FALSE
          )
        }
      }

      attr(cens_vec, "cens_col") <- cens_col_name
    } else {
      if (has_na) {
        stop(
          sprintf(
            "outcome `%s`: %d missing value(s) found in outcome but no censoring indicator was specified. %s",
            nm,
            sum(has_na),
            "Provide a censoring column (censoring = ...) that is 0 wherever the outcome is NA."
          ),
          call. = FALSE
        )
      }
    }

    new_censoring[[nm]] <- cens_vec

    # --- Contrasts ---
    new_outcome_contrasts[[nm]] <- oc$contrasts
  }

  # ── Assign into task ──────────────────────────────────────────────────────
  if (length(new_treatment_meta)) {
    if (is.null(task$treatment_meta)) {
      task$treatment_meta <- new_treatment_meta
      task$treatment_labels <- new_treatment_labels
    } else {
      task$treatment_meta <- c(task$treatment_meta, new_treatment_meta)
      task$treatment_labels <- c(task$treatment_labels, new_treatment_labels)
    }
  }

  if (length(new_outcomes)) {
    if (is.null(task$outcomes)) {
      task$outcomes <- new_outcomes
      task$outcome_labels <- new_outcome_labels
      task$adjustment_sets <- new_adjustment_sets
      task$censoring <- new_censoring
      task$outcome_contrasts <- new_outcome_contrasts
    } else {
      task$outcomes <- c(task$outcomes, new_outcomes)
      task$outcome_labels <- c(task$outcome_labels, new_outcome_labels)
      task$adjustment_sets <- c(task$adjustment_sets, new_adjustment_sets)
      task$censoring <- c(task$censoring, new_censoring)
      task$outcome_contrasts <- c(task$outcome_contrasts, new_outcome_contrasts)
    }
  }

  task
}
