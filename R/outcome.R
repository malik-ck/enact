# ══════════════════════════════════════════════════════════════════════════════
# add_treatment() / add_outcome() — attach treatments and outcomes to a task
# ══════════════════════════════════════════════════════════════════════════════

#' Add a treatment to a study task
#'
#' Attaches a treatment specification to an \code{enact_task}.  Single-column
#' treatments use the column name as their identifier by default; multi-column
#' grouped treatments require an explicit \code{label} which then serves as
#' the identifier in downstream selectors (e.g. \code{treatments("A", ...)})
#' and tells \code{\link{add_models}} to build a single joint nuisance task
#' over the grouped columns.
#'
#' @param task A \code{enact_task} object.
#' @param which <[`tidyselect`][tidyselect::language]> Treatment column(s) in
#'   the study data.  One column for atomic treatments, multiple for a
#'   grouped (multivariate) treatment.  Also accepts a character vector of
#'   column names or an integer index vector.
#' @param label Character string used as the treatment's identifier.  For
#'   single-column \code{which}, defaults to the column name.  Required when
#'   \code{which} resolves to more than one column.
#' @param column_labels Optional character vector of per-column display labels
#'   for Table 1.  Named (matched to column names) or unnamed (positional,
#'   same length as \code{which}).  Defaults to column names.
#'
#' @return The modified \code{enact_task}.
#' @export
add_treatment <- function(task, which, label = NULL, column_labels = NULL) {
  if (!inherits(task, "enact_task")) {
    stop("`task` must be an enact_task object.", call. = FALSE)
  }
  if (!is.null(label) && (!is.character(label) || length(label) != 1L ||
                          is.na(label) || !nzchar(label))) {
    stop("`label` must be a single non-empty character string or NULL.", call. = FALSE)
  }

  which_quo <- rlang::enquo(which)
  ctx <- task_resolution_ctx(task)

  which_idx <- resolve_cols(which_quo, "which", ctx)
  if (is.null(which_idx) || length(which_idx) == 0L) {
    stop("`which` resolved to no columns.", call. = FALSE)
  }
  col_nms <- names(which_idx)

  if (length(col_nms) > 1L && is.null(label)) {
    stop(sprintf(
      "`label` is required when `which` resolves to multiple columns (%s). It becomes the group's identifier.",
      paste(col_nms, collapse = ", ")
    ), call. = FALSE)
  }

  identifier <- if (!is.null(label)) label else col_nms[1L]

  if (!is.null(task$treatment_meta) && identifier %in% names(task$treatment_meta)) {
    stop(sprintf("Treatment identifier already exists in task: %s", identifier), call. = FALSE)
  }
  if (!is.null(task$treatment_meta)) {
    existing_cols <- unlist(lapply(task$treatment_meta, names), use.names = FALSE)
    overlap <- intersect(existing_cols, col_nms)
    if (length(overlap)) {
      stop(sprintf(
        "Treatment column(s) already attached to another treatment: %s",
        paste(overlap, collapse = ", ")
      ), call. = FALSE)
    }
  }

  # For single-col with a user-supplied label and no explicit column_labels,
  # the label doubles as the per-column display label.
  col_labels <- if (length(col_nms) == 1L && !is.null(label) && is.null(column_labels)) {
    setNames(label, col_nms)
  } else {
    resolve_column_labels(column_labels, col_nms, identifier)
  }

  treat_block <- extract_block(ctx$data, which_idx, ctx$is_df)
  meta <- lapply(seq_along(col_nms), function(i) {
    col_nm <- col_nms[i]
    col_vec <- if (ctx$is_df) treat_block[[col_nm]] else treat_block[, i]
    cl <- classify_treatment_col(col_vec, col_nm)
    list(type = cl$type, label_info = cl$label_info)
  })
  names(meta) <- col_nms

  if (is.null(task$treatment_meta)) {
    task$treatment_meta <- list()
    task$treatment_labels <- character(0)
  }
  task$treatment_meta[[identifier]] <- meta
  task$treatment_labels[col_nms] <- col_labels

  task
}


#' Add an outcome to a study task
#'
#' Attaches an outcome specification to an \code{enact_task}.  A non-empty
#' \code{label} is required and becomes the outcome's identifier used by
#' downstream selectors (e.g. \code{outcomes("Y", ...)}).  Multi-column
#' outcomes (multivariate \code{which}) are grouped under the single label.
#' Nuisance models are specified separately via \code{\link{add_models}}.
#'
#' @param task A \code{enact_task} object.
#' @param which <[`tidyselect`][tidyselect::language]> Outcome column(s) in
#'   the study data.  Also accepts a character vector of column names or an
#'   integer index vector.
#' @param label Character string.  Identifier and display label for this
#'   outcome.  Required.
#' @param censoring <[`tidyselect`][tidyselect::language]> Censoring indicator
#'   column in the study data.  Values should be \code{0} (censored)
#'   or \code{1} (observed).  Also accepts a character column name or integer
#'   column index (must resolve to a single column).
#'   Required when the outcome contains \code{NA} values.  The censoring
#'   indicator must be \code{0} wherever the outcome is \code{NA}.
#'   When \code{NULL} and the outcome has no \code{NA}s, no censoring
#'   is recorded.
#' @param contrasts List of contrast functions to apply to this outcome.
#'  Each function should take two arguments: \code{reference} and \code{treatment}.
#'  \code{\link{ate}}, \code{\link{log_relative_ate}}, and \code{\link{log_odds_ratio}}
#'  are provided as standard contrasts for interventions. Default is the average treatment effect.
#' @param adjustment_set Character vector of confounder column names or integer
#'   indices into the confounder block.  \code{NULL} (default) inherits the full
#'   global confounder set.
#'
#' @return The modified \code{enact_task}.
#' @export
add_outcome <- function(
  task,
  which,
  label,
  censoring = NULL,
  contrasts = list(ate()),
  adjustment_set = NULL
) {
  if (!inherits(task, "enact_task")) {
    stop("`task` must be an enact_task object.", call. = FALSE)
  }
  if (missing(label) || !is.character(label) || length(label) != 1L ||
      is.na(label) || !nzchar(label)) {
    stop("`label` is required and must be a single non-empty character string.", call. = FALSE)
  }
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
  if (!is.null(task$outcomes) && label %in% names(task$outcomes)) {
    stop(sprintf("Outcome label already exists in task: %s", label), call. = FALSE)
  }

  which_quo <- rlang::enquo(which)
  censoring_quo <- rlang::enquo(censoring)
  ctx <- task_resolution_ctx(task)

  which_idx <- resolve_cols(which_quo, paste0(label, "$which"), ctx)
  if (is.null(which_idx) || length(which_idx) == 0L) {
    stop(sprintf("outcome `%s`: `which` resolved to no columns.", label), call. = FALSE)
  }

  if (!is.null(task$treatment_meta)) {
    treat_cols <- names(task$treatment_meta)
    overlap <- intersect(treat_cols, names(which_idx))
    if (length(overlap)) {
      stop(
        sprintf(
          "outcome `%s`: column(s) also appear in treatment: %s",
          label,
          paste(overlap, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  y <- extract_block(ctx$data, which_idx, ctx$is_df)
  adj_idx <- resolve_adjustment_set(adjustment_set, task, label)
  cens_vec <- resolve_censoring(censoring_quo, y, ctx, label)

  if (is.null(task$outcomes)) {
    task$outcomes <- list()
    task$outcome_labels <- character(0)
    task$adjustment_sets <- list()
    task$censoring <- list()
    task$outcome_contrasts <- list()
  }
  task$outcomes[[label]] <- y
  task$outcome_labels[label] <- label
  task$adjustment_sets[[label]] <- adj_idx
  task$censoring[[label]] <- cens_vec
  task$outcome_contrasts[[label]] <- contrasts

  task
}


# ══════════════════════════════════════════════════════════════════════════════
# File-private helpers
# ══════════════════════════════════════════════════════════════════════════════

task_resolution_ctx <- function(task) {
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

  list(
    data = data,
    is_df = is_df,
    n_col = n_col,
    df_proxy = df_proxy,
    col_names = colnames(data)
  )
}

resolve_cols <- function(quo, arg_name, ctx) {
  tryCatch(
    tidyselect::eval_select(quo, ctx$df_proxy),
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
        idx <- match(val, ctx$col_names)
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
        oob <- idx[idx < 1L | idx > ctx$n_col]
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
        setNames(idx, ctx$col_names[idx])
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

extract_block <- function(data, idx, is_df) {
  block <- data[, idx, drop = FALSE]
  if (is_df) as.data.frame(block) else as.matrix(block)
}

classify_treatment_col <- function(col, col_name) {
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

resolve_column_labels <- function(user_labels, col_nms, identifier) {
  if (is.null(user_labels)) return(setNames(col_nms, col_nms))
  if (!is.character(user_labels)) {
    stop(sprintf(
      "treatment `%s`: `column_labels` must be a character vector.", identifier
    ), call. = FALSE)
  }
  out <- setNames(col_nms, col_nms)
  if (!is.null(names(user_labels))) {
    bad <- setdiff(names(user_labels), col_nms)
    if (length(bad)) {
      stop(sprintf(
        "treatment `%s`: `column_labels` names not in `which`: %s",
        identifier, paste(bad, collapse = ", ")
      ), call. = FALSE)
    }
    out[names(user_labels)] <- user_labels
  } else {
    if (length(user_labels) != length(col_nms)) {
      stop(sprintf(
        "treatment `%s`: `column_labels` has %d element(s) but `which` has %d column(s).",
        identifier, length(user_labels), length(col_nms)
      ), call. = FALSE)
    }
    out[] <- user_labels
  }
  out
}

resolve_adjustment_set <- function(sel, task, outcome_nm) {
  if (is.null(sel)) {
    return(NULL)
  }
  if (is.null(task$confounder_cols)) {
    stop(
      sprintf(
        "outcome `%s`: adjustment_set specified but no confounders in task.",
        outcome_nm
      ),
      call. = FALSE
    )
  }
  conf_names <- task$confounder_cols
  n_conf <- length(conf_names)
  if (is.character(sel)) {
    idx <- match(sel, conf_names)
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
    oob <- idx[idx < 1L | idx > n_conf]
    if (length(oob)) {
      stop(
        sprintf(
          "outcome `%s`: adjustment_set index/indices out of range (ncol = %d): %s",
          outcome_nm,
          n_conf,
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

resolve_censoring <- function(censoring_quo, y, ctx, outcome_nm) {
  na_in_outcome <- apply(is.na(y), 1L, any)
  has_na <- any(na_in_outcome)

  if (rlang::quo_is_null(censoring_quo)) {
    if (has_na) {
      stop(
        sprintf(
          "outcome `%s`: %d missing value(s) found in outcome but no censoring indicator was specified. %s",
          outcome_nm,
          sum(has_na),
          "Provide a censoring column (censoring = ...) that is 0 wherever the outcome is NA."
        ),
        call. = FALSE
      )
    }
    return(NULL)
  }

  cens_idx <- resolve_cols(
    censoring_quo,
    paste0(outcome_nm, "$censoring"),
    ctx
  )
  if (is.null(cens_idx) || length(cens_idx) == 0L) {
    stop(
      sprintf("outcome `%s`: `censoring` resolved to no columns.", outcome_nm),
      call. = FALSE
    )
  }
  if (length(cens_idx) > 1L) {
    stop(
      sprintf(
        "outcome `%s`: `censoring` must resolve to a single column, not %d.",
        outcome_nm,
        length(cens_idx)
      ),
      call. = FALSE
    )
  }

  cens_col_name <- names(cens_idx)[1L]
  cens_raw <- extract_block(ctx$data, cens_idx, ctx$is_df)[[1L]]
  if (!is.numeric(cens_raw) && !is.integer(cens_raw)) {
    stop(
      sprintf(
        "outcome `%s`: censoring column must be numeric or integer, got '%s'.",
        outcome_nm,
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
          outcome_nm,
          paste(bad_vals, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  cens_vec <- as.integer(cens_raw)

  if (has_na) {
    uncovered <- na_in_outcome & !is.na(cens_vec) & cens_vec == 1L
    if (any(uncovered)) {
      stop(
        sprintf(
          "outcome `%s`: %d observation(s) have NA in outcome but censoring indicator '%s' is 1 (observed). %s",
          outcome_nm,
          sum(uncovered),
          cens_col_name,
          "Censoring must be 0 wherever the outcome is NA."
        ),
        call. = FALSE
      )
    }
  }

  attr(cens_vec, "cens_col") <- cens_col_name
  cens_vec
}
