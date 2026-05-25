# ══════════════════════════════════════════════════════════════════════════════
# add_treatment() / add_outcome() — attach treatments and outcomes to a task
# ══════════════════════════════════════════════════════════════════════════════

#' Add a treatment to a study task
#'
#' Attaches a single treatment specification to an \code{enact_task}.  The
#' column selection is resolved against the task's stored data immediately;
#' metadata is recorded for downstream use.  Nuisance models are specified
#' separately via \code{\link{add_models}}.
#'
#' @param task A \code{enact_task} object.
#' @param name Character.  Name used to refer to this treatment in downstream
#'   selectors (e.g. \code{treatments("A", ...)}).
#' @param which <[`tidyselect`][tidyselect::language]> Treatment column(s) in
#'   the study data.  Also accepts a character vector of column names or an
#'   integer index vector.
#' @param label Optional character string.  Display label for this treatment.
#'
#' @details
#' Treatments are validated (must be numeric, binary coding is checked) and
#' metadata is stored for downstream use.
#'
#' @return The modified \code{enact_task}.
#' @export
add_treatment <- function(task, name, which, label = NULL) {
  if (!inherits(task, "enact_task")) {
    stop("`task` must be an enact_task object.", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("`name` must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.null(label) && (!is.character(label) || length(label) != 1L)) {
    stop("`label` must be a single character string or NULL.", call. = FALSE)
  }
  if (!is.null(task$treatment_meta) && name %in% names(task$treatment_meta)) {
    stop(
      sprintf("Treatment name already exists in task: %s", name),
      call. = FALSE
    )
  }

  which_quo <- rlang::enquo(which)
  ctx <- task_resolution_ctx(task)

  which_idx <- resolve_cols(which_quo, paste0(name, "$which"), ctx)
  if (is.null(which_idx) || length(which_idx) == 0L) {
    stop(
      sprintf("treatment `%s`: `which` resolved to no columns.", name),
      call. = FALSE
    )
  }

  treat_block <- extract_block(ctx$data, which_idx, ctx$is_df)
  treat_col_names <- names(which_idx)

  treat_results <- if (ctx$is_df) {
    lapply(treat_col_names, function(col_nm) {
      classify_treatment_col(treat_block[[col_nm]], col_nm)
    })
  } else {
    lapply(seq_len(ncol(treat_block)), function(i) {
      classify_treatment_col(treat_block[, i], treat_col_names[[i]])
    })
  }
  names(treat_results) <- treat_col_names

  meta <- lapply(treat_results, function(x) {
    list(type = x$type, label_info = x$label_info)
  })
  lab <- if (!is.null(label)) label else name

  if (is.null(task$treatment_meta)) {
    task$treatment_meta <- list()
    task$treatment_labels <- character(0)
  }
  task$treatment_meta[[name]] <- meta
  task$treatment_labels[name] <- lab

  task
}


#' Add an outcome to a study task
#'
#' Attaches a single outcome specification to an \code{enact_task}.  Selections
#' are resolved against the task's stored data immediately; the outcome block
#' is extracted and censoring (if any) is validated.  Nuisance models are
#' specified separately via \code{\link{add_models}}.
#'
#' @param task A \code{enact_task} object.
#' @param name Character.  Name used to refer to this outcome in downstream
#'   selectors (e.g. \code{outcomes("Y", ...)}).
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
#' @details
#' Outcomes are extracted and censoring is handled as follows:
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
#' @return The modified \code{enact_task}.
#' @export
add_outcome <- function(
  task,
  name,
  which,
  censoring = NULL,
  label = NULL,
  contrasts = list(ate()),
  adjustment_set = NULL
) {
  if (!inherits(task, "enact_task")) {
    stop("`task` must be an enact_task object.", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("`name` must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.null(label) && (!is.character(label) || length(label) != 1L)) {
    stop("`label` must be a single character string or NULL.", call. = FALSE)
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
  if (!is.null(task$outcomes) && name %in% names(task$outcomes)) {
    stop(
      sprintf("Outcome name already exists in task: %s", name),
      call. = FALSE
    )
  }

  which_quo <- rlang::enquo(which)
  censoring_quo <- rlang::enquo(censoring)
  ctx <- task_resolution_ctx(task)

  which_idx <- resolve_cols(which_quo, paste0(name, "$which"), ctx)
  if (is.null(which_idx) || length(which_idx) == 0L) {
    stop(
      sprintf("outcome `%s`: `which` resolved to no columns.", name),
      call. = FALSE
    )
  }

  if (!is.null(task$treatment_meta)) {
    treat_cols <- names(task$treatment_meta)
    overlap <- intersect(treat_cols, names(which_idx))
    if (length(overlap)) {
      stop(
        sprintf(
          "outcome `%s`: column(s) also appear in treatment: %s",
          name,
          paste(overlap, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  y <- extract_block(ctx$data, which_idx, ctx$is_df)
  lab <- if (!is.null(label)) label else name
  adj_idx <- resolve_adjustment_set(adjustment_set, task, name)
  cens_vec <- resolve_censoring(censoring_quo, y, ctx, name)

  if (is.null(task$outcomes)) {
    task$outcomes <- list()
    task$outcome_labels <- character(0)
    task$adjustment_sets <- list()
    task$censoring <- list()
    task$outcome_contrasts <- list()
  }
  task$outcomes[[name]] <- y
  task$outcome_labels[name] <- lab
  task$adjustment_sets[[name]] <- adj_idx
  task$censoring[[name]] <- cens_vec
  task$outcome_contrasts[[name]] <- contrasts

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
