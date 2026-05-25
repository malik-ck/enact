#' Initiate a causal inference study task
#'
#' Creates the base \code{enact_task} scaffold that holds study data, confounders,
#' and optionally cluster variables.  Treatments and outcomes are added later via
#' \code{\link{add}} with \code{\link{treatment}} and \code{\link{outcome}}
#' constructors.
#'
#' @param data A \code{data.frame} or \code{matrix} containing all study
#'   variables.  Stored by reference in a locked environment (no copy).
#' @param confounders <[`tidyselect`][tidyselect::language]> Global adjustment
#'   set.  Also accepts a character vector of column names or an integer index
#'   vector.  Stored as column name references (no data extraction).
#' @param cluster <[`tidyselect`][tidyselect::language]> Optional cluster
#'   variable. Also accepts a character value or an integer index.
#' @param confounder_labels Optional character vector of display labels for
#'   confounder columns.  Named: matched by column name.  Unnamed: positional.
#' @param cluster_label Optional character vector of display labels for
#'   cluster columns.  Named or unnamed (positional).
#' @param extra_vars Optional named list of arbitrary objects to be carried in
#'   the task and made available to downstream functions (e.g. penalty matrices,
#'   offset vectors, external data).
#' @param verbose Logical.  If \code{TRUE} (default), pipeline functions emit
#'   informational messages.
#'
#' @return An S3 object of class \code{enact_task}.
#' @export
initiate_study <- function(
  data,
  confounders,
  cluster = NULL,
  confounder_labels = NULL,
  cluster_label = NULL,
  extra_vars = NULL,
  verbose = TRUE
) {
  # ── 1. Input validation ────────────────────────────────────────────────────
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("`data` must be a data.frame or matrix.", call. = FALSE)
  }
  if (
    !is.null(extra_vars) &&
      (!is.list(extra_vars) ||
        is.null(names(extra_vars)) ||
        any(names(extra_vars) == ""))
  ) {
    stop("`extra_vars` must be a fully named list.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L) {
    stop("`verbose` must be a single logical value.", call. = FALSE)
  }

  is_df <- is.data.frame(data)
  n_col <- ncol(data)
  n_obs <- nrow(data)

  if (is.null(colnames(data))) {
    colnames(data) <- paste0("V", seq_len(n_col))
  }

  # ── 2. Zero-row proxy for tidyselect ──────────────────────────────────────
  df_proxy <- if (is_df) {
    data[0L, , drop = FALSE]
  } else {
    as.data.frame(data[0L, , drop = FALSE])
  }

  # ── 3. Selection helper ───────────────────────────────────────────────────
  resolve_cols <- function(quo, arg_name, optional = FALSE) {
    if (optional && rlang::quo_is_null(quo)) {
      return(NULL)
    }
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

  # ── 4. Resolve confounders ────────────────────────────────────────────────
  confounders_idx <- resolve_cols(rlang::enquo(confounders), "confounders")
  confounder_cols <- names(confounders_idx)

  # ── 5. Resolve cluster ────────────────────────────────────────────────────
  cluster_idx <- resolve_cols(rlang::enquo(cluster), "cluster", optional = TRUE)
  cluster_col <- if (!is.null(cluster_idx)) names(cluster_idx) else NULL

  # ── 6. Resolve labels ────────────────────────────────────────────────────
  resolve_labels <- function(user_labels, col_names, arg_name) {
    out <- setNames(col_names, col_names)
    if (is.null(user_labels)) {
      return(out)
    }
    if (!is.character(user_labels)) {
      stop(sprintf("`%s` must be a character vector.", arg_name), call. = FALSE)
    }
    if (!is.null(names(user_labels))) {
      hits <- intersect(names(user_labels), col_names)
      out[hits] <- user_labels[hits]
    } else {
      if (length(user_labels) != length(col_names)) {
        stop(sprintf(
          "`%s` has %d element(s) but there are %d variable(s) in that group.",
          arg_name,
          length(user_labels),
          length(col_names)
        ), call. = FALSE)
      }
      out[] <- user_labels
    }
    out
  }

  resolved_confounder_labels <- resolve_labels(
    confounder_labels,
    confounder_cols,
    "confounder_labels"
  )
  resolved_cluster_label <- if (!is.null(cluster_col)) {
    resolve_labels(cluster_label, cluster_col, "cluster_label")
  } else {
    NULL
  }

  # Check that there is only one cluster variable, if specified
  if (!is.null(cluster_col) && length(cluster_col) > 1L) {
    stop("Multiple cluster variables specified. Can only specify one.", call. = FALSE)
  }

  # ── 7. Store data by reference ───────────────────────────────────────────
  data_env <- new.env(parent = emptyenv())
  data_env$data <- data
  lockEnvironment(data_env, bindings = TRUE)

  # ── 8. Assemble task ─────────────────────────────────────────────────────
  structure(
    list(
      n_obs = n_obs,

      data_env = data_env,
      confounder_cols = confounder_cols,
      confounder_labels = resolved_confounder_labels,

      cluster_col = cluster_col,
      cluster_label = resolved_cluster_label,

      # Populated by add()
      treatment_meta = NULL,
      treatment_labels = NULL,

      outcomes = NULL,
      outcome_labels = NULL,
      adjustment_sets = NULL,
      censoring = NULL,
      outcome_contrasts = NULL,

      # Enfold sub-tasks — populated by add_models()
      treatment_tasks = NULL,
      outcome_tasks = NULL,
      censoring_tasks = NULL,
      mtp_tasks = NULL,

      # Intervened datasets — populated by define_interventions()
      intervened_data = NULL,

      # Fitted results — populated by fit_interventions() / fit_outcomes()
      clever_covariates = NULL,
      outcome_predictions = NULL,

      # TMLE results — populated by do_tmle()
      tmle_results = NULL,
      tmle_bootstrap = NULL,

      # CV folds — populated by add_cv_folds()
      fold_store = NULL,

      extra_vars = extra_vars,
      verbose = verbose
    ),
    class = "enact_task"
  )
}


#' @export
print.enact_task <- function(x, ...) {
  cat(sprintf("── enact_task %s\n", paste(rep("\u2500", 38), collapse = "")))
  cat(sprintf("  Observations : %d\n\n", x$n_obs))

  # Confounders
  cat(sprintf("  Confounders  : %d variable(s)\n\n", length(x$confounder_cols)))

  # Treatments
  cat("  Treatments\n")
  if (is.null(x$treatment_meta)) {
    cat("    \u00b7 (none \u2014 use add())\n")
  } else {
    type_tag <- function(type) {
      switch(type, binary = "[binary]", numerical = "[numerical]", sprintf("[%s]", type))
    }
    for (nm in names(x$treatment_meta)) {
      m <- x$treatment_meta[[nm]]
      # Handle nested format (from add): list(col = list(type=...))
      # vs flat format: list(type=..., label_info=...)
      if (!is.null(m$type)) {
        cat(sprintf(
          "    \u00b7 %-35s %s\n",
          x$treatment_labels[[nm]],
          type_tag(m$type)
        ))
      } else {
        for (col_nm in names(m)) {
          cat(sprintf(
            "    \u00b7 %-35s %s\n",
            x$treatment_labels[[nm]],
            type_tag(m[[col_nm]]$type)
          ))
        }
      }
    }
  }
  cat("\n")

  # Outcomes
  cat("  Outcomes\n")
  if (is.null(x$outcomes)) {
    cat("    \u00b7 (none \u2014 use add())\n")
  } else {
    for (nm in names(x$outcomes)) {
      y <- x$outcomes[[nm]]
      dim_str <- if (!is.vector(y)) sprintf("  (%d columns)", ncol(y)) else ""
      cens_str <- if (nm %in% names(x$censoring) && !is.null(x$censoring[[nm]])) {
        cens <- x$censoring[[nm]]
        sprintf("  [%d censored]", sum(cens == 0L, na.rm = TRUE))
      } else {
        ""
      }
      cat(sprintf(
        "    \u00b7 %s%s%s\n",
        x$outcome_labels[[nm]],
        dim_str,
        cens_str
      ))
    }
  }

  # Optional structural variables
  optional_lines <- character(0)

  if (!is.null(x$cluster_col)) {
    optional_lines <- c(
      optional_lines,
      sprintf("  Cluster      : %d variable(s)", length(x$cluster_col))
    )
  }
  if (!is.null(x$adjustment_sets)) {
    custom_n <- sum(vapply(x$adjustment_sets, Negate(is.null), logical(1L)))
    optional_lines <- c(
      optional_lines,
      sprintf(
        "  Adj. sets    : custom for %d / %d outcome(s)",
        custom_n,
        length(x$outcomes)
      )
    )
  }
  if (!is.null(x$extra_vars)) {
    optional_lines <- c(
      optional_lines,
      sprintf(
        "  Extra vars   : %s",
        paste(names(x$extra_vars), collapse = ", ")
      )
    )
  }
  if (length(optional_lines)) {
    cat("\n", paste(optional_lines, collapse = "\n"), "\n", sep = "")
  }

  # Enfold sub-tasks
  n_trt <- if (!is.null(x$treatment_tasks)) length(x$treatment_tasks) else 0L
  n_out <- if (!is.null(x$outcome_tasks)) length(x$outcome_tasks) else 0L
  n_cens <- if (!is.null(x$censoring_tasks)) {
    sum(vapply(x$censoring_tasks, length, integer(1L)))
  } else {
    0L
  }
  n_mtp <- if (!is.null(x$mtp_tasks)) length(x$mtp_tasks) else 0L
  if (n_trt + n_out + n_cens + n_mtp > 0L) {
    cat(sprintf(
      "\n  Enfold tasks : %d treatment, %d outcome, %d censoring, %d mtp\n",
      n_trt, n_out, n_cens, n_mtp
    ))
  }

  # CV folds
  if (!is.null(x$fold_store) && !is.null(x$fold_store$cv)) {
    cv <- x$fold_store$cv
    perf_n  <- if (!is.null(cv$performance_sets)) length(cv$performance_sets) else 0L
    build_n <- if (!is.null(cv$build_sets)) length(cv$build_sets[[1L]]) else 0L
    cat(sprintf("  CV folds     : %d outer, %d inner\n", perf_n, build_n))
  }

  cat(paste(rep("\u2500", 50), collapse = ""), "\n")
  invisible(x)
}