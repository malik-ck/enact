
#' @export
define_interventions <- function(task, ..., reference = NULL) {
  if (!inherits(task, "enact_task")) {
    stop("`task` must be a enact_task object.")
  }

  interventions <- list(...)

  if (length(interventions) == 0L) {
    stop("At least one intervention must be provided.")
  }

  bad <- !vapply(interventions, inherits, logical(1L), "nana_intervention")
  if (any(bad)) {
    indices <- which(bad)
    stop(sprintf(
      "Argument(s) %s are not valid intervention objects.\n%s",
      paste(indices, collapse = ", "),
      "Use intervention(), or a wrapper like static_intervention()."
    ))
  }

  # Each intervention must have a label
  labels <- vapply(
    interventions,
    function(obj) {
      if (
        is.null(obj$label) ||
          !is.character(obj$label) ||
          length(obj$label) != 1L
      ) {
        NA_character_
      } else {
        obj$label
      }
    },
    character(1L)
  )

  missing_label <- is.na(labels)
  if (any(missing_label)) {
    stop(
      sprintf(
        "Intervention(s) %s have no label set. All interventions must have a label.",
        paste(which(missing_label), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Check for duplicate labels
  dupes <- labels[duplicated(labels)]
  if (length(dupes)) {
    stop(
      sprintf(
        "Duplicate intervention labels: %s",
        paste(unique(dupes), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Check for conflicts with existing interventions
  if (!is.null(task$interventions)) {
    existing <- names(task$interventions)
    overlap <- intersect(labels, existing)
    if (length(overlap)) {
      stop(
        sprintf(
          "Intervention label(s) already exist in task: %s",
          paste(overlap, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  # Store by label
  names(interventions) <- labels
  task$interventions <- c(task$interventions, interventions)

  # Validate interventions and cache intervened datasets
  for (lab in labels) {
    validate_intervention(task, lab)
    task$intervened_data[[lab]] <- intervene(task, lab)
  }

  # Store which intervention is the reference
  # If NULL, take the first one passed
  if (is.null(reference)) {
    reference <- labels[[1L]]
  } else if (!reference %in% labels) {
    stop(
      sprintf(
        "Reference intervention '%s' not found among defined interventions.",
        reference
      ),
      call. = FALSE
    )
  }

  task$reference_intervention <- reference

  task
}

# ── Some methods ────────────────────────────────────────────────────────

#' @export
print.nana_intervention <- function(x, ...) {
  lab <- if (!is.null(x$label)) x$label else "(unlabelled)"
  mtp_str <- if (x$mtp) "MTP" else "non-MTP"
  cat(sprintf("nana_intervention | %s | %s\n", lab, mtp_str))
  invisible(x)
}

#' Get one realization of an intervention
#'
#' @param study A study, which is a \code{list} of class \code{enact_task}.
#' @param intervention_name Name of the intervention to apply to the study.
#' @export
intervene <- function(study, intervention_name) {
  UseMethod("intervene")
}

#' @export
intervene.enact_task <- function(study, intervention_name) {
  # Check existence of interventions
  if (is.null(study$interventions)) {
    stop(
      "No interventions found. Please use define_interventions() on the study before intervening."
    )
  }

  # Check existence of intervention
  if (is.null(study$interventions[[intervention_name]])) {
    stop("Intervention not found.")
  }

  # Check that treatment variables exist on the task
  if (is.null(study$treatment_meta)) {
    stop(
      "No treatment variables found in task. Use add() with treatment() before define_interventions().",
      call. = FALSE
    )
  }

  get_intervention_function <- study$interventions[[
    intervention_name
  ]]$intervene

  # Extract treatment and confounders from stored data
  data <- study$data_env$data
  is_df <- is.data.frame(data)

  # Build treatment block from treatment_meta column names
  trt_col_names <- unlist(lapply(study$treatment_meta, function(m) {
    vapply(m, function(mi) mi$label_info, character(1L))
  }))
  treatment_block <- if (is_df) {
    as.data.frame(data[, trt_col_names, drop = FALSE])
  } else {
    as.matrix(data[, trt_col_names, drop = FALSE])
  }

  conf_block <- if (is_df) {
    as.data.frame(data[, study$confounder_cols, drop = FALSE])
  } else {
    as.matrix(data[, study$confounder_cols, drop = FALSE])
  }

  result <- get_intervention_function(treatment_block, conf_block)

  if (!is.data.frame(result) && !is.matrix(result)) {
    stop(
      sprintf(
        "Intervention '%s' must return a data frame or matrix; got '%s'.",
        intervention_name,
        class(result)[[1L]]
      ),
      call. = FALSE
    )
  }

  result
}


#' Create a causal intervention object
#'
#' @param intervene A function with signature \code{function(a, l)} where
#'   \code{a} is a numeric data frame or matrix of observed treatment values and \code{l} is
#'   a data frame or matrix of covariates (one row per observation). Must
#'   return a numeric vector of length \code{length(a)} representing the
#'   intervened treatment values.
#' @param mtp Logical. If \code{TRUE}, the density ratio is estimated via the
#'   2n augmented dataset classification approach, which is required when the
#'   intervention depends on the natural value of treatment. If \code{FALSE},
#'   the density ratio is computed directly from the propensity score model.
#' @param label Character string.  Display label and identifier used by
#'   \code{\link{define_interventions}}.  Required.
#'
#' @return An S3 object of class \code{nana_intervention}.
#' @export
intervention <- function(
  intervene,
  mtp = FALSE,
  label = NULL
) {
  if (!is.function(intervene)) {
    stop("`intervene` must be a function.")
  }
  params <- names(formals(intervene))
  if (length(params) != 2L) {
    stop("`intervene` must accept exactly two arguments: (a, l).")
  }
  if (!is.logical(mtp) || length(mtp) != 1L) {
    stop("`mtp` must be a single logical value.")
  }
  if (is.null(label) || !is.character(label) || length(label) != 1L) {
    stop("`label` must be a single character string.")
  }

  structure(
    list(
      intervene = intervene,
      mtp = mtp,
      label = label
    ),
    class = "nana_intervention"
  )
}


#' Validate that an intervention returns a result compatible with the treatment
#'
#' @param task A \code{enact_task} object.
#' @param intervention_name Character. Name of the intervention to validate.
#' @return Invisibly returns the intervened treatment value if valid.
#'   Stops with an informative error if not.
#' @export
validate_intervention <- function(task, intervention_name) {
  if (!inherits(task, "enact_task")) {
    stop("`task` must be a enact_task object.")
  }
  if (is.null(task$interventions)) {
    stop("No interventions defined. Run define_interventions() first.")
  }
  if (is.null(task$interventions[[intervention_name]])) {
    stop(sprintf("Intervention '%s' not found in task.", intervention_name))
  }

  # Extract treatment from data_env for validation
  data <- task$data_env$data
  is_df <- is.data.frame(data)
  trt_col_names <- unlist(lapply(task$treatment_meta, function(m) {
    vapply(m, function(mi) mi$label_info, character(1L))
  }))
  trt <- if (is_df) {
    as.data.frame(data[, trt_col_names, drop = FALSE])
  } else {
    as.matrix(data[, trt_col_names, drop = FALSE])
  }

  result <- tryCatch(
    intervene(task, intervention_name),
    error = function(e) {
      stop(sprintf(
        "Intervention '%s' errored when called:\n  %s",
        intervention_name,
        conditionMessage(e)
      ))
    }
  )

  # Class check
  trt_class <- class(trt)[[1L]]
  res_class <- class(result)[[1L]]
  if (!identical(trt_class, res_class)) {
    stop(sprintf(
      "Intervention '%s': intervene() returned class '%s' but treatment is '%s'.\n%s",
      intervention_name,
      res_class,
      trt_class,
      "The intervention function must return the same class as the treatment block."
    ))
  }

  # Dimension check — works for vectors, matrices, and data frames
  trt_dim <- if (is.null(dim(trt))) length(trt) else dim(trt)
  res_dim <- if (is.null(dim(result))) length(result) else dim(result)

  if (!identical(trt_dim, res_dim)) {
    stop(sprintf(
      "Intervention '%s': intervene() returned %s but treatment has %s.\n%s",
      intervention_name,
      if (length(res_dim) == 1L) {
        sprintf("length %d", res_dim)
      } else {
        paste(res_dim, collapse = " x ")
      },
      if (length(trt_dim) == 1L) {
        sprintf("length %d", trt_dim)
      } else {
        paste(trt_dim, collapse = " x ")
      },
      "The intervention function must return a result with the same dimensions as the treatment block."
    ))
  }

  # Numeric check — all entries must be numeric after intervention
  vals <- if (is.data.frame(result)) unlist(result) else as.vector(result)
  if (!is.numeric(vals)) {
    stop(sprintf(
      "Intervention '%s': intervene() returned non-numeric values (class '%s').\n%s",
      intervention_name,
      class(vals)[[1L]],
      "All treatment values must be numeric after intervention."
    ))
  }

  if (task$verbose) {
    message(sprintf(
      "Intervention '%s': validated successfully.",
      intervention_name
    ))
  }

  invisible(result)
}


# ── Common wrappers ────────────────────────────────────────────────────────

#' Single-arm intervention: set one treatment column to 1, all others to 0
#'
#' @param column Column name or integer index identifying the treatment arm to
#'   activate. All other treatment columns are set to 0.
#' @param label Optional display label.
#' @export
intervention_arm <- function(column, label = NULL) {
  if (!is.character(column) && !is.numeric(column)) {
    stop("`column` must be a column name or integer index.")
  }
  force(column)
  intervention(
    intervene = function(a, l) {
      idx <- if (is.character(column)) {
        which(colnames(a) == column)
      } else {
        as.integer(column)
      }
      if (length(idx) == 0L || idx < 1L || idx > ncol(a)) {
        stop(sprintf("Column '%s' not found in treatment matrix.", column))
      }
      a[] <- 0
      a[, idx] <- 1
      a
    },
    mtp = FALSE,
    label = label %||% paste0("Arm: ", column)
  )
}

#' Static intervention: set treatment to a fixed value for all observations
#'
#' @param value A single numeric value.
#' @param label Optional display label.
#' @export
static_intervention <- function(value, label = NULL) {
  if (!is.numeric(value) || length(value) != 1L) {
    stop("`value` must be a single numeric scalar.")
  }
  force(value)
  intervention(
    intervene = function(a, l) {
      a[] <- value
      a
    },
    mtp = FALSE,
    label = label %||% paste0("A = ", value)
  )
}

#' MTP intervention: shift treatment as a deterministic function of (a, l)
#'
#' @param shift_fn A function \code{function(a, l)} returning the shifted
#'   treatment value.
#' @param label Optional display label.
#' @export
mtp_intervention <- function(shift_fn, label = "MTP intervention") {
  if (!is.function(shift_fn)) {
    stop("`shift_fn` must be a function.")
  }
  intervention(
    intervene = shift_fn,
    mtp = TRUE,
    label = label
  )
}
