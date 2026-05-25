
fit_mtp_task <- function(task, intervention_name) {
  if (!inherits(task, "enact_task")) {
    stop("This function is meant for 'enact_task' objects.")
  }
  if (!intervention_name %in% names(task$mtp_tasks)) {
    stop("Specified intervention not found in task's mtp_tasks.")
  }
  if (task$interventions[[intervention_name]]$mtp != TRUE) {
    stop("Specified intervention is not a modified treatment policy.")
  }

  n <- task$n_obs
  mtp_sub <- task$mtp_tasks[[intervention_name]]

  # Stash originals so we can restore after fitting
  original_x_env <- mtp_sub$x_env
  original_y_env <- mtp_sub$y_env
  original_perf <- mtp_sub$cv$performance_sets
  original_build <- mtp_sub$cv$build_sets

  # Get intervened treatment values
  trt_intervened <- intervene(task, intervention_name)

  # Build augmented x: original rows + rows with treatment replaced
  x_aug <- rbind(original_x_env$x, original_x_env$x)
  trt_col_nms <- colnames(trt_intervened)
  x_aug[seq(n + 1L, 2L * n), trt_col_nms] <- as.matrix(trt_intervened)

  # Set censoring indicators to 1 in the intervened half
  cens_col_nms <- vapply(
    task$censoring, attr, character(1L), "cens_col"
  )
  cens_col_nms <- cens_col_nms[!is.na(cens_col_nms)]
  if (length(cens_col_nms)) {
    x_aug[seq(n + 1L, 2L * n), cens_col_nms] <- 1
  }

  # Swap in fresh environments
  aug_x_env <- new.env(parent = emptyenv())
  aug_x_env$x <- x_aug
  mtp_sub$x_env <- aug_x_env

  aug_y_env <- new.env(parent = emptyenv())
  aug_y_env$y <- c(original_y_env$y, rep(1, n))
  mtp_sub$y_env <- aug_y_env

  # Offset CV folds to cover the doubled dataset
  if (!is.null(original_perf)) {
    mtp_sub$cv$performance_sets <- offset_enfold_list(original_perf, n)
  }
  if (!is.null(original_build)) {
    mtp_sub$cv$build_sets <- lapply(
      original_build, offset_enfold_list, n = n
    )
  }

  # Fit the density ratio model on the 2n augmented data
  fitted <- enfold::fit(mtp_sub)

  # Restore original environments and folds on the sub-task
  mtp_sub$x_env <- original_x_env
  mtp_sub$y_env <- original_y_env
  mtp_sub$cv$performance_sets <- original_perf
  mtp_sub$cv$build_sets <- original_build

  # fit() modified mtp_sub in place, which triggered a copy (R copy-on-modify).
  # The returned `fitted` object is the copy with 2n data/folds baked in.
  # Restore its environments and cv to the original n-row data so predict()
  # works correctly with n-row newdata.
  fitted$x_env <- original_x_env
  fitted$y_env <- original_y_env
  fitted$cv$performance_sets <- original_perf
  fitted$cv$build_sets <- original_build

  task$mtp_tasks[[intervention_name]] <- fitted
  task
}

# Little helper that takes enfold_fold_lists and offsets by n
offset_enfold_list <- function(folds, n) {
  new_folds <- lapply(
    folds,
    function(x, n) {
      x$validation_set <- append(x$validation_set, x$validation_set + n)
      if (!is.null(x$training_set)) {
        x$training_set <- append(x$training_set, n + x$training_set)
      }
      if (!is.null(x$excluded)) {
        x$excluded <- append(x$excluded, n + x$excluded)
      }
      x$n <- x$n + n
      x
    },
    n = n
  )
  class(new_folds) <- class(folds)
  new_folds
}
