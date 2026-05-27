#' Strip everything but the reporting-relevant slots from an enact_task
#'
#' Generic function for removing slots from an \code{enact_task}
#' that are not needed for reporting. This file is safe to send to collaborators
#' without privacy concerns, or to reduce memory footprint before reporting.
#'
#' @param task A study task object (e.g. \code{enact_task}).
#' @param \dots Additional arguments.
#' @return The stripped task.
#' @export
strip <- function(task, ...) {
  UseMethod("strip")
}


#' Strip everything but the reporting-relevant slots from an enact_task
#'
#' Generic function for removing slots from an \code{enact_task}
#' that are not needed for reporting. This file is safe to send to collaborators
#' without privacy concerns, or to reduce memory footprint before reporting.
#'
#'' @param task An \code{enact_task} object.
#' @param keep Optional character vector of slot names to retain in addition to the default reporting slots (\code{tmle_results}, \code{table_one}, and \code{balance_checks}).
#' @return An \code{enact_task} object with only the specified slots retained
#'
#' @export
strip.enact_task <- function(task, keep = NULL) {
  slots_to_keep <- c(
    "tmle_results",
    "table_one",
    "balance_checks"
  )
  if (!is.null(keep)) {
    if (!is.character(keep)) {
      stop("`keep` must be a character vector of slot names to retain.")
    }

    bad_slots <- setdiff(keep, names(task))
    if (length(bad_slots) > 0) {
      stop(sprintf(
        "The following slots do not exist on enact_task: %s",
        paste(bad_slots, collapse = ", ")
      ))
    }
    slots_to_keep <- unique(c(slots_to_keep, keep))
  }
  for (slot in setdiff(names(task), slots_to_keep)) {
    task[[slot]] <- NULL
  }

  # Change class, invalidating old methods and all that
  class(task) <- "enact_task_stripped"
  task
}

# Little printer
#' @export
print.enact_task_stripped <- function(x, ...) {
  cat("An enact_task object with the following slots:\n")
  print(names(x))
  invisible(x)
}


#' @export
summary.enact_task_stripped <- function(object, digits = 3L, ...) {
  print_tmle_results(object$tmle_results, digits = digits)
  invisible(object)
}