# Standard contrast constructors for interventions.
# Each returns a function(reference, treatment) with a "label" attribute.

#' Average treatment effect contrast
#'
#' Returns a function \code{function(reference, treatment)} that computes the
#' difference \code{treatment - reference}.  Pass to the \code{contrasts}
#' argument of \code{\link{add_outcome}}.
#'
#' @return A function with a \code{"label"} attribute set to \code{"ATE"}.
#' @export
ate <- function() {
  fn <- function(reference, treatment) treatment - reference
  attr(fn, "label") <- "ATE"
  fn
}

#' Log relative average treatment effect contrast
#'
#' Returns a function \code{function(reference, treatment)} that computes
#' \code{log(treatment) - log(reference)}.  Pass to the \code{contrasts}
#' argument of \code{\link{add_outcome}}.
#'
#' @return A function with a \code{"label"} attribute set to \code{"Log Relative ATE"}.
#' @export
log_relative_ate <- function() {
  fn <- function(reference, treatment) log(treatment) - log(reference)
  attr(fn, "label") <- "Log Relative ATE"
  fn
}

#' Log odds ratio contrast
#'
#' Returns a function \code{function(reference, treatment)} that computes the
#' log odds ratio of treatment vs reference.  Both inputs must lie in
#' \code{(0, 1)}.  Pass to the \code{contrasts} argument of
#' \code{\link{add_outcome}}.
#'
#' @return A function with a \code{"label"} attribute set to \code{"Log Odds Ratio"}.
#' @export
log_odds_ratio <- function() {
  fn <- function(reference, treatment) {
    log(treatment / (1 - treatment)) - log(reference / (1 - reference))
  }
  attr(fn, "label") <- "Log Odds Ratio"
  fn
}
