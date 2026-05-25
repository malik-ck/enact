
# Standard contrast constructors for interventions.
# Each returns a function(reference, treatment) with a "label" attribute.

ate <- function() {
  fn <- function(reference, treatment) treatment - reference
  attr(fn, "label") <- "ATE"
  fn
}

log_relative_ate <- function() {
  fn <- function(reference, treatment) log(treatment) - log(reference)
  attr(fn, "label") <- "Log Relative ATE"
  fn
}

log_odds_ratio <- function() {
  fn <- function(reference, treatment) {
    log(treatment / (1 - treatment)) - log(reference / (1 - reference))
  }
  attr(fn, "label") <- "Log Odds Ratio"
  fn
}
