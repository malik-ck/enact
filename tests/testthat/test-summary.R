make_task_with_tmle <- function() {
  tmle_df <- data.frame(
    Contrast = c("TSM - treat", "TSM - control", "ATE (treat vs control)"),
    Estimate = c(0.42, 0.30, 0.12),
    SE       = c(0.03, 0.03, 0.04),
    Lower_CI = c(0.36, 0.24, 0.04),
    Upper_CI = c(0.48, 0.36, 0.20),
    P_value  = c(0.001, 0.002, 0.012),
    stringsAsFactors = FALSE
  )
  structure(list(tmle_results = list(Y = tmle_df)), class = "enact_task")
}

test_that("summary.enact_task prints TMLE results when present", {
  task <- make_task_with_tmle()
  expect_output(summary(task), "Outcome: Y")
  expect_output(summary(task), "TSM - treat")
  expect_output(summary(task), "P_value")
})

test_that("summary.enact_task says so when TMLE results are missing", {
  task <- structure(list(tmle_results = NULL), class = "enact_task")
  expect_output(summary(task), "No TMLE results")
})

test_that("summary.enact_task_stripped prints TMLE results when present", {
  task <- make_task_with_tmle()
  class(task) <- "enact_task_stripped"
  expect_output(summary(task), "Outcome: Y")
  expect_output(summary(task), "ATE")
})

test_that("summary returns the task invisibly", {
  task <- make_task_with_tmle()
  out <- utils::capture.output(ret <- summary(task))
  expect_identical(ret, task)
})
