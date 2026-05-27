# â”€â”€ Helper â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
make_data <- function(n = 100L) {
  set.seed(42L)
  data.frame(
    X1 = rnorm(n),
    X2 = rnorm(n),
    A  = rbinom(n, 1L, 0.5),
    Y  = rnorm(n),
    Z  = rnorm(n),
    cl = sample(letters[1:4], n, replace = TRUE)
  )
}


# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# NAs require explicit censoring
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•

test_that("NAs in outcome without censoring column produce an error", {
  d <- make_data(50L)
  d$Y[c(2, 5, 10)] <- NA

  task <- initiate_study(d, confounders = X1, verbose = FALSE) |>
    add_treatment(A)
  expect_error(
    add_outcome(task, Y, "Y"),
    "missing value.*no censoring"
  )
})

test_that("censoring column must cover all NAs in outcome", {
  d <- make_data(50L)
  d$C <- 1L
  d$C[3] <- 0L
  d$Y[8] <- NA  # not covered by C

  task <- initiate_study(d, confounders = X1, verbose = FALSE) |>
    add_treatment(A)
  expect_error(
    add_outcome(task, Y, "Y", censoring = C),
    "NA in outcome but censoring.*is 1"
  )
})

test_that("no censoring created when outcome has no NAs", {
  d <- make_data(50L)
  task <- initiate_study(d, confounders = X1, verbose = FALSE) |>
    add_treatment(A) |>
    add_outcome(Y, "Y")

  expect_length(task$censoring, 0L)
  expect_false("Y" %in% names(task$censoring))
})

# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# Manual censoring via add_outcome(censoring = ...)
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•

test_that("manual censoring column is picked up by add_outcome()", {
  d <- make_data(50L)
  d$C <- 1L
  d$C[c(3, 7)] <- 0L

  task <- initiate_study(d, confounders = X1, verbose = FALSE) |>
    add_treatment(A) |>
    add_outcome(Y, "Y", censoring = C)

  expect_true("Y" %in% names(task$censoring))
  expect_equal(task$censoring$Y[c(3, 7)], c(0L, 0L))
  expect_equal(task$censoring$Y[1], 1L)
})

test_that("manual censoring with no NAs in outcome works", {
  d <- make_data(50L)
  d$C <- 1L
  d$C[c(2, 4)] <- 0L
  # No NAs in Y

  task <- initiate_study(d, confounders = X1, verbose = FALSE) |>
    add_treatment(A) |>
    add_outcome(Y, "Y", censoring = C)

  expect_true("Y" %in% names(task$censoring))
  expect_length(task$censoring$Y, nrow(d))
  expect_equal(task$censoring$Y[c(2, 4)], c(0L, 0L))
  expect_equal(sum(task$censoring$Y == 0L), 2L)
})

test_that("censoring column covers outcome NAs exactly", {
  d <- make_data(50L)
  d$C <- 1L
  d$C[c(3, 8)] <- 0L
  d$Y[8] <- NA  # covered by C

  task <- initiate_study(d, confounders = X1, verbose = FALSE) |>
    add_treatment(A) |>
    add_outcome(Y, "Y", censoring = C)

  expect_true("Y" %in% names(task$censoring))
  expect_equal(task$censoring$Y[3], 0L)
  expect_equal(task$censoring$Y[8], 0L)
  expect_equal(task$censoring$Y[1], 1L)
  expect_equal(sum(task$censoring$Y == 0L), 2L)
})

test_that("manual censoring: non-existent column name errors", {
  d <- make_data(50L)

  task <- initiate_study(d, confounders = X1, verbose = FALSE) |>
    add_treatment(A)
  expect_error(
    add_outcome(task, Y, "Y", censoring = NonExistent),
    "Cannot resolve|not found|doesn't exist"
  )
})

test_that("manual censoring via column index", {
  d <- make_data(50L)
  d$C <- 1L
  d$C[c(3, 7)] <- 0L

  task <- initiate_study(d, confounders = X1, verbose = FALSE) |>
    add_treatment(A) |>
    # C is column 7 in d (X1, X2, A, Y, Z, cl, C)
    add_outcome(Y, "Y", censoring = 7)

  expect_true("Y" %in% names(task$censoring))
  expect_equal(task$censoring$Y[c(3, 7)], c(0L, 0L))
  expect_equal(task$censoring$Y[1], 1L)
})

test_that("censoring stored as integer vector, not data.frame", {
  d <- make_data(50L)
  d$C <- 1L
  d$C[5] <- 0L

  task <- initiate_study(d, confounders = X1, verbose = FALSE) |>
    add_treatment(A) |>
    add_outcome(Y, "Y", censoring = C)

  expect_true(is.integer(task$censoring$Y))
  expect_false(is.data.frame(task$censoring$Y))
})

test_that("censoring values outside {0, 1} produce an error", {
  d <- make_data(50L)
  d$C <- 1L
  d$C[1] <- 2L

  task <- initiate_study(d, confounders = X1, verbose = FALSE) |>
    add_treatment(A)
  expect_error(
    add_outcome(task, Y, "Y", censoring = C),
    "must be 0 or 1"
  )
})

test_that("print shows censored count", {
  d <- make_data(50L)
  d$C <- 1L
  d$C[c(1, 2)] <- 0L

  task <- initiate_study(d, confounders = X1, verbose = FALSE) |>
    add_treatment(A) |>
    add_outcome(Y, "Y", censoring = C)
  expect_output(print(task), "censored")
})
