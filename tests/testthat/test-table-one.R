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


make_task <- function(d = NULL) {
  if (is.null(d)) d <- make_data()
  initiate_study(d, confounders = c(X1, X2), verbose = FALSE, confounder_labels = c("Confounder 1", "Confounder 2")) |>
    add_treatment(A, label = "Treatment") |>
    add_outcome(Y, "Y")
}

# Create table 1
test_that("create_table_one rejects non-enact_task", {
  expect_error(create_table_one("not_a_task"), "enact_task")
})

test_that("create_table_one rejects bad use_weights", {
  task <- make_task()
  expect_error(create_table_one(task, use_weights = "yes"),
               "single logical")
})

test_that("create_table_one rejects IPW with multiple strata", {
  skip_if_not_installed("gtsummary")
  task <- make_task()
  expect_error(
    create_table_one(task, stratify = c("A", "A"), use_weights = TRUE),
    "single stratification"
  )
})

test_that("create_table_one works unstratified", {
  skip_if_not_installed("gtsummary")
  task <- make_task()
  tbl <- create_table_one(task)
  expect_true(!is.null(tbl))
})

test_that("create_table_one works stratified by binary treatment", {
  skip_if_not_installed("gtsummary")
  task <- make_task()
  tbl <- create_table_one(task, stratify = "A")
  expect_true(!is.null(tbl))
})

test_that("create_table_one works with tidyselect vars subset", {
  skip_if_not_installed("gtsummary")
  task <- make_task()
  tbl <- create_table_one(task, vars = c(X1))
  expect_true(!is.null(tbl))
})

test_that("create_table_one works with character vars subset", {
  skip_if_not_installed("gtsummary")
  task <- make_task()
  tbl <- create_table_one(task, vars = "X1")
  expect_true(!is.null(tbl))
})

test_that("create_table_one errors on bad vars column", {
  skip_if_not_installed("gtsummary")
  task <- make_task()
  expect_error(create_table_one(task, vars = "nonexistent"), "not found")
})

test_that("create_table_one errors on non-character stratify", {
  skip_if_not_installed("gtsummary")
  task <- make_task()
  expect_error(create_table_one(task, stratify = 42), "character vector")
})