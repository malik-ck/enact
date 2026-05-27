# â”€â”€ Fixtures â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€

make_fake_stripped <- function(with_table_one = FALSE) {
  tmle_df <- data.frame(
    Contrast = c("TSM - treat", "TSM - control", "ATE (treat vs control)"),
    Estimate = c(0.42, 0.30, 0.12),
    SE       = c(0.03, 0.03, 0.04),
    Lower_CI = c(0.36, 0.24, 0.04),
    Upper_CI = c(0.48, 0.36, 0.20),
    stringsAsFactors = FALSE
  )
  smd_df <- data.frame(
    confounder = c("X1", "X2"),
    smd        = c(0.05, -0.12),
    abs_smd    = c(0.05, 0.12),
    violation  = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  blank_plot <- if (requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
      ggplot2::geom_point()
  } else NULL
  iv_results <- list(
    treat = list(
      smd_df       = smd_df,
      love_plot    = blank_plot,
      density_plot = blank_plot,
      n_violations = 1L
    )
  )
  balance <- structure(
    list(
      threshold = 0.1,
      outcomes  = list(Y = list(
        interventions    = iv_results,
        max_smd          = 0.12,
        total_violations = 1L
      ))
    ),
    class = "enact_balance_check"
  )
  task <- list(
    tmle_results   = list(Y = tmle_df),
    balance_checks = balance,
    table_one      = NULL
  )
  if (with_table_one) {
    # Caller is responsible for ensuring gtsummary is available.
    df <- data.frame(X1 = rnorm(20L), X2 = rnorm(20L))
    task$table_one <- gtsummary::tbl_summary(df)
  }
  class(task) <- "enact_task_stripped"
  task
}

skip_if_no_pandoc <- function() {
  if (!rmarkdown::pandoc_available()) testthat::skip("pandoc not available")
}


# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# Dispatch & input validation
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•

test_that("report() errors on objects without a method", {
  expect_error(report("not_a_task", tempfile(fileext = ".docx")),
               "no applicable method|UseMethod")
})

test_that("report() rejects non-string output_file", {
  task <- make_fake_stripped()
  expect_error(report(task, 1L), "single file path")
})

test_that("report() rejects extension/format mismatch", {
  task <- make_fake_stripped()
  expect_error(report(task, tempfile(fileext = ".pdf"), format = "docx"),
               "does not match format")
  expect_error(report(task, tempfile(fileext = ".docx"), format = "tex"),
               "does not match format")
})

test_that("report() accepts .latex for format = 'tex'", {
  skip_if_no_pandoc()
  task <- make_fake_stripped()
  out  <- tempfile(fileext = ".latex")
  # Validation should pass; render itself may still fail if LaTeX absent,
  # so test the validation path only.
  expect_error(
    report(task, out, format = "tex", template = "/nonexistent.tex"),
    "path does not exist"
  )
})

test_that("report() rejects non-logical include_* flags", {
  task <- make_fake_stripped()
  expect_error(
    report(task, tempfile(fileext = ".docx"), include_love_plots = "yes"),
    "TRUE or FALSE"
  )
  expect_error(
    report(task, tempfile(fileext = ".docx"), include_density_plots = NA),
    "TRUE or FALSE"
  )
})


# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# Template resolution
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•

test_that("resolve_template(NULL, docx) returns a word_document format", {
  ofmt <- resolve_template("docx", NULL)
  expect_s3_class(ofmt, "rmarkdown_output_format")
})

test_that("resolve_template(NULL, tex) returns a latex_document format", {
  ofmt <- resolve_template("tex", NULL)
  expect_s3_class(ofmt, "rmarkdown_output_format")
})

test_that("resolve_template passes through an output_format object", {
  fmt <- rmarkdown::word_document()
  expect_identical(resolve_template("docx", fmt), fmt)
})

test_that("resolve_template rejects nonexistent paths", {
  expect_error(resolve_template("docx", "/no/such/file.docx"),
               "path does not exist")
})

test_that("resolve_template rejects extension/format mismatch", {
  tmp <- tempfile(fileext = ".tex"); file.create(tmp); on.exit(unlink(tmp))
  expect_error(resolve_template("docx", tmp), "must be a .docx file")
  tmp2 <- tempfile(fileext = ".docx"); file.create(tmp2); on.exit(unlink(tmp2), add = TRUE)
  expect_error(resolve_template("tex", tmp2), "must be a .tex or .latex file")
})

test_that("resolve_template accepts a valid .docx path", {
  tmp <- tempfile(fileext = ".docx"); file.create(tmp); on.exit(unlink(tmp))
  ofmt <- resolve_template("docx", tmp)
  expect_s3_class(ofmt, "rmarkdown_output_format")
})

test_that("resolve_template accepts a valid .tex path", {
  tmp <- tempfile(fileext = ".tex"); file.create(tmp); on.exit(unlink(tmp))
  ofmt <- resolve_template("tex", tmp)
  expect_s3_class(ofmt, "rmarkdown_output_format")
})

test_that("resolve_template rejects nonsense", {
  expect_error(resolve_template("docx", 42L),
               "NULL, a single file path")
})


# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# Template identity & scaffolding
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•

test_that("template_identity recovers (name, package) for an rticles format", {
  skip_if_not_installed("rticles")
  id <- template_identity(rticles::sage_article())
  expect_identical(id, list(name = "sage", package = "rticles"))
})

test_that("template_identity returns NULL for non-template formats", {
  expect_null(template_identity(rmarkdown::latex_document()))
  expect_null(template_identity(rmarkdown::word_document()))
  expect_null(template_identity("not a format object"))
  tmp <- tempfile(fileext = ".tex"); file.create(tmp); on.exit(unlink(tmp))
  expect_null(template_identity(rmarkdown::latex_document(template = tmp)))
})

test_that("scaffold_template_assets is a no-op for non-template formats", {
  work <- tempfile("scaffold_"); dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  res <- scaffold_template_assets(rmarkdown::latex_document(), work)
  expect_false(res)
  expect_length(list.files(work), 0L)
})

test_that("scaffold_template_assets materialises rticles skeleton files", {
  skip_if_not_installed("rticles")
  work <- tempfile("scaffold_"); dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  res <- scaffold_template_assets(rticles::sage_article(), work)
  expect_true(res)
  files <- list.files(work)
  expect_true(any(grepl("sagej\\.cls$", files)))
  expect_true(any(grepl("\\.bst$",      files)))
  expect_false(any(grepl("\\.scaffold\\.", files)))
})


# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# Rmd assembly (no rendering)
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•

test_that("build_rmd contains expected section headings", {
  task <- make_fake_stripped()
  rmd <- build_rmd(task, FALSE, FALSE)
  for (h in c("# Introduction", "# Methods", "# Results",
              "# Discussion", "# Appendix")) {
    expect_match(rmd, h, fixed = TRUE)
  }
})

test_that("build_rmd embeds balance summary when balance_checks present", {
  task <- make_fake_stripped()
  rmd  <- build_rmd(task, FALSE, FALSE)
  expect_match(rmd, "balance-summary", fixed = TRUE)
  expect_false(grepl("love-plots", rmd, fixed = TRUE))
})

test_that("build_rmd embeds love plot chunk when requested", {
  task <- make_fake_stripped()
  rmd  <- build_rmd(task, TRUE, FALSE)
  expect_match(rmd, "love-plots", fixed = TRUE)
})

test_that("build_rmd embeds density plot chunk when requested", {
  task <- make_fake_stripped()
  rmd  <- build_rmd(task, FALSE, TRUE)
  expect_match(rmd, "density-plots", fixed = TRUE)
})

test_that("build_rmd falls back for missing Table 1 / balance / tmle", {
  task <- structure(
    list(tmle_results = list(), balance_checks = NULL, table_one = NULL),
    class = "enact_task_stripped"
  )
  rmd <- build_rmd(task, FALSE, FALSE)
  expect_match(rmd, "No Table 1 found",      fixed = TRUE)
  expect_match(rmd, "No TMLE results found", fixed = TRUE)
  expect_match(rmd, "No balance checks",     fixed = TRUE)
})


# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•
# Integration: actually render
# â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•

test_that("report() renders a docx without Table 1", {
  skip_on_cran()
  skip_if_no_pandoc()
  task <- make_fake_stripped(with_table_one = FALSE)
  out  <- tempfile(fileext = ".docx")
  on.exit(unlink(out))
  res <- report(task, out, format = "docx")
  expect_true(file.exists(res))
  expect_gt(file.info(res)$size, 0)
})

zip_entries <- function(path) {
  if (requireNamespace("zip", quietly = TRUE)) zip::zip_list(path)$filename
  else utils::unzip(path, list = TRUE)$Name
}

test_that("report() bundles a tex project as .zip with figures", {
  skip_on_cran()
  skip_if_no_pandoc()
  skip_if_not_installed("ggplot2")
  task <- make_fake_stripped(with_table_one = FALSE)
  out  <- tempfile(fileext = ".zip")
  on.exit(unlink(out))
  res <- report(task, out, format = "tex", include_love_plots = TRUE)
  expect_true(file.exists(res))
  entries <- zip_entries(res)
  expect_true(any(grepl("report\\.tex$",   entries)))
  expect_true(any(grepl("figure-latex/",   entries)))
  expect_false(any(grepl("\\.Rmd$",        entries)))
  expect_false(any(grepl("\\.scaffold\\.", entries)))
})

test_that("report() with rticles bundles companion .cls/.bst into the zip", {
  skip_on_cran()
  skip_if_no_pandoc()
  skip_if_not_installed("rticles")
  skip_if_not_installed("kableExtra")
  skip_if_not_installed("gtsummary")
  task <- make_fake_stripped(with_table_one = TRUE)
  out  <- tempfile(fileext = ".zip")
  on.exit(unlink(out))
  res <- report(task, out, format = "tex",
                template = rticles::sage_article(),
                include_love_plots = TRUE, include_density_plots = TRUE)
  entries <- zip_entries(res)
  expect_true(any(grepl("report\\.tex$",  entries)))
  expect_true(any(grepl("figure-latex/",  entries)))
  expect_true(any(grepl("sagej\\.cls$",   entries)))
  expect_true(any(grepl("\\.bst$",        entries)))
})

test_that("report() renders with love plots in the appendix", {
  skip_on_cran()
  skip_if_no_pandoc()
  skip_if_not_installed("ggplot2")
  task <- make_fake_stripped(with_table_one = FALSE)
  out  <- tempfile(fileext = ".docx")
  on.exit(unlink(out))
  res <- report(task, out, format = "docx", include_love_plots = TRUE)
  expect_true(file.exists(res))
})

test_that("report() errors when kableExtra is missing for tex + Table 1", {
  # Hard to mock removal of an installed package portably; smoke-test the
  # branch by stubbing requireNamespace.
  skip_if_not_installed("mockery")
  task <- make_fake_stripped(with_table_one = FALSE)
  task$table_one <- structure(list(), class = "gtsummary")
  mockery::stub(report.enact_task_stripped, "requireNamespace", function(...) FALSE)
  expect_error(
    report.enact_task_stripped(task, tempfile(fileext = ".tex"), format = "tex"),
    "Package 'kableExtra' is required"
  )
})
