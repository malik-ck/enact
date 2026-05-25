#' Generate a report from a stripped enact_task
#'
#' Renders a Word (.docx) or LaTeX (.tex) document containing Table 1, TMLE
#' results, and balance diagnostics held in a \code{enact_task_stripped}. The
#' document is laid out under \emph{Introduction / Methods / Results /
#' Discussion / Appendix} headings; non-result sections contain placeholder
#' prose for the user to fill in.
#'
#' @param task A \code{enact_task_stripped} object (see \code{\link{strip}}).
#' @param ... Passed to methods.
#' @return The output file path, invisibly.
#' @export
report <- function(task, ...) UseMethod("report")


#' @param output_file Output file path. For \code{format = "docx"} must
#'   end in \code{.docx}. For \code{format = "tex"} accepted extensions are
#'   \code{.zip}, \code{.tex}, or \code{.latex}; the file on disk is always
#'   a zip archive bundling the \code{.tex} file with its
#'   \code{report_files/} figure directory (and any template companion
#'   files such as \code{.cls}/\code{.bst} when a journal template like
#'   \code{rticles::sage_article()} is used) so the project can be uploaded
#'   directly to Overleaf.
#' @param format Either \code{"docx"} or \code{"tex"}.
#' @param template Optional. One of: (i) \code{NULL} for plain output;
#'   (ii) a path to a reference \code{.docx} (when \code{format = "docx"})
#'   or a pandoc LaTeX \code{.tex}/\code{.latex} template (when
#'   \code{format = "tex"}); or (iii) an already-built \code{rmarkdown}
#'   output format object such as \code{rticles::arxiv_article()}. When
#'   (iii) corresponds to an rmarkdown package template (rticles,
#'   prettydoc, etc.), its skeleton files (\code{.cls}/\code{.bst}/\code{.bib})
#'   are scaffolded via \code{rmarkdown::draft()} and included in the zip.
#' @param include_love_plots,include_density_plots Logical. Embed
#'   balance-check love plots / clever-covariate density plots in the
#'   Appendix. Default \code{FALSE}.
#' @rdname report
#' @export
report.enact_task_stripped <- function(task, output_file,
                                       format = c("docx", "tex"),
                                       template = NULL,
                                       include_love_plots = FALSE,
                                       include_density_plots = FALSE,
                                       ...) {
  format <- match.arg(format)
  if (!is.character(output_file) || length(output_file) != 1L) {
    stop("`output_file` must be a single file path.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(output_file))
  valid_exts <- if (format == "docx") "docx" else c("zip", "tex", "latex")
  if (!ext %in% valid_exts) {
    stop(sprintf(
      "`output_file` extension '.%s' does not match format '%s' (expected %s).",
      ext, format, paste(sprintf("'.%s'", valid_exts), collapse = "/")
    ), call. = FALSE)
  }
  if (!is.logical(include_love_plots) || length(include_love_plots) != 1L ||
      is.na(include_love_plots)) {
    stop("`include_love_plots` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(include_density_plots) || length(include_density_plots) != 1L ||
      is.na(include_density_plots)) {
    stop("`include_density_plots` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!rmarkdown::pandoc_available()) {
    stop("`pandoc` is required to render reports.", call. = FALSE)
  }
  if (!is.null(task$table_one)) {
    pkg <- if (format == "tex") "kableExtra" else "flextable"
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf(
        "Package '%s' is required to render Table 1 to %s.", pkg, format
      ), call. = FALSE)
    }
  }

  ofmt <- resolve_template(format, template)
  rmd_text <- build_rmd(task, include_love_plots, include_density_plots)

  tmp_dir <- tempfile("enact_report_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  rmd_path <- file.path(tmp_dir, "report.Rmd")
  writeLines(rmd_text, rmd_path)

  env <- new.env(parent = globalenv())
  env$task <- task
  env$rmd_format <- format

  output_file <- normalizePath(output_file, mustWork = FALSE)
  if (format == "docx") {
    rmarkdown::render(
      input         = rmd_path,
      output_format = ofmt,
      output_file   = output_file,
      envir         = env,
      quiet         = TRUE
    )
  } else {
    # Step 1: scaffold template companion files (.cls/.bst/.bib) via
    # rmarkdown::draft() — pdflatex would normally pull them in during
    # compilation, but we skip compilation so we must materialise them
    # ourselves. No-op for non-package templates.
    # Step 2: render to .tex only. Passing output_file with a .tex
    # extension makes pandoc derive its target from the extension and
    # skip pdflatex even for PDF-producing formats (rticles etc.).
    # Step 3: clean = FALSE preserves report_files/figure-latex/.
    # Step 4: bundle_tex_zip sweeps tmp_dir into the user's archive.
    scaffold_template_assets(ofmt, tmp_dir)
    rmarkdown::render(
      input         = rmd_path,
      output_format = ofmt,
      output_file   = "report.tex",
      envir         = env,
      clean         = FALSE,
      quiet         = TRUE
    )
    bundle_tex_zip(tmp_dir, output_file)
  }
  invisible(output_file)
}


# ── Template resolution ─────────────────────────────────────────────────────

resolve_template <- function(format, template) {
  if (is.null(template)) {
    return(if (format == "docx") rmarkdown::word_document()
           else                    rmarkdown::latex_document())
  }
  if (inherits(template, "rmarkdown_output_format")) return(template)
  if (!(is.character(template) && length(template) == 1L)) {
    stop("`template` must be NULL, a single file path, or an rmarkdown output format object.",
         call. = FALSE)
  }
  if (!file.exists(template)) {
    stop(sprintf("`template` path does not exist: %s", template), call. = FALSE)
  }
  ext <- tolower(tools::file_ext(template))
  if (format == "docx") {
    if (ext != "docx") {
      stop(sprintf(
        "Template for format = 'docx' must be a .docx file; got '.%s'.", ext
      ), call. = FALSE)
    }
    return(rmarkdown::word_document(reference_docx = template))
  }
  if (!ext %in% c("tex", "latex")) {
    stop(sprintf(
      "Template for format = 'tex' must be a .tex or .latex file; got '.%s'.", ext
    ), call. = FALSE)
  }
  rmarkdown::latex_document(template = template)
}


# ── Template asset scaffolding ──────────────────────────────────────────────

scaffold_template_assets <- function(ofmt, work_dir) {
  id <- template_identity(ofmt)
  if (is.null(id)) return(invisible(FALSE))
  scaffold <- file.path(work_dir, ".scaffold.Rmd")
  ok <- tryCatch({
    rmarkdown::draft(scaffold, template = id$name, package = id$package,
                     create_dir = FALSE, edit = FALSE)
    TRUE
  }, error = function(e) {
    warning("Template asset scaffolding failed; companion files ",
            "(.cls/.bst) may be missing from the archive: ",
            conditionMessage(e), call. = FALSE)
    FALSE
  })
  if (file.exists(scaffold)) unlink(scaffold)
  invisible(ok)
}

template_identity <- function(ofmt) {
  if (!inherits(ofmt, "rmarkdown_output_format")) return(NULL)
  tmpl <- ofmt$pandoc$template
  if (is.null(tmpl) || !nzchar(tmpl)) {
    args <- ofmt$pandoc$args
    i <- which(args == "--template")
    if (!length(i) || i[1] >= length(args)) return(NULL)
    tmpl <- args[i[1] + 1]
  }
  if (!file.exists(tmpl)) return(NULL)
  parts <- strsplit(normalizePath(tmpl, winslash = "/", mustWork = FALSE),
                    "/", fixed = TRUE)[[1]]
  n <- length(parts)
  if (n < 6) return(NULL)
  if (parts[n - 1] != "resources" || parts[n - 3] != "templates" ||
      parts[n - 4] != "rmarkdown") return(NULL)
  list(name = parts[n - 2], package = parts[n - 5])
}


# ── Tex bundling ────────────────────────────────────────────────────────────

bundle_tex_zip <- function(work_dir, zip_path) {
  all_files <- list.files(work_dir, recursive = TRUE, all.files = FALSE)
  # Drop the Rmd source, pandoc intermediate markdown, and LaTeX build artifacts.
  exclude_pat <- paste0(
    "(^|/)report\\.Rmd$",
    "|\\.knit\\.md$|\\.utf8\\.md$",
    "|\\.(log|aux|out|toc|lof|lot|nav|snm|vrb|fls|fdb_latexmk|bbl|blg)$",
    "|\\.synctex\\.gz$"
  )
  files <- all_files[!grepl(exclude_pat, all_files)]
  if (!length(files)) {
    stop("No rendered files found to bundle.", call. = FALSE)
  }
  if (file.exists(zip_path)) unlink(zip_path)
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(zipfile = zip_path, files = files, root = work_dir)
  } else {
    old_wd <- setwd(work_dir); on.exit(setwd(old_wd), add = TRUE)
    rc <- utils::zip(zipfile = zip_path, files = files, flags = "-r9Xq")
    if (rc != 0L) {
      stop("Bundling the .tex project failed. Install the 'zip' package ",
           "or ensure a 'zip' executable is on PATH.", call. = FALSE)
    }
  }
  invisible(zip_path)
}


# ── Rmd assembly ────────────────────────────────────────────────────────────

build_rmd <- function(task, include_love_plots, include_density_plots) {
  paste(c(
    "---",
    "title: \"Target Trial Emulation Report\"",
    "---",
    "",
    "# Introduction", "",
    "_Provide a brief overview of the research question, target population, and motivation for the study._",
    "",
    "# Methods", "",
    "_Describe the data source, target trial protocol, interventions, outcomes, and analysis strategy (e.g. TMLE with fractional-weighted-bootstrap inference)._",
    "",
    "# Results", "",
    "## Baseline characteristics", "",
    chunk_table_one(task), "",
    "## Treatment effect estimates", "",
    chunk_tmle(task), "",
    "# Discussion", "",
    "_Interpret the findings, address limitations, and outline implications._",
    "",
    "# Appendix: Balance diagnostics", "",
    chunk_balance(task, include_love_plots, include_density_plots)
  ), collapse = "\n")
}


# ── Section chunk builders ──────────────────────────────────────────────────

chunk_table_one <- function(task) {
  if (is.null(task$table_one)) return("_No Table 1 found in this task._")
  c("```{r table-one, echo=FALSE}",
    "if (rmd_format == \"tex\") gtsummary::as_kable_extra(task$table_one) else gtsummary::as_flex_table(task$table_one)",
    "```")
}

chunk_tmle <- function(task) {
  if (length(task$tmle_results) == 0L) return("_No TMLE results found in this task._")
  c("```{r tmle-tables, echo=FALSE, results='asis'}",
    "for (nm in names(task$tmle_results)) {",
    "  cat(sprintf(\"\\n\\n**Outcome: %s**\\n\\n\", nm))",
    "  print(knitr::kable(task$tmle_results[[nm]], caption = sprintf(\"TMLE results: %s\", nm), digits = 3))",
    "}",
    "```")
}

chunk_balance <- function(task, include_love_plots, include_density_plots) {
  if (is.null(task$balance_checks)) {
    return("_No balance checks found in this task._")
  }
  out <- c("## Balance summary", "",
    "```{r balance-summary, echo=FALSE}",
    "knitr::kable(summary(task$balance_checks), caption = \"Balance check summary\", digits = 3)",
    "```")
  if (include_love_plots) {
    out <- c(out, "", "## Love plots", "",
      "```{r love-plots, echo=FALSE, results='asis', fig.height=4, fig.width=6}",
      "for (on in names(task$balance_checks$outcomes)) {",
      "  for (iv in names(task$balance_checks$outcomes[[on]]$interventions)) {",
      "    cat(sprintf(\"\\n\\n**%s — %s**\\n\\n\", on, iv))",
      "    print(task$balance_checks$outcomes[[on]]$interventions[[iv]]$love_plot)",
      "  }",
      "}",
      "```")
  }
  if (include_density_plots) {
    out <- c(out, "", "## Clever covariate density plots", "",
      "```{r density-plots, echo=FALSE, results='asis', fig.height=4, fig.width=6}",
      "for (on in names(task$balance_checks$outcomes)) {",
      "  for (iv in names(task$balance_checks$outcomes[[on]]$interventions)) {",
      "    cat(sprintf(\"\\n\\n**%s — %s**\\n\\n\", on, iv))",
      "    print(task$balance_checks$outcomes[[on]]$interventions[[iv]]$density_plot)",
      "  }",
      "}",
      "```")
  }
  out
}
