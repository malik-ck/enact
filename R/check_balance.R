# ══════════════════════════════════════════════════════════════════════════════
# check_balance — per-outcome covariate balance diagnostics
# ══════════════════════════════════════════════════════════════════════════════

#' Check covariate balance after fitting interventions
#'
#' For each outcome and non-reference intervention, computes standardized mean
#' differences (SMDs) in confounders using clever covariates as IPW weights.
#' Produces love plots and clever covariate density plots.  Results are stored
#' in \code{task$balance_checks} as an \code{enact_balance_check} object.
#'
#' @param task An \code{enact_task} with clever covariates already populated
#'   (i.e. after \code{\link{fit_interventions}}).
#' @param threshold Numeric.  SMD cutoff for counting balance violations.
#'   Defaults to \code{0.1}.
#'
#' @return The modified \code{enact_task} with \code{balance_checks} populated.
#' @importFrom rlang .data
#' @export
check_balance <- function(task, threshold = 0.1) {
  if (!inherits(task, "enact_task")) {
    stop("`task` must be an enact_task object.", call. = FALSE)
  }
  if (is.null(task$clever_covariates)) {
    stop(
      "No clever covariates found. Call fit_interventions() first.",
      call. = FALSE
    )
  }
  if (is.null(task$interventions)) {
    stop(
      "No interventions defined. Call define_interventions() first.",
      call. = FALSE
    )
  }
  if (!is.numeric(threshold) || length(threshold) != 1L || threshold <= 0) {
    stop("`threshold` must be a single positive number.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }

  x_data <- task$data_env$data
  ref_iv <- task$reference_intervention
  iv_nms <- names(task$interventions)
  other_iv <- setdiff(iv_nms, ref_iv)
  out_nms <- names(task$clever_covariates)

  # Extract observed treatment columns
  trt_col_nms <- unlist(lapply(task$treatment_meta, names))
  trt_data <- as.data.frame(x_data[, trt_col_nms, drop = FALSE])

  result <- vector("list", length(out_nms))
  names(result) <- out_nms

  for (out_nm in out_nms) {
    H_mat <- task$clever_covariates[[out_nm]]
    H_ref <- H_mat[, ref_iv]

    # Resolve adjustment set
    adj_idx <- task$adjustment_sets[[out_nm]]
    conf_cols <- if (is.null(adj_idx)) {
      task$confounder_cols
    } else {
      task$confounder_cols[adj_idx]
    }
    W <- as.data.frame(x_data[, conf_cols, drop = FALSE])
    conf_labels <- vapply(
      conf_cols,
      function(cn) {
        task$confounder_labels[[cn]] %||% cn
      },
      character(1L)
    )

    iv_results <- vector("list", length(other_iv))
    names(iv_results) <- other_iv

    for (iv_nm in other_iv) {
      H_a <- H_mat[, iv_nm]
      smd_df <- compute_smds(W, H_a, H_ref, conf_labels, threshold)

      iv_results[[iv_nm]] <- list(
        smd_df = smd_df,
        love_plot = make_love_plot(smd_df, threshold),
        density_plot = make_covariate_density(
          H_a,
          H_ref,
          iv_nm,
          ref_iv,
          task
        ),
        n_violations = sum(smd_df$violation)
      )
    }

    all_viol <- vapply(iv_results, `[[`, integer(1L), "n_violations")
    all_smd <- vapply(
      iv_results,
      function(iv) max(iv$smd_df$abs_smd),
      numeric(1L)
    )

    result[[out_nm]] <- list(
      interventions = iv_results,
      max_smd = max(all_smd),
      total_violations = sum(all_viol)
    )
  }

  task$balance_checks <- structure(
    list(threshold = threshold, outcomes = result),
    class = "enact_balance_check"
  )

  task
}


# ══════════════════════════════════════════════════════════════════════════════
# Helpers
# ══════════════════════════════════════════════════════════════════════════════

compute_smds <- function(W, H_a, H_ref, conf_labels, threshold) {
  n_conf <- ncol(W)
  smds <- numeric(n_conf)

  for (j in seq_len(n_conf)) {
    wj <- W[[j]]
    denom <- stats::sd(wj, na.rm = TRUE)
    if (is.na(denom) || denom < 1e-12) {
      smds[j] <- 0
      next
    }
    mean_a <- sum(H_a * wj, na.rm = TRUE) / sum(H_a, na.rm = TRUE)
    mean_ref <- sum(H_ref * wj, na.rm = TRUE) / sum(H_ref, na.rm = TRUE)
    smds[j] <- (mean_a - mean_ref) / denom
  }

  data.frame(
    confounder = conf_labels,
    smd = smds,
    abs_smd = abs(smds),
    violation = abs(smds) > threshold,
    stringsAsFactors = FALSE
  )
}


# ── Love plot ────────────────────────────────────────────────────────────────

make_love_plot <- function(smd_df, threshold) {
  smd_df <- smd_df[order(smd_df$abs_smd), ]
  smd_df$confounder <- factor(smd_df$confounder, levels = smd_df$confounder)

  ggplot2::ggplot(
    smd_df,
    ggplot2::aes(x = .data$abs_smd, y = .data$confounder)
  ) +
    ggplot2::geom_vline(
      xintercept = threshold,
      linetype = "dashed",
      colour = "grey30"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = 0, yend = .data$confounder),
      linewidth = 0.8,
      colour = "#3B6E8F"
    ) +
    ggplot2::geom_point(size = 2.5, colour = "#3B6E8F") +
    ggplot2::labs(x = "Absolute SMD", y = NULL, title = "Covariate Balance") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}


# ── Clever covariate density plot ────────────────────────────────────────────

make_covariate_density <- function(H_a, H_ref, iv_name, ref_name, task) {
  iv_lab <- task$interventions[[iv_name]]$label %||% iv_name
  ref_lab <- task$interventions[[ref_name]]$label %||% ref_name

  pos_a <- H_a[H_a > 0]
  pos_ref <- H_ref[H_ref > 0]

  df <- data.frame(
    H = c(pos_a, pos_ref),
    Intervention = rep(
      c(paste0("H: ", iv_lab), paste0("H: ", ref_lab)),
      c(length(pos_a), length(pos_ref))
    ),
    stringsAsFactors = FALSE
  )

  prop_zero_a <- mean(H_a <= 0)
  prop_zero_ref <- mean(H_ref <= 0)
  subtitle <- sprintf(
    "Proportion with H ≤ 0 — %s: %.1f%%, %s: %.1f%%",
    iv_lab,
    prop_zero_a * 100,
    ref_lab,
    prop_zero_ref * 100
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$H, fill = .data$Intervention)) +
    ggplot2::geom_density(alpha = 0.4, na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = c("#3B6E8F", "#C44E52")) +
    ggplot2::labs(
      x = "Clever covariate (H > 0)",
      y = "Density",
      title = "Clever Covariate Distribution",
      subtitle = subtitle
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 9, colour = "grey30"),
      legend.position = "bottom"
    )
}


# ══════════════════════════════════════════════════════════════════════════════
# S3 methods
# ══════════════════════════════════════════════════════════════════════════════

#' @export
print.enact_balance_check <- function(x, ...) {
  cat(sprintf("Balance check (threshold = %.2f)\n\n", x$threshold))

  rows <- list()
  for (out_nm in names(x$outcomes)) {
    oc <- x$outcomes[[out_nm]]
    for (iv_nm in names(oc$interventions)) {
      iv <- oc$interventions[[iv_nm]]
      rows[[length(rows) + 1L]] <- data.frame(
        Outcome = out_nm,
        Intervention = iv_nm,
        Max_SMD = max(iv$smd_df$abs_smd),
        Violations = iv$n_violations,
        stringsAsFactors = FALSE
      )
    }
  }
  tbl <- do.call(rbind, rows)
  print(tbl, row.names = FALSE)
  invisible(x)
}

#' @export
summary.enact_balance_check <- function(object, ...) {
  rows <- list()
  for (out_nm in names(object$outcomes)) {
    oc <- object$outcomes[[out_nm]]
    for (iv_nm in names(oc$interventions)) {
      smd_df <- oc$interventions[[iv_nm]]$smd_df
      rows[[length(rows) + 1L]] <- data.frame(
        Outcome = out_nm,
        Intervention = iv_nm,
        smd_df,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}
