
# Draw from a uniform dirichlet distribution
r_unif_dirichlet <- function(n, k) {

  if (n <= 0 || k <= 1) {
    stop("n must be positive and k must be at least 2")
  }

  # Generate n x k matrix of gamma(1,1) i.e. exp(1) random variables
  gamma_samples <- matrix(rexp(n * k, rate = 1), nrow = k, ncol = n)

  # Normalize each row to sum to 1
  return(gamma_samples / (colSums(gamma_samples) / k))

}

# Write a transform-type function that applies to variable names
quick_transform <- function(data, name, replacement) {
  data[,name] <- replacement
  return(data)
}

# Some typechecking for fwb_tmle_bin
typecheck_lazy_sl_tmle <- function(model_obj, metalearner_type) {

  if ("LazySL" %in% class(model_obj)) {

    if (is.null(metalearner_type) & model_obj$was_cv_ensemble == FALSE & model_obj$metalearner_count > 1) {

      stop("A LazySL-object with unspecified metalearner detected.\nIf your ensemble is not cross-validated and you have multiple metalearners,\nyou need to specify a metalearner name in metalearner_treatment.")

    }

    if (is.null(metalearner_type) & model_obj$was_cv_ensemble == TRUE & model_obj$metalearner_count > 1) {

      warning("A LazySL-object with multiple metalearners detected, but none specified. Defaults to the best as determined via out-of-sample loss.")

    }


  }

  if (!is.null(model_obj) & sum(class(model_obj) %in% c("LazySL", "glm", "weightit")) == 0) {

    stop("Please ensure that model objects are of class LazySL, glm, or weightit.")

  }

  return(NULL)

}

typechecks_fwb_tmle_bin <- function(treatment_model, or_full, treatment_name,
                                    metalearner_treatment, metalearner_outcome, trim_ipw, n_bstrap, custom_parameters) {

  if (is.null(treatment_name)) stop("Please provide a treatment name.")
  if (is.null(or_full)) stop("Please provide an outcome model.")

  if (!is.null(custom_parameters) & !is.list(custom_parameters)) stop("custom_parameters has to be NULL or a list.")

  if (is.list(custom_parameters) & length(custom_parameters) > 0) {

    for (i in 1:length(custom_parameters)) {
      if (!is.function(custom_parameters[[i]])) stop("If you pass a list for custom_parameters, each contained element needs to be a function.")
    }

  }

}

# Column applier
apply_cols <- function(X, FUN, ...) {

  if (is.data.frame(X)) {
    lapply(X, FUN, ...)
  } else if (is.matrix(X)) {
    apply(X, 2, FUN, ...)
  } else {
    stop("Input must be a data frame or matrix")
  }

}

# Function to check whether something is an integer, with some tolerance just in case
# Can alternatively return the rounded value to make sure
integer_checker <- function(x, object_name = NULL, require_positive = TRUE, return = FALSE) {

  if (!is.null(x)) {
    if ((abs(round(x) - x)) > 1e-16 | !is.numeric(x)) {

      if (is.null(object_name)) {
        stop("Non-whole number or integer detected where one was expected. Please check inputs.")
      } else {
        stop(paste("Please specify NULL, an integer or a whole number for", object_name, collapse = " "))
      }
    }

    # Just do this separately
    if (require_positive == TRUE & x < 1) {

      if (is.null(object_name)) {
        stop("Non-positive integer detected where one was expected. Please check inputs.")
      } else {
        stop(paste("Please specify NULL, a positive integer or a whole number for", object_name, collapse = " "))
      }
    }
  }

  if (return == FALSE) {
    return(NULL)
  } else {
    return(round(x))
  }

}
