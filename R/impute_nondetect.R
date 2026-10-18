#' Impute Non-Detect Values in Laboratory Data
#'
#' This function imputes non-detect (censored) values in environmental laboratory
#' analytical data using survival models with automatic distribution selection.
#' It validates data quality requirements and fits multiple distributions to
#' select the best model based on AIC. Each imputed value is guaranteed to be
#' below its respective detection limit and above the specified minimum value.
#'
#' @param dt A data.frame or data.table containing laboratory analytical data
#' @param value_col Character string specifying the column name containing values
#' @param cens_col Character string specifying the column name containing censoring indicators (0 = non-detect/censored, 1 = detected/observed)
#' @param parameter_col Character string specifying the column name containing parameter names (optional, for validation)
#' @param unit_col Character string specifying the column name containing units (optional, for validation)
#' @param dist Character vector of distributions to test. Options include: "gaussian", "lognormal", "weibull", "exponential", "logistic", "loglogistic"
#' @param min_observations Minimum number of observations required for modeling (default: 25)
#' @param max_censored_pct Maximum percentage of censored values allowed (default: 75)
#' @param min_value Minimum allowable value for imputed concentrations (default: 0, use 1e-10 for strictly positive)
#' @param zero_value Value that replaces exact zeros when fitting the
#'   distributions defined only for positive values ("lognormal", "weibull",
#'   "exponential", "loglogistic"), which otherwise cannot be fitted (default:
#'   1e-5). The replacement is used for model fitting only; the zeros are kept
#'   in \code{[value_col]_final}. Use \code{NULL} to disable, in which case
#'   those distributions are skipped whenever the data contain a zero.
#' @param seed Optional integer passed to \code{\link[base]{set.seed}} right
#'   before the imputed values are drawn, making the imputation reproducible
#'   (default: \code{NULL}, no seed is set).
#' @param control A \code{\link[survival]{survreg.control}} object used to control
#'   the fitting algorithm, e.g. maximum number of iterations and convergence
#'   tolerance. Defaults to \code{survival::survreg.control()}. Increase
#'   \code{maxiter} (e.g. \code{survreg.control(maxiter = 200)}) when
#'   convergence warnings are raised for complex datasets.
#' @param verbose Logical indicating whether to display progress messages and distribution fitting information (default: FALSE)
#'
#' @return A data.table with additional columns:
#' \describe{
#'   \item{[value_col]_imputed}{Imputed values for non-detect observations}
#'   \item{[value_col]_final}{Final values combining original detected and imputed non-detect values}
#' }
#' The returned object also has attributes containing model information:
#' \describe{
#'   \item{best_model}{The fitted survival model object}
#'   \item{best_distribution}{Name of the best-fitting distribution}
#'   \item{detection_limits}{Vector of all detection limits found in the data}
#'   \item{max_detection_limit}{The highest detection limit (for reference)}
#'   \item{parameter}{Parameter name (if parameter_col provided)}
#'   \item{unit}{Unit of measurement (if unit_col provided)}
#'   \item{aic}{AIC value of the best model}
#'   \item{sample_size}{Total number of observations}
#'   \item{censored_pct}{Percentage of censored observations}
#'   \item{best_converged}{\code{FALSE} if no distribution converged and the
#'     selected model is the best non-converged fit}
#'   \item{fits}{A data.table with one row per distribution tried: AIC,
#'     convergence, and any error or warning message}
#'   \item{n_zero_replaced}{Number of exact zeros replaced by \code{zero_value}
#'     when fitting the positive distributions}
#'   \item{seed}{The seed used, or \code{NULL}}
#' }
#'
#' @details
#' The function performs several validation checks:
#' 1. Ensures sufficient sample size (>= min_observations)
#' 2. Checks that censoring percentage is reasonable (<= max_censored_pct)
#' 3. Validates that only one parameter and unit are present (if columns provided)
#' 4. Tests multiple distributions and selects the best based on AIC among
#'    those that converged (if none converged, the best non-converged fit is
#'    used and a warning is raised)
#' 5. Generates random imputed values below each observation's detection limit and above min_value
#'
#' For non-detect observations (censored = 0), the value in value_col is treated
#' as the detection limit for that specific analysis, allowing for different
#' detection limits across samples or analytical methods.
#'
#' IMPORTANT: This function should be applied to data containing only ONE parameter
#' at a time. Different environmental parameters have different distributions and
#' should not be modeled together.
#'
#' When verbose = FALSE, the function operates silently except for critical errors,
#' making it suitable for batch processing of multiple parameters.
#'
#' @examples
#' # Load example data
#' data(multi_censored_data)
#'
#' # Basic imputation with default settings
#' set.seed(123)
#' result <- impute_nondetect(
#'   dt = multi_censored_data,
#'   value_col = "value",
#'   cens_col = "censored",
#'   verbose = FALSE
#' )
#'
#' # View imputed values for non-detects
#' head(result[censored == 0, .(value, value_imputed, value_final)])
#'
#' # Check best distribution selected
#' attr(result, "best_distribution")
#'
#' # Increase max iterations for difficult datasets
#' result <- impute_nondetect(
#'   dt        = multi_censored_data,
#'   value_col = "value",
#'   cens_col  = "censored",
#'   control   = survival::survreg.control(maxiter = 200)
#' )
#'
#' # With parameter and unit validation
#' result <- impute_nondetect(
#'   dt = multi_censored_data,
#'   value_col = "value",
#'   cens_col = "censored",
#'   parameter_col = "parameter",
#'   unit_col = "unit"
#' )
#'
#' # For strictly positive values (avoiding exactly zero)
#' result <- impute_nondetect(
#'   dt = multi_censored_data,
#'   value_col = "value",
#'   cens_col = "censored",
#'   min_value = 1e-10,
#'   verbose = FALSE
#' )
#'
#' @import data.table
#' @import survival
#' @import truncnorm
#' @importFrom stats AIC predict runif pweibull qweibull pexp qexp plogis qlogis
#' @export
impute_nondetect <- function(
  dt,
  value_col = "value",
  cens_col = "censored",
  parameter_col = NULL,
  unit_col = NULL,
  dist = c(
    "gaussian",
    "lognormal",
    "weibull",
    "exponential",
    "logistic",
    "loglogistic"
  ),
  min_observations = 25,
  max_censored_pct = 75,
  min_value = 0,
  zero_value = 1e-5,
  seed = NULL,
  control = survival::survreg.control(),
  verbose = FALSE
) {
  # Input validation
  if (!value_col %in% names(dt)) {
    stop(paste("Column", value_col, "not found in data"))
  }
  if (!cens_col %in% names(dt)) {
    stop(paste("Column", cens_col, "not found in data"))
  }
  if (!is.null(parameter_col) && !parameter_col %in% names(dt)) {
    stop(paste("Column", parameter_col, "not found in data"))
  }
  if (!is.null(unit_col) && !unit_col %in% names(dt)) {
    stop(paste("Column", unit_col, "not found in data"))
  }

  # Convert to data.table if not already
  if (!data.table::is.data.table(dt)) {
    dt <- data.table::as.data.table(dt)
  } else {
    dt <- data.table::copy(dt) # Work on copy to avoid modifying original
  }

  # Validate sample size
  total_obs <- nrow(dt)
  if (total_obs < min_observations) {
    stop(paste(
      "Insufficient data: only",
      total_obs,
      "observations. Minimum required:",
      min_observations
    ))
  }

  # Extract values and censoring indicators
  x <- dt[[value_col]]
  cens <- dt[[cens_col]]

  # Calculate censoring statistics
  non_detects <- sum(cens == 0)
  detects <- sum(cens == 1)
  censored_pct <- (non_detects / total_obs) * 100

  # Check if there are any non-detect observations
  if (non_detects == 0) {
    if (verbose) {
      message("No non-detect observations found. Returning original data unchanged.")
    }
    # Create the two columns so downstream code never fails:
    dt[, paste0(value_col, "_imputed") := as.numeric(NA)]
    dt[, paste0(value_col, "_final") := get(value_col)]
    return(dt)
  }

  # Validate censoring percentage
  if (censored_pct > max_censored_pct) {
    stop(paste(
      "Too many censored values:",
      round(censored_pct, 1),
      "%. Maximum allowed:",
      max_censored_pct,
      "%"
    ))
  }

  # Validate parameter and unit consistency (if columns provided)
  parameter_name <- NULL
  unit_name <- NULL

  if (!is.null(parameter_col)) {
    unique_parameters <- dt[, unique(get(parameter_col))]
    if (length(unique_parameters) > 1) {
      stop(paste(
        "Multiple parameters found in dataset:",
        paste(unique_parameters, collapse = ", "),
        "- Please filter to only one parameter before imputation"
      ))
    }
    parameter_name <- unique_parameters[1]
  }

  if (!is.null(unit_col)) {
    unique_units <- dt[, unique(get(unit_col))]
    if (length(unique_units) > 1) {
      stop(paste(
        "Multiple units found in dataset:",
        paste(unique_units, collapse = ", "),
        "- Please ensure consistent units before imputation"
      ))
    }
    unit_name <- unique_units[1]
  }

  # Auto-detect detection limits
  detection_limits <- dt[get(cens_col) == 0, unique(get(value_col))]
  max_detection_limit <- max(detection_limits)

  # Distributions defined only for positive values cannot be fitted to exact
  # zeros, so the zeros are replaced by zero_value for those fits only.
  positive_dists <- c("lognormal", "weibull", "exponential", "loglogistic")
  is_zero <- x == 0
  n_zero_replaced <- if (is.null(zero_value)) 0L else sum(is_zero)
  x_pos <- x
  if (n_zero_replaced > 0L) {
    x_pos[is_zero] <- zero_value
  }

  # Fit every distribution, recording AIC and convergence for each
  models <- list()
  fit_rows <- vector("list", length(dist))

  for (i in seq_along(dist)) {
    d <- dist[i]
    x_fit <- if (d %in% positive_dists) x_pos else x
    fit_warnings <- character()
    fit_error <- NA_character_

    # Fit survival model with left censoring (censored = 0, observed = 1)
    mod <- tryCatch(
      withCallingHandlers(
        survival::survreg(
          survival::Surv(x_fit, cens, type = "left") ~ 1,
          dist = d,
          control = control
        ),
        warning = function(w) {
          fit_warnings <<- c(fit_warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        fit_error <<- conditionMessage(e)
        NULL
      }
    )

    aic_val <- if (is.null(mod)) NA_real_ else stats::AIC(mod)
    converged <- !is.null(mod) && is.finite(aic_val) &&
      !any(grepl("converge|iteration", fit_warnings, ignore.case = TRUE))

    if (!is.null(mod)) {
      models[[d]] <- mod
    }

    fit_rows[[i]] <- data.table::data.table(
      distribution = d,
      aic = aic_val,
      converged = converged,
      message = if (!is.na(fit_error)) {
        fit_error
      } else if (length(fit_warnings) > 0L) {
        paste(unique(fit_warnings), collapse = " | ")
      } else {
        NA_character_
      }
    )

    if (verbose && !converged) {
      message(paste0(
        "Distribution ", d, " ",
        if (is.null(mod)) "failed to fit: " else "did not converge: ",
        fit_rows[[i]]$message
      ))
    }
  }

  fits <- data.table::rbindlist(fit_rows)

  # Select the lowest AIC among the converged fits; fall back to the best
  # non-converged fit only when none converged.
  candidates <- fits[fits$converged]
  best_converged <- nrow(candidates) > 0L
  if (!best_converged) {
    candidates <- fits[is.finite(fits$aic)]
  }
  if (nrow(candidates) == 0L) {
    stop("No distribution could be fitted to the data")
  }

  best_dist <- candidates$distribution[which.min(candidates$aic)]
  best_aic <- min(candidates$aic)
  best_model <- models[[best_dist]]

  if (!best_converged) {
    warning(paste0(
      "No distribution converged; using the best non-converged fit (",
      best_dist, "): ", fits[fits$distribution == best_dist]$message
    ))
  }

  # Only show distribution message if verbose
  if (verbose) {
    message(paste0(
      "Best distribution: ",
      best_dist,
      ", with AIC = ",
      round(best_aic, 2)
    ))
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Extract parameters for random sampling with non-negative constraint
  if (best_dist == "gaussian") {
    sigma <- best_model$scale
    mu <- stats::predict(best_model, type = "response")[1]

    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col)
        epsilon <- individual_loq * 1e-6
        # Ensure lower bound is at least min_value
        lower_bound <- pmax(min_value, mu - 5 * sigma) # Vectorized max
        upper_bound <- individual_loq - epsilon

        # Use ifelse for vectorized conditional logic
        ifelse(
          lower_bound >= upper_bound,
          # If range is invalid, use uniform distribution as fallback
          stats::runif(
            .N,
            min_value,
            pmax(individual_loq - epsilon, min_value + 1e-10)
          ),
          # Otherwise use truncated normal
          truncnorm::rtruncnorm(
            .N,
            a = lower_bound,
            b = upper_bound,
            mean = mu,
            sd = sigma
          )
        )
      }
    ]
  } else if (best_dist == "lognormal") {
    sigma <- best_model$scale
    mu_log <- stats::predict(best_model, type = "lp")[1]

    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col)
        epsilon <- individual_loq * 1e-6
        upper_bound <- log(pmax(individual_loq - epsilon, 1e-10))
        lower_bound <- ifelse(min_value > 0, log(min_value), log(1e-10))

        # Vectorized conditional sampling
        pmax(
          ifelse(
            lower_bound >= upper_bound,
            # Fallback to uniform
            stats::runif(.N, pmax(min_value, 1e-10), individual_loq - epsilon),
            # Normal case: truncated lognormal
            exp(truncnorm::rtruncnorm(
              .N,
              a = lower_bound,
              b = upper_bound,
              mean = mu_log,
              sd = sigma
            ))
          ),
          min_value # Ensure >= min_value
        )
      }
    ]
  } else if (best_dist == "weibull") {
    lambda <- stats::predict(best_model, type = "response")[1]
    k <- 1 / best_model$scale

    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col)
        epsilon <- individual_loq * 1e-6

        # Calculate CDF values for truncation
        p_min <- ifelse(
          min_value > 0,
          stats::pweibull(min_value, shape = k, scale = lambda),
          0
        )
        p_loq <- stats::pweibull(
          individual_loq - epsilon,
          shape = k,
          scale = lambda
        )

        # Vectorized conditional sampling
        pmax(
          ifelse(
            p_min >= p_loq,
            # Fallback to uniform
            stats::runif(.N, min_value, individual_loq - epsilon),
            # Normal case: truncated weibull
            {
              u <- stats::runif(.N, p_min, p_loq)
              stats::qweibull(u, shape = k, scale = lambda)
            }
          ),
          min_value
        )
      }
    ]
  } else if (best_dist == "exponential") {
    lambda <- stats::predict(best_model, type = "response")[1]
    rate <- 1 / lambda

    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col)
        epsilon <- individual_loq * 1e-6

        p_min <- ifelse(min_value > 0, stats::pexp(min_value, rate = rate), 0)
        p_loq <- stats::pexp(individual_loq - epsilon, rate = rate)

        # Vectorized conditional sampling
        pmax(
          ifelse(
            p_min >= p_loq,
            stats::runif(.N, min_value, individual_loq - epsilon),
            {
              u <- stats::runif(.N, p_min, p_loq)
              stats::qexp(u, rate = rate)
            }
          ),
          min_value
        )
      }
    ]
  } else if (best_dist == "logistic") {
    location <- stats::predict(best_model, type = "response")[1]
    scale <- best_model$scale

    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col)
        epsilon <- individual_loq * 1e-6

        # The logistic has support on the whole real line, so it must be
        # truncated at min_value even when min_value is 0; otherwise draws
        # below min_value are clamped to it by pmax() below.
        p_min <- stats::plogis(min_value, location = location, scale = scale)
        p_loq <- stats::plogis(
          individual_loq - epsilon,
          location = location,
          scale = scale
        )

        # Vectorized conditional sampling
        pmax(
          ifelse(
            p_min >= p_loq,
            stats::runif(.N, min_value, individual_loq - epsilon),
            {
              u <- stats::runif(.N, p_min, p_loq)
              stats::qlogis(u, location = location, scale = scale)
            }
          ),
          min_value
        )
      }
    ]
  } else if (best_dist == "loglogistic") {
    sigma <- best_model$scale
    mu_log <- stats::predict(best_model, type = "lp")[1]

    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col)
        epsilon <- individual_loq * 1e-6

        log_min <- ifelse(min_value > 0, log(min_value), log(1e-10))
        log_upper <- log(pmax(individual_loq - epsilon, 1e-10))

        p_min <- stats::plogis(log_min, location = mu_log, scale = sigma)
        p_loq <- stats::plogis(log_upper, location = mu_log, scale = sigma)

        # Vectorized conditional sampling
        pmax(
          ifelse(
            p_min >= p_loq,
            stats::runif(.N, pmax(min_value, 1e-10), individual_loq - epsilon),
            {
              u <- stats::runif(.N, p_min, p_loq)
              log_samples <- stats::qlogis(u, location = mu_log, scale = sigma)
              exp(log_samples)
            }
          ),
          min_value
        )
      }
    ]
  }

  # Create final imputed column combining original and imputed values
  dt[,
    paste0(value_col, "_final") := ifelse(
      get(cens_col) == 0,
      get(paste0(value_col, "_imputed")),
      get(value_col)
    )
  ]

  # Validate that all imputed values are within bounds
  imputed_values <- dt[get(cens_col) == 0, get(paste0(value_col, "_imputed"))]
  if (any(imputed_values < min_value, na.rm = TRUE)) {
    warning(
      "Some imputed values are below min_value. Check distribution fitting."
    )
  }

  # Add model information as attributes
  data.table::setattr(dt, "best_model", best_model)
  data.table::setattr(dt, "best_distribution", best_dist)
  data.table::setattr(dt, "detection_limits", detection_limits)
  data.table::setattr(dt, "max_detection_limit", max_detection_limit)
  data.table::setattr(dt, "parameter", parameter_name)
  data.table::setattr(dt, "unit", unit_name)
  data.table::setattr(dt, "aic", best_aic)
  data.table::setattr(dt, "sample_size", total_obs)
  data.table::setattr(dt, "censored_pct", round(censored_pct, 1))
  data.table::setattr(dt, "min_value", min_value)
  data.table::setattr(dt, "best_converged", best_converged)
  data.table::setattr(dt, "fits", fits)
  data.table::setattr(dt, "n_zero_replaced", n_zero_replaced)
  data.table::setattr(dt, "seed", seed)

  return(dt)
}