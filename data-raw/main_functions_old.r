#' Impute Non-Detect Values in Laboratory Data
#'
#' This function imputes non-detect (censored) values in environmental laboratory
#' analytical data using survival models with automatic distribution selection.
#' It validates data quality requirements and fits multiple distributions to
#' select the best model based on AIC. Each imputed value is guaranteed to be
#' below its respective detection limit.
#'
#' @param dt A data.frame or data.table containing laboratory analytical data
#' @param value_col Character string specifying the column name containing values
#' @param cens_col Character string specifying the column name containing censoring indicators (0 = non-detect/censored, 1 = detected/observed)
#' @param parameter_col Character string specifying the column name containing parameter names (optional, for validation)
#' @param unit_col Character string specifying the column name containing units (optional, for validation)
#' @param dist Character vector of distributions to test. Options include: "gaussian", "lognormal", "weibull", "exponential", "logistic", "loglogistic"
#' @param min_observations Minimum number of observations required for modeling (default: 25)
#' @param max_censored_pct Maximum percentage of censored values allowed (default: 75)
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
#' }
#'
#' @details
#' The function performs several validation checks:
#' 1. Ensures sufficient sample size (>= min_observations)
#' 2. Checks that censoring percentage is reasonable (<= max_censored_pct)
#' 3. Validates that only one parameter and unit are present (if columns provided)
#' 4. Tests multiple distributions and selects the best based on AIC
#' 5. Generates random imputed values below each observation's detection limit
#'
#' For non-detect observations (censored = 0), the value in value_col is treated
#' as the detection limit for that specific analysis, allowing for different
#' detection limits across samples or analytical methods.
#'
#' @examples
#' \dontrun{
#' # Load example environmental data
#' data(multi_censored_data)
#'
#' # Basic imputation for laboratory data
#' result <- impute_nondetect(multi_censored_data,
#'                           value_col = "value",
#'                           cens_col = "censored",
#'                           parameter_col = "parameter",
#'                           unit_col = "unit")
#'
#' # View imputed non-detect values
#' result[censored == 0, .(value, value_imputed, value_final)]
#'
#' # Check model information
#' attr(result, "best_distribution")
#' attr(result, "parameter")
#' attr(result, "unit")
#' }
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
  max_censored_pct = 75
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
    message(
      "No non-detect observations found. Returning original data unchanged."
    )
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
    message(paste("Parameter:", parameter_name))
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
    message(paste("Unit:", unit_name))
  }

  # Auto-detect detection limits (only runs if there are non-detects)
  detection_limits <- dt[get(cens_col) == 0, unique(get(value_col))]
  max_detection_limit <- max(detection_limits)

  message(paste("Dataset summary:"))
  message(paste("- Total observations:", total_obs))
  message(paste(
    "- Non-detects:",
    non_detects,
    paste0("(", round(censored_pct, 1), "%)")
  ))
  message(paste(
    "- Detects:",
    detects,
    paste0("(", round(100 - censored_pct, 1), "%)")
  ))
  message(paste("- Detection limit levels:", length(detection_limits)))
  message(paste(
    "- Detection limits:",
    paste(sort(detection_limits), collapse = ", ")
  ))
  message(paste("- Maximum detection limit:", max_detection_limit))

  # Test different distributions and select best based on AIC
  best_model <- NULL
  best_aic <- Inf
  best_dist <- NULL

  for (d in dist) {
    tryCatch(
      {
        # Fit survival model with left censoring (censored = 0, observed = 1)
        mod <- survival::survreg(
          survival::Surv(x, cens, type = "left") ~ 1,
          dist = d
        )
        aic_val <- stats::AIC(mod)

        if (aic_val < best_aic) {
          best_aic <- aic_val
          best_model <- mod
          best_dist <- d
        }
      },
      error = function(e) {
        message(paste("Failed to fit", d, "distribution:", e$message))
      }
    )
  }

  if (is.null(best_model)) {
    stop("No distribution could be fitted to the data")
  }

  message(paste0(
    "Best distribution: ",
    best_dist,
    ", with AIC = ",
    round(best_aic, 2)
  ))

  # Extract parameters for random sampling
  # Handle individual LOQs for each censored observation (censored = 0)
  if (best_dist == "gaussian") {
    # For Gaussian: sigma and mu
    sigma <- best_model$scale
    mu <- stats::predict(best_model, type = "response")[1] # Use intercept for all

    # Generate random samples for EACH censored observation using its own LOQ
    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col) # The censored value IS the LOQ for that obs
        # Add small epsilon to ensure strict inequality (imputed < LOQ)
        epsilon <- individual_loq * 1e-6
        truncnorm::rtruncnorm(
          .N,
          a = -Inf,
          b = individual_loq - epsilon,
          mean = mu,
          sd = sigma
        )
      }
    ]
  } else if (best_dist == "lognormal") {
    # For lognormal: extract log-scale parameters
    sigma <- best_model$scale
    mu_log <- stats::predict(best_model, type = "lp")[1] # linear predictor (log scale)

    # Sample from truncated lognormal for each observation
    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col) # The censored value IS the LOQ
        epsilon <- individual_loq * 1e-6
        # Sample on log scale then exponentiate, ensuring result < individual_loq
        log_samples <- truncnorm::rtruncnorm(
          .N,
          a = -Inf,
          b = log(individual_loq - epsilon),
          mean = mu_log,
          sd = sigma
        )
        exp(log_samples)
      }
    ]
  } else if (best_dist == "weibull") {
    # For Weibull: extract scale and shape parameters
    lambda <- stats::predict(best_model, type = "response")[1] # scale parameter
    k <- 1 / best_model$scale # shape parameter

    # For Weibull truncation using individual LOQs
    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col) # The censored value IS the LOQ
        epsilon <- individual_loq * 1e-6
        # Calculate CDF at individual LOQ
        p_loq <- stats::pweibull(
          individual_loq - epsilon,
          shape = k,
          scale = lambda
        )
        # Sample uniform and transform
        u <- stats::runif(.N, 0, p_loq)
        stats::qweibull(u, shape = k, scale = lambda)
      }
    ]
  } else if (best_dist == "exponential") {
    # For Exponential: extract rate parameter
    lambda <- stats::predict(best_model, type = "response")[1] # scale parameter
    rate <- 1 / lambda # rate = 1/scale for exponential

    # For exponential truncation using individual LOQs
    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col)
        epsilon <- individual_loq * 1e-6
        # Calculate CDF at individual LOQ
        p_loq <- stats::pexp(individual_loq - epsilon, rate = rate)
        # Sample uniform and transform
        u <- stats::runif(.N, 0, p_loq)
        stats::qexp(u, rate = rate)
      }
    ]
  } else if (best_dist == "logistic") {
    # For Logistic: extract location and scale parameters
    location <- stats::predict(best_model, type = "response")[1] # location parameter
    scale <- best_model$scale # scale parameter

    # For logistic truncation using individual LOQs
    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col)
        epsilon <- individual_loq * 1e-6
        # Calculate CDF at individual LOQ
        p_loq <- stats::plogis(
          individual_loq - epsilon,
          location = location,
          scale = scale
        )
        # Sample uniform and transform
        u <- stats::runif(.N, 0, p_loq)
        stats::qlogis(u, location = location, scale = scale)
      }
    ]
  } else if (best_dist == "loglogistic") {
    # For Log-logistic: similar to lognormal but with logistic distribution
    sigma <- best_model$scale
    mu_log <- stats::predict(best_model, type = "lp")[1] # linear predictor (log scale)

    # Sample from truncated log-logistic for each observation
    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col)
        epsilon <- individual_loq * 1e-6
        # Sample on log scale using logistic distribution, then exponentiate
        log_samples <- stats::qlogis(
          stats::runif(
            .N,
            0,
            stats::plogis(
              log(individual_loq - epsilon),
              location = mu_log,
              scale = sigma
            )
          ),
          location = mu_log,
          scale = sigma
        )
        exp(log_samples)
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

  return(dt)
}


#' Validate Laboratory Non-Detect Imputation Results
#'
#' This function validates the quality of non-detect value imputation by checking that
#' imputed values are below their respective limits of quantification and providing
#' comprehensive summary statistics and model diagnostics.
#'
#' @param dt_imputed A data.table returned from \code{\link{impute_nondetect}}
#' @param value_col Character string specifying the column name containing original values
#' @param cens_col Character string specifying the column name containing censoring indicators
#'
#' @return Invisibly returns the input data.table. Prints validation results to console.
#'
#' @details
#' The function checks:
#' \itemize{
#'   \item All imputed values are strictly below their respective limits of quantification
#'   \item Uniqueness of imputed values
#'   \item Summary statistics by limits of quantification level
#'   \item Model fit information including parameter and unit details
#'   \item Dataset characteristics (sample size, censoring percentage)
#' }
#'
#' @examples
#' \dontrun{
#' data(multi_censored_data)
#' result <- impute_nondetect(multi_censored_data)
#' validate_imputation(result)
#' }
#'
#' @export
validate_imputation <- function(
  dt_imputed,
  value_col = "value",
  cens_col = "censored"
) {
  message("=== Laboratory Non-Detect Imputation Validation ===")

  # Display dataset information first
  if (!is.null(attr(dt_imputed, "parameter"))) {
    message(paste("Parameter:", attr(dt_imputed, "parameter")))
  }
  if (!is.null(attr(dt_imputed, "unit"))) {
    message(paste("Unit:", attr(dt_imputed, "unit")))
  }

  message(paste("Sample size:", attr(dt_imputed, "sample_size")))
  message(paste("Censoring percentage:", attr(dt_imputed, "censored_pct"), "%"))

  # Check that each imputed value is below its corresponding detection limit
  censored_rows <- dt_imputed[get(cens_col) == 0] # non-detects = 0
  individual_loqs <- censored_rows[[value_col]]
  imputed_vals <- censored_rows[[paste0(value_col, "_imputed")]]

  violations <- sum(imputed_vals >= individual_loqs)

  if (violations == 0) {
    message(
      "\u2713 All imputed values are strictly below their respective detection limits"
    )
  } else {
    warning(paste(
      "\u2717",
      violations,
      "imputed values exceed or equal their detection limit!"
    ))

    # Show violations
    violation_idx <- which(imputed_vals >= individual_loqs)
    message("Violations:")
    for (i in violation_idx[1:min(5, length(violation_idx))]) {
      # Show max 5
      message(sprintf(
        "  Row %d: Detection Limit=%.6f, Imputed=%.6f (diff=%.2e)",
        i,
        individual_loqs[i],
        imputed_vals[i],
        imputed_vals[i] - individual_loqs[i]
      ))
    }
    if (length(violation_idx) > 5) {
      message(sprintf(
        "  ... and %d more violations",
        length(violation_idx) - 5
      ))
    }
  }

  # Check uniqueness of imputed values
  if (length(unique(imputed_vals)) == length(imputed_vals)) {
    message("\u2713 All imputed values are unique")
  } else {
    duplicates <- length(imputed_vals) - length(unique(imputed_vals))
    message(paste("\u26A0", duplicates, "duplicate imputed values found"))
  }

  # Summary statistics by detection limit level
  loq_summary <- censored_rows[,
    .(
      count = .N,
      min_imputed = min(get(paste0(value_col, "_imputed"))),
      max_imputed = max(get(paste0(value_col, "_imputed"))),
      mean_imputed = mean(get(paste0(value_col, "_imputed")))
    ),
    by = value_col
  ][
    order(get(value_col))
  ]

  message("\nSummary by detection limit level:")
  print(loq_summary)

  message(paste(
    "Total non-detect observations:",
    sum(dt_imputed[[cens_col]] == 0)
  ))
  message(paste("Best distribution:", attr(dt_imputed, "best_distribution")))
  message(paste("Model AIC:", round(attr(dt_imputed, "aic"), 2)))
  message(paste(
    "Detection limits found:",
    paste(attr(dt_imputed, "detection_limits"), collapse = ", ")
  ))

  return(invisible(dt_imputed))
}
