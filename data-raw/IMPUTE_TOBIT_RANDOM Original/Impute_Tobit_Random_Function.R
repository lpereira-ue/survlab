
# library(data.table)
# library(survival)
# library(truncnorm)


# Enhanced function for imputing censored values using Tobit models
impute_tobit_random <- function(
  dt,
  value_col = "value",
  cens_col = "censored",
  loq = NULL,
  dist = c(
    "gaussian",
    "lognormal",
    "weibull",
    "exponential",
    "logistic",
    "loglogistic"
  ),
  n_samples = 1
) {

  # Convert to data.table if not already
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  } else {
    dt <- copy(dt) # Work on copy to avoid modifying original
  }

  # Auto-detect LOQ if not provided (for logging purposes only - we use individual LOQs)
  if (is.null(loq)) {
    loq_levels <- dt[get(cens_col) == 0, unique(get(value_col))]
    loq <- max(loq_levels) # Report the highest LOQ for logging
    message(paste(
      "Auto-detected LOQ levels:",
      paste(sort(loq_levels), collapse = ", ")
    ))
    message(paste("Using individual LOQs for each observation"))
  }

  # Extract values and censoring indicators
  x <- dt[[value_col]]
  cens <- dt[[cens_col]]

  # Only proceed if there are censored observations (censored = 0)
  if (sum(cens == 0) == 0) {
    message("No censored observations found.")
    return(dt)
  }

  # Test different distributions and select best based on AIC
  best_model <- NULL
  best_aic <- Inf
  best_dist <- NULL

  for (d in dist) {
    tryCatch(
      {
        # Fit survival model with left censoring (censored = 0, observed = 1)
        mod <- survreg(Surv(x, cens, type = "left") ~ 1, dist = d)
        aic_val <- AIC(mod)

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

  message(paste0("Best distribution: ", best_dist, ", with AIC = ", round(best_aic, 2)))

  # Extract parameters for random sampling
  # Handle individual LOQs for each censored observation (censored = 0)
  if (best_dist == "gaussian") {
    # For Gaussian: sigma and mu
    sigma <- best_model$scale
    mu <- predict(best_model, type = "response")[1] # Use intercept for all

    # Generate random samples for EACH censored observation using its own LOQ
    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col) # The censored value IS the LOQ for that obs
        # Add small epsilon to ensure strict inequality (imputed < LOQ)
        epsilon <- individual_loq * 1e-6
        rtruncnorm(
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
    mu_log <- predict(best_model, type = "lp")[1] # linear predictor (log scale)

    # Sample from truncated lognormal for each observation
    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col) # The censored value IS the LOQ
        epsilon <- individual_loq * 1e-6
        # Sample on log scale then exponentiate, ensuring result < individual_loq
        log_samples <- rtruncnorm(
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
    lambda <- predict(best_model, type = "response")[1] # scale parameter
    k <- 1 / best_model$scale # shape parameter

    # For Weibull truncation using individual LOQs
    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col) # The censored value IS the LOQ
        epsilon <- individual_loq * 1e-6
        # Calculate CDF at individual LOQ
        p_loq <- pweibull(individual_loq - epsilon, shape = k, scale = lambda)
        # Sample uniform and transform
        u <- runif(.N, 0, p_loq)
        qweibull(u, shape = k, scale = lambda)
      }
    ]

  } else if (best_dist == "exponential") {
    # For Exponential: extract rate parameter
    lambda <- predict(best_model, type = "response")[1] # scale parameter
    rate <- 1 / lambda # rate = 1/scale for exponential

    # For exponential truncation using individual LOQs
    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col)
        epsilon <- individual_loq * 1e-6
        # Calculate CDF at individual LOQ
        p_loq <- pexp(individual_loq - epsilon, rate = rate)
        # Sample uniform and transform
        u <- runif(.N, 0, p_loq)
        qexp(u, rate = rate)
      }
    ]

  } else if (best_dist == "logistic") {
    # For Logistic: extract location and scale parameters
    location <- predict(best_model, type = "response")[1] # location parameter
    scale <- best_model$scale # scale parameter

    # For logistic truncation using individual LOQs
    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col)
        epsilon <- individual_loq * 1e-6
        # Calculate CDF at individual LOQ
        p_loq <- plogis(
          individual_loq - epsilon,
          location = location,
          scale = scale
        )
        # Sample uniform and transform
        u <- runif(.N, 0, p_loq)
        qlogis(u, location = location, scale = scale)
      }
    ]

  } else if (best_dist == "loglogistic") {
    # For Log-logistic: similar to lognormal but with logistic distribution
    sigma <- best_model$scale
    mu_log <- predict(best_model, type = "lp")[1] # linear predictor (log scale)

    # Sample from truncated log-logistic for each observation
    dt[
      get(cens_col) == 0,
      paste0(value_col, "_imputed") := {
        individual_loq <- get(value_col)
        epsilon <- individual_loq * 1e-6
        # Sample on log scale using logistic distribution, then exponentiate
        log_samples <- qlogis(
          runif(
            .N,
            0,
            plogis(
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
  setattr(dt, "best_model", best_model)
  setattr(dt, "best_distribution", best_dist)
  setattr(dt, "loq", loq)
  setattr(dt, "aic", best_aic)

  return(dt)
}

# Example usage:
# dt <- fread("synthetic_censored_data_normal.csv")
# dt_imputed <- impute_tobit_random(dt, value_col = "value", cens_col = "censored")


# Validation function to check imputation quality
validate_imputation <- function(
  dt_imputed,
  value_col = "value",
  cens_col = "censored"
) {
  message("=== Imputation Validation ===")

  # Check that each imputed value is below its corresponding LOQ
  censored_rows <- dt_imputed[get(cens_col) == 0] # censored = 0 now
  individual_loqs <- censored_rows[[value_col]]
  imputed_vals <- censored_rows[[paste0(value_col, "_imputed")]]

  violations <- sum(imputed_vals >= individual_loqs)

  if (violations == 0) {
    message("✓ All imputed values are strictly below their respective LOQs")
  } else {
    warning(paste("✗", violations, "imputed values exceed or equal their LOQ!"))

    # Show violations
    violation_idx <- which(imputed_vals >= individual_loqs)
    message("Violations:")
    for (i in violation_idx[1:min(5, length(violation_idx))]) {
      # Show max 5
      message(sprintf(
        "  Row %d: LOQ=%.6f, Imputed=%.6f (diff=%.2e)",
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
    message("✓ All imputed values are unique")
  } else {
    duplicates <- length(imputed_vals) - length(unique(imputed_vals))
    message(paste("⚠", duplicates, "duplicate imputed values found"))
  }

  # Summary statistics by LOQ level
  loq_summary <- censored_rows[,
    .(
      count = .N,
      min_imputed = min(get(paste0(value_col, "_imputed"))),
      max_imputed = max(get(paste0(value_col, "_imputed"))),
      mean_imputed = mean(get(paste0(value_col, "_imputed")))
    ),
    by = value_col
  ]

  message("\nSummary by LOQ level:")
  print(loq_summary)

  message(paste(
    "Total censored observations:",
    sum(dt_imputed[[cens_col]] == 0)
  ))
  message(paste("Best distribution:", attr(dt_imputed, "best_distribution")))
  message(paste("Model AIC:", round(attr(dt_imputed, "aic"), 2)))

  return(invisible(dt_imputed))
}
