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

  setnames(loq_summary, value_col, "loq")

  message("\nSummary by limit of quantification level:")
  print(loq_summary)

  message(paste(
    "Total non-detect observations:",
    sum(dt_imputed[[cens_col]] == 0)
  ))
  message(paste("Best distribution:", attr(dt_imputed, "best_distribution")))
  message(paste("Model AIC:", round(attr(dt_imputed, "aic"), 2)))
  message(paste(
    "Detection limits found:",
    paste(sort(attr(dt_imputed, "detection_limits")), collapse = ", ")
  ))

  return(invisible(dt_imputed))
}
