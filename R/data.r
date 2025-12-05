#' Environmental Laboratory Nitrate Data with Non-Detects
#'
#' A synthetic dataset containing environmental nitrate measurements with non-detect
#' values, generated from a lognormal distribution. This dataset represents typical
#' water quality monitoring data from an environmental laboratory, designed for
#' demonstrating survival model-based imputation techniques.
#'
#' @format A data.table with 200 rows and 4 variables:
#' \describe{
#'   \item{parameter}{Character string indicating the chemical parameter ("Nitrate")}
#'   \item{unit}{Character string indicating the unit of measurement ("mg/l NO3")}
#'   \item{value}{Numeric values representing either detected measurements or
#'                detection limits for non-detect observations}
#'   \item{censored}{Integer indicator where 0 = non-detect (below detection limit),
#'                   1 = detected (above detection limit)}
#' }
#'
#' @details
#' This dataset simulates real-world environmental water quality data where nitrate
#' measurements below certain detection limits are reported as non-detects. The data includes:
#' \itemize{
#'   \item Single parameter (Nitrate) with consistent units (mg/l NO3)
#'   \item Multiple detection limit levels reflecting different analytical conditions
#'   \item Realistic distribution of detected vs non-detect values (83.5% detects, 16.5% non-detects)
#'   \item Detection limits ranging from 5 to 25 mg/l NO3
#'   \item Lognormal distribution typical of environmental contaminant data
#' }
#'
#' For non-detect observations (censored = 0), the 'value' column contains the
#' detection limit for that specific analysis. For detected measurements (censored = 1),
#' the 'value' column contains the actual measured nitrate concentration.
#'
#' @source Synthetic data generated for package demonstration, based on typical
#'         environmental water quality monitoring programs
#'
#' @examples
#' data(multi_censored_data)
#'
#' # Basic data exploration
#' multi_censored_data[, .(
#'   total_samples = .N,
#'   non_detects = sum(censored == 0),
#'   detects = sum(censored == 1)
#' )]
#'
#' # View parameter and unit information
#' multi_censored_data[, .(
#'   parameter = unique(parameter),
#'   unit = unique(unit)
#' )]
#'
#' # View detection limit levels
#' multi_censored_data[censored == 0, unique(value)]
#'
#' # Apply survival model imputation
#' result <- impute_nondetect(multi_censored_data,
#'                           parameter_col = "parameter",
#'                           unit_col = "unit")
#' validate_imputation(result)
"multi_censored_data"