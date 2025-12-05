# Data preparation script for survlab package
# This script prepares the example dataset for inclusion in the package

library(data.table)

# Read the CSV file (assuming it's in the data-raw directory)
multi_censored_data <- fread("data-raw/multi_censored_lognormal_data.csv")

# Add documentation attributes
attr(
  multi_censored_data,
  "description"
) <- "Example environmental laboratory nitrate dataset with non-detect values for survival model imputation"

# Ensure proper data types
multi_censored_data[, parameter := as.character(parameter)]
multi_censored_data[, unit := as.character(unit)]
multi_censored_data[, value := as.numeric(value)]
multi_censored_data[, censored := as.integer(censored)]

# Validate data structure
cat("Validating data structure...\n")
if (length(unique(multi_censored_data$parameter)) > 1) {
  stop("Multiple parameters found - package expects single parameter datasets")
}
if (length(unique(multi_censored_data$unit)) > 1) {
  stop("Multiple units found - package expects consistent units")
}

# Save as .rda file for package inclusion
usethis::use_data(multi_censored_data, overwrite = TRUE)

# Print summary for verification
cat("Dataset prepared successfully!\n")
cat("Dimensions:", dim(multi_censored_data), "\n")
cat("Structure:\n")
str(multi_censored_data)
cat("\nSummary:\n")
print(multi_censored_data[, .(
  parameter = unique(parameter),
  unit = unique(unit),
  total_samples = .N,
  non_detects = sum(censored == 0),
  detects = sum(censored == 1),
  min_value = min(value),
  max_value = max(value),
  detection_limits = length(unique(value[censored == 0]))
)])
