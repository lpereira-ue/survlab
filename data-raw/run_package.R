
library(survlab)

# Load environmental data
data(multi_censored_data)

# Professional imputation with validation
result <- impute_nondetect(
  multi_censored_data,
  parameter_col = "parameter",
  unit_col = "unit"
)

cat("\n")

# Comprehensive validation
validate_imputation(result)
