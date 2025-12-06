\# survlab [![survlab
website](reference/figures/logo.png)](https://lpereira-ue.github.io/survlab/)

## Overview

`survlab` provides functions for imputing non-detect values in
environmental laboratory data using survival models (including Tobit
models) with automatic distribution selection. Is designed specifically
for working with analytical data where measurements fall below detection
limits or limits of quantification (LOQ).

## Key Features

- **Automatic distribution selection**: Tests multiple distributions
  (gaussian, lognormal, weibull, exponential, logistic, log-logistic)
  and selects the best fit based on AIC
- **Data quality validation**: Ensures sufficient sample size (≥25) and
  reasonable censoring percentage (≤75%)
- **Parameter/unit validation**: Validates data consistency for single
  parameter datasets
- **Individual detection limit handling**: Each non-detect observation
  can have its own detection limit
- **Realistic imputation**: Generates unique random values below their
  respective detection limits
- **Built-in validation**: Comprehensive validation and diagnostic tools
  designed for laboratory data
- **Efficient implementation**: Uses `data.table` for fast and
  memory-efficient operations
- **Environmental focus**: Designed specifically for environmental
  monitoring and contamination assessment data

## Installation

``` r
# Load the package
library(survlab)
```

## Quick Start

``` r
library(survlab)
library(data.table)

# Load example environmental data
data(multi_censored_data)

# Explore the data
multi_censored_data[, .(
  total_samples = .N,
  non_detects = sum(censored == 0),  # 0 = non-detect (below detection limit)
  detects = sum(censored == 1)       # 1 = detected (above detection limit)
)]

# Perform imputation with parameter validation
set.seed(123)
result <- impute_nondetect(
  dt = multi_censored_data,
  value_col = "value", 
  cens_col = "censored",
  parameter_col = "parameter",
  unit_col = "unit"
)

# Validate results
validate_imputation(result)

# View imputed values
result[censored == 0, .(
  original_detection_limit = value,
  imputed_value = round(value_imputed, 4),
  final_value = round(value_final, 4)
)][1:10]
```

## Data Structure

The package expects laboratory data with the following structure:

- **value_col**: Contains either detected values (for samples above
  detection limit) or detection limit values (for non-detect samples)
- **cens_col**: Binary indicator where `0 = non-detect` (below detection
  limit), `1 = detected` (above detection limit)
- **parameter_col** (optional): Parameter name (e.g., “Nitrate”, “Lead”,
  “pH”)
- **unit_col** (optional): Unit of measurement (e.g., “mg/L”, “ug/L”,
  “pH units”)

For non-detect observations, the value in `value_col` is treated as the
detection limit for that specific analysis. The package validates that
only one parameter and one unit are present when these columns are
provided.

## Main Functions

### `impute_nondetect()`

The main imputation function that: - Validates data quality (sample size
≥25, censoring ≤75%) - Ensures parameter and unit consistency - Fits
multiple survival models with different distributions - Selects the best
model based on AIC - Generates random imputed values below each
observation’s detection limit - Returns original data with additional
imputed value columns

### `validate_imputation()`

Validation function that checks: - All imputed values are below their
respective detection limits - Uniqueness of imputed values - Summary
statistics by detection limit level - Model fit information

## Example Output

``` r
# After imputation, the data includes new columns:
# - value_imputed: Imputed values for non-detect observations
# - value_final: Combined detected and imputed values

# Model information is stored as attributes:
attr(result, "best_distribution")    # Best-fitting distribution
attr(result, "aic")                 # Model AIC
attr(result, "parameter")           # Parameter name
attr(result, "unit")                # Unit of measurement
attr(result, "sample_size")         # Total observations
attr(result, "censored_pct")        # Percentage censored
attr(result, "detection_limits")    # All detection limits found
attr(result, "max_detection_limit") # Highest detection limit
attr(result, "best_model")          # Fitted survival model
```

## Advanced Usage

### Custom Validation Thresholds

``` r
# Adjust validation criteria
result <- impute_nondetect(
  dt = data,
  min_observations = 50,     # Require at least 50 observations
  max_censored_pct = 60      # Allow up to 60% censoring
)
```

### Custom Distribution Selection

``` r
# Test only specific distributions
result <- impute_nondetect(
  dt = data,
  dist = c("gaussian", "lognormal")
)
```

### Working with Different Column Names

``` r
# Specify custom column names
result <- impute_tobit_random(
  dt = data,
  value_col = "measurement",
  cens_col = "is_below_loq"
)
```

## Citation

If you use this package in your research, please cite:

    Pereira L, Infante P, Ferreira T, Quaresma P (2025). survlab: Survival Model-Based Imputation for Laboratory Non-Detect Data. R package version 0.1.0.
