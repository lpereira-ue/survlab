# Impute Non-Detect Values in Laboratory Data

This function imputes non-detect (censored) values in environmental
laboratory analytical data using survival models with automatic
distribution selection. It validates data quality requirements and fits
multiple distributions to select the best model based on AIC. Each
imputed value is guaranteed to be below its respective detection limit
and above the specified minimum value.

## Usage

``` r
impute_nondetect(
  dt,
  value_col = "value",
  cens_col = "censored",
  parameter_col = NULL,
  unit_col = NULL,
  dist = c("gaussian", "lognormal", "weibull", "exponential", "logistic", "loglogistic"),
  min_observations = 25,
  max_censored_pct = 75,
  min_value = 0,
  verbose = FALSE
)
```

## Arguments

- dt:

  A data.frame or data.table containing laboratory analytical data

- value_col:

  Character string specifying the column name containing values

- cens_col:

  Character string specifying the column name containing censoring
  indicators (0 = non-detect/censored, 1 = detected/observed)

- parameter_col:

  Character string specifying the column name containing parameter names
  (optional, for validation)

- unit_col:

  Character string specifying the column name containing units
  (optional, for validation)

- dist:

  Character vector of distributions to test. Options include:
  "gaussian", "lognormal", "weibull", "exponential", "logistic",
  "loglogistic"

- min_observations:

  Minimum number of observations required for modeling (default: 25)

- max_censored_pct:

  Maximum percentage of censored values allowed (default: 75)

- min_value:

  Minimum allowable value for imputed concentrations (default: 0, use
  1e-10 for strictly positive)

- verbose:

  Logical indicating whether to display progress messages and
  distribution fitting information (default: FALSE)

## Value

A data.table with additional columns:

- \[value_col\]\_imputed:

  Imputed values for non-detect observations

- \[value_col\]\_final:

  Final values combining original detected and imputed non-detect values

The returned object also has attributes containing model information:

- best_model:

  The fitted survival model object

- best_distribution:

  Name of the best-fitting distribution

- detection_limits:

  Vector of all detection limits found in the data

- max_detection_limit:

  The highest detection limit (for reference)

- parameter:

  Parameter name (if parameter_col provided)

- unit:

  Unit of measurement (if unit_col provided)

- aic:

  AIC value of the best model

- sample_size:

  Total number of observations

- censored_pct:

  Percentage of censored observations

## Details

The function performs several validation checks: 1. Ensures sufficient
sample size (\>= min_observations) 2. Checks that censoring percentage
is reasonable (\<= max_censored_pct) 3. Validates that only one
parameter and unit are present (if columns provided) 4. Tests multiple
distributions and selects the best based on AIC 5. Generates random
imputed values below each observation's detection limit and above
min_value

For non-detect observations (censored = 0), the value in value_col is
treated as the detection limit for that specific analysis, allowing for
different detection limits across samples or analytical methods.

IMPORTANT: This function should be applied to data containing only ONE
parameter at a time. Different environmental parameters have different
distributions and should not be modeled together.

When verbose = FALSE, the function operates silently except for critical
errors, making it suitable for batch processing of multiple parameters.

## Examples

``` r
# Load example data
data(multi_censored_data)

# Basic imputation with default settings
set.seed(123)
result <- impute_nondetect(
  dt = multi_censored_data,
  value_col = "value",
  cens_col = "censored",
  verbose = FALSE
)

# View imputed values for non-detects
head(result[censored == 0, .(value, value_imputed, value_final)])
#>    value value_imputed value_final
#>    <num>         <num>       <num>
#> 1:    25      8.487192    8.487192
#> 2:    15     10.300671   10.300671
#> 3:     5      4.937120    4.937120
#> 4:     5      4.798058    4.798058
#> 5:     5      4.408386    4.408386
#> 6:     5      4.284067    4.284067

# Check best distribution selected
attr(result, "best_distribution")
#> [1] "lognormal"

# With parameter and unit validation
result <- impute_nondetect(
  dt = multi_censored_data,
  value_col = "value",
  cens_col = "censored",
  parameter_col = "parameter",
  unit_col = "unit"
)

# For strictly positive values (avoiding exactly zero)
result <- impute_nondetect(
  dt = multi_censored_data,
  value_col = "value",
  cens_col = "censored",
  min_value = 1e-10,
  verbose = FALSE
)
```
