# Environmental Laboratory Nitrate Data with Non-Detects

A synthetic dataset containing environmental nitrate measurements with
non-detect values, generated from a lognormal distribution. This dataset
represents typical water quality monitoring data from an environmental
laboratory, designed for demonstrating survival model-based imputation
techniques.

## Usage

``` r
multi_censored_data
```

## Format

A data.table with 200 rows and 4 variables:

- parameter:

  Character string indicating the chemical parameter ("Nitrate")

- unit:

  Character string indicating the unit of measurement ("mg/l NO3")

- value:

  Numeric values representing either detected measurements or detection
  limits for non-detect observations

- censored:

  Integer indicator where 0 = non-detect (below detection limit), 1 =
  detected (above detection limit)

## Source

Synthetic data generated for package demonstration, based on typical
environmental water quality monitoring programs

## Details

This dataset simulates real-world environmental water quality data where
nitrate measurements below certain detection limits are reported as
non-detects. The data includes:

- Single parameter (Nitrate) with consistent units (mg/l NO3)

- Multiple detection limit levels reflecting different analytical
  conditions

- Realistic distribution of detected vs non-detect values (83.5

- Detection limits ranging from 5 to 25 mg/l NO3

- Lognormal distribution typical of environmental contaminant data

For non-detect observations (censored = 0), the 'value' column contains
the detection limit for that specific analysis. For detected
measurements (censored = 1), the 'value' column contains the actual
measured nitrate concentration.

## Examples

``` r
data(multi_censored_data)

# Basic data exploration
multi_censored_data[, .(
  total_samples = .N,
  non_detects = sum(censored == 0),
  detects = sum(censored == 1)
)]
#>    total_samples non_detects detects
#>            <int>       <int>   <int>
#> 1:           200          33     167

# View parameter and unit information
multi_censored_data[, .(
  parameter = unique(parameter),
  unit = unique(unit)
)]
#>    parameter     unit
#>       <char>   <char>
#> 1:   Nitrate mg/l NO3

# View detection limit levels
multi_censored_data[censored == 0, unique(value)]
#> [1] 25 15  5  8

# Apply survival model imputation
result <- impute_nondetect(multi_censored_data,
                          parameter_col = "parameter",
                          unit_col = "unit")
validate_imputation(result)
#> === Laboratory Non-Detect Imputation Validation ===
#> Parameter: Nitrate
#> Unit: mg/l NO3
#> Sample size: 200
#> Censoring percentage: 16.5 %
#> ✓ All imputed values are strictly below their respective detection limits
#> ✓ All imputed values are unique
#> 
#> Summary by limit of quantification level:
#>      loq count min_imputed max_imputed mean_imputed
#>    <num> <int>       <num>       <num>        <num>
#> 1:     5    10    2.908333    4.851259     4.019263
#> 2:     8     8    3.655422    7.576604     5.746114
#> 3:    15    12    6.320140   13.368316     8.867565
#> 4:    25     3    5.466556   12.248956     8.491381
#> Total non-detect observations: 33
#> Best distribution: lognormal
#> Model AIC: 1220.47
#> Detection limits found: 5, 8, 15, 25
```
