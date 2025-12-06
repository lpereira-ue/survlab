# Validate Laboratory Non-Detect Imputation Results

This function validates the quality of non-detect value imputation by
checking that imputed values are below their respective limits of
quantification and providing comprehensive summary statistics and model
diagnostics.

## Usage

``` r
validate_imputation(
  dt_imputed,
  value_col = "value",
  cens_col = "censored",
  verbose = TRUE
)
```

## Arguments

- dt_imputed:

  A data.table returned from
  [`impute_nondetect`](https://lpereira-ue.github.io/survlab/reference/impute_nondetect.md)

- value_col:

  Character string specifying the column name containing original values

- cens_col:

  Character string specifying the column name containing censoring
  indicators

- verbose:

  Logical indicating whether to print validation results to console
  (default: TRUE)

## Value

Invisibly returns the input data.table. When verbose = TRUE, prints
validation results to console including:

- Whether all imputed values are below their detection limits

- Number of duplicate imputed values (if any)

- Summary statistics by detection limit level

- Model fit information

## Details

The function checks:

- All imputed values are strictly below their respective limits of
  quantification

- Uniqueness of imputed values

- Summary statistics by limits of quantification level

- Model fit information including parameter and unit details

- Dataset characteristics (sample size, censoring percentage)

## Examples

``` r
data(multi_censored_data)
result <- impute_nondetect(multi_censored_data, verbose = FALSE)
validate_imputation(result)
#> === Laboratory Non-Detect Imputation Validation ===
#> Sample size: 200
#> Censoring percentage: 16.5 %
#> ✓ All imputed values are strictly below their respective detection limits
#> ✓ All imputed values are unique
#> 
#> Summary by limit of quantification level:
#>      loq count min_imputed max_imputed mean_imputed
#>    <num> <int>       <num>       <num>        <num>
#> 1:     5    10    1.714617    4.903044     3.373300
#> 2:     8     8    4.146878    7.799514     6.466325
#> 3:    15    12    3.755751   11.857531     8.251673
#> 4:    25     3   13.599483   22.597835    18.331666
#> Total non-detect observations: 33
#> Best distribution: lognormal
#> Model AIC: 1220.47
#> Detection limits found: 5, 8, 15, 25

# Silent validation for batch processing
validate_imputation(result, verbose = FALSE)
```
