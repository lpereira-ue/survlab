library(testthat)
library(survlab)
library(data.table)

test_that("impute_nondetect works with example data", {
  data(multi_censored_data)

  # Test basic functionality
  set.seed(123)
  result <- impute_nondetect(
    multi_censored_data,
    parameter_col = "parameter",
    unit_col = "unit",
    verbose = FALSE
  )

  # Check that result is a data.table
  expect_s3_class(result, "data.table")

  # Check that new columns are created
  expect_true("value_imputed" %in% names(result))
  expect_true("value_final" %in% names(result))

  # Check that attributes are set
  expect_true(!is.null(attr(result, "best_distribution")))
  expect_true(!is.null(attr(result, "aic")))
  expect_true(!is.null(attr(result, "best_model")))
  expect_true(!is.null(attr(result, "detection_limits")))
  expect_true(!is.null(attr(result, "max_detection_limit")))
  expect_true(!is.null(attr(result, "parameter")))
  expect_true(!is.null(attr(result, "unit")))
  expect_true(!is.null(attr(result, "sample_size")))
  expect_true(!is.null(attr(result, "censored_pct")))

  # Check that imputed values are below detection limits
  censored_data <- result[censored == 0]
  if (nrow(censored_data) > 0) {
    expect_true(all(censored_data$value_imputed < censored_data$value))
  }

  # Check that final values are correctly assigned
  expect_equal(
    result[censored == 1, value_final],
    result[censored == 1, value]
  )
  expect_equal(
    result[censored == 0, value_final],
    result[censored == 0, value_imputed]
  )

  # Check parameter and unit attributes
  expect_equal(attr(result, "parameter"), "Nitrate")
  expect_equal(attr(result, "unit"), "mg/l NO3")
})

test_that("input validation works", {
  data(multi_censored_data)

  # Test missing column error
  expect_error(
    impute_nondetect(multi_censored_data, value_col = "nonexistent"),
    "Column nonexistent not found"
  )

  expect_error(
    impute_nondetect(multi_censored_data, cens_col = "nonexistent"),
    "Column nonexistent not found"
  )

  expect_error(
    impute_nondetect(multi_censored_data, parameter_col = "nonexistent"),
    "Column nonexistent not found"
  )

  expect_error(
    impute_nondetect(multi_censored_data, unit_col = "nonexistent"),
    "Column nonexistent not found"
  )
})

test_that("sample size validation works", {
  # Create small dataset
  small_data <- data.table(
    parameter = rep("Test", 10),
    unit = rep("mg/L", 10),
    value = c(rep(1, 5), rep(2, 5)),
    censored = c(rep(0, 5), rep(1, 5))
  )

  # Should fail with default min_observations = 25
  expect_error(
    impute_nondetect(small_data),
    "Insufficient data.*only 10 observations.*Minimum required: 25"
  )

  # Should work with lower threshold
  set.seed(123)
  result <- impute_nondetect(small_data, min_observations = 10, verbose = FALSE)
  expect_s3_class(result, "data.table")
})

test_that("censoring percentage validation works", {
  # Create dataset with too many censored values
  high_censored_data <- data.table(
    parameter = rep("Test", 100),
    unit = rep("mg/L", 100),
    value = c(rep(1, 80), rep(2, 20)), # 80% censored
    censored = c(rep(0, 80), rep(1, 20))
  )

  # Should fail with default max_censored_pct = 75
  expect_error(
    impute_nondetect(high_censored_data),
    "Too many censored values.*80.*Maximum allowed: 75"
  )

  # Should work with higher threshold
  set.seed(123)
  result <- impute_nondetect(high_censored_data, max_censored_pct = 85, verbose = FALSE)
  expect_s3_class(result, "data.table")
})

test_that("parameter validation works", {
  # Create dataset with multiple parameters
  multi_param_data <- data.table(
    parameter = c(rep("Nitrate", 50), rep("Phosphate", 50)),
    unit = rep("mg/L", 100),
    value = rep(c(1, 2), 50),
    censored = rep(c(0, 1), 50)
  )

  expect_error(
    impute_nondetect(multi_param_data, parameter_col = "parameter"),
    "Multiple parameters found.*Nitrate, Phosphate"
  )
})

test_that("unit validation works", {
  # Create dataset with multiple units
  multi_unit_data <- data.table(
    parameter = rep("Nitrate", 100),
    unit = c(rep("mg/L", 50), rep("ug/L", 50)),
    value = rep(c(1, 2), 50),
    censored = rep(c(0, 1), 50)
  )

  expect_error(
    impute_nondetect(multi_unit_data, unit_col = "unit"),
    "Multiple units found.*mg/L, ug/L"
  )
})

test_that("function handles data with no non-detect observations", {
  # Create data with no non-detect observations (without pre-existing imputed columns)
  test_data <- data.table(
    parameter = rep("Test", 5),
    unit = rep("mg/L", 5),
    value = c(1, 2, 3, 4, 5),
    censored = c(1, 1, 1, 1, 1)
  )

  # Run with verbose = FALSE first to check basic functionality
  result <- impute_nondetect(test_data, min_observations = 5, verbose = FALSE)

  # Should return data with imputed columns added
  expect_equal(nrow(result), 5)
  expect_true("value_imputed" %in% names(result))
  expect_true("value_final" %in% names(result))
  
  # value_imputed should be all NA, value_final should equal value
  expect_true(all(is.na(result$value_imputed)))
  expect_equal(result$value_final, result$value)
})

test_that("function produces message for data with no non-detects when verbose", {
 # Create data with no non-detect observations
  test_data <- data.table(
    parameter = rep("Test", 5),
    unit = rep("mg/L", 5),
    value = c(1, 2, 3, 4, 5),
    censored = c(1, 1, 1, 1, 1)
  )

  # Check that message is produced when verbose = TRUE
  expect_message(
    impute_nondetect(test_data, min_observations = 5, verbose = TRUE),
    "No non-detect observations found"
  )
})

test_that("function correctly auto-detects detection limits", {
  # Create data with known detection limits
  test_data <- data.table(
    parameter = rep("Test", 30),
    unit = rep("mg/L", 30),
    value = c(rep(0.1, 5), rep(0.5, 5), rep(1.0, 10), rep(2.0, 10)),
    censored = c(rep(0, 10), rep(1, 20)) # First 10 are non-detects
  )

  set.seed(123)
  result <- impute_nondetect(test_data, min_observations = 25, verbose = FALSE)

  # Check that detection limits are correctly identified
  expected_limits <- c(0.1, 0.5)
  expect_equal(sort(attr(result, "detection_limits")), sort(expected_limits))
  expect_equal(attr(result, "max_detection_limit"), 0.5)
})

test_that("validate_imputation works correctly", {
  data(multi_censored_data)

  set.seed(123)
  result <- impute_nondetect(
    multi_censored_data,
    parameter_col = "parameter",
    unit_col = "unit",
    verbose = FALSE
  )

  # Should run without error and return the input invisibly
  validation_result <- validate_imputation(result, verbose = FALSE)
  expect_identical(validation_result, result)
})

test_that("validate_imputation verbose output works", {
  data(multi_censored_data)

  set.seed(123)
  result <- impute_nondetect(
    multi_censored_data,
    parameter_col = "parameter",
    unit_col = "unit",
    verbose = FALSE
  )

  # Should produce messages when verbose = TRUE
  expect_message(
    validate_imputation(result, verbose = TRUE),
    "Laboratory Non-Detect Imputation Validation"
  )
})

test_that("function works with different column names", {
  data(multi_censored_data)

  # Rename columns
  test_data <- copy(multi_censored_data)
  setnames(test_data, c("value", "censored"), c("measurement", "is_censored"))

  set.seed(123)
  result <- impute_nondetect(
    test_data,
    value_col = "measurement",
    cens_col = "is_censored",
    parameter_col = "parameter",
    unit_col = "unit",
    verbose = FALSE
  )

  expect_true("measurement_imputed" %in% names(result))
  expect_true("measurement_final" %in% names(result))
})

test_that("function works with regular data.frame input", {
  data(multi_censored_data)

  # Convert to regular data.frame
  df <- as.data.frame(multi_censored_data)

  set.seed(123)
  result <- impute_nondetect(df, parameter_col = "parameter", unit_col = "unit", verbose = FALSE)

  # Should still work and return data.table
  expect_s3_class(result, "data.table")
  expect_true("value_imputed" %in% names(result))
})