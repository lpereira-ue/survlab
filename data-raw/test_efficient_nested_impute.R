
library(data.table)
library(survlab)

# Alternative approach using data.table grouping (more efficient for very large datasets)
# Solution 1: Return a modified copy instead of using := on .SD
efficient_nested_impute <- function(dt, 
                                    wb_group_col = "wb_group",
                                    parameter_col = "param_name", 
                                    unit_col = "param_unit",
                                    value_col = "value",
                                    cens_col = "censored",
                                    min_value = 0,
                                    ...) {
  
  cat("Processing using data.table grouping (efficient for large datasets)...\n")
  
  results <- dt[, {
    tryCatch({
      # Run imputation for this group
      impute_nondetect(.SD, 
                       value_col = value_col,
                       cens_col = cens_col, 
                       parameter_col = parameter_col,
                       unit_col = unit_col,
                       min_value = min_value,
                       verbose = FALSE,
                       ...)
    }, error = function(e) {
      # Return original data with error columns (don't modify .SD directly)
      result_dt <- copy(.SD)
      result_dt[, (paste0(value_col, "_imputed")) := NA_real_]
      result_dt[, (paste0(value_col, "_final")) := get(value_col)]
      result_dt[, imputation_error := e$message]
      return(result_dt)
    })
  }, by = c(wb_group_col, parameter_col)]
  
  return(results)
}


# Ou use the efficient data.table function:
results <- efficient_nested_impute(river_data,
                                    wb_group_col = "wb_group",
                                    parameter_col = "param_name",
                                    unit_col = "param_unit",
                                    value_col = "value",
                                    cens_col = "censored",
                                    min_value = 0
)

