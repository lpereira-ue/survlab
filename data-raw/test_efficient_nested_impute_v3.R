

library(data.table)
library(survlab)

to <- proc.time()

# Solution 3: Split-apply-combine approach (most readable)
efficient_nested_impute_v3 <- function(dt, 
                                       wb_group_col = "wb_group",
                                       parameter_col = "param_name", 
                                       unit_col = "param_unit",
                                       value_col = "value",
                                       cens_col = "censored",
                                       min_value = 0,
                                       ...) {
  
  cat("Processing using split-apply-combine approach...\n")
  
  # Get unique groups
  group_cols <- c(wb_group_col, parameter_col)
  groups <- unique(dt[, ..group_cols])
  
  # Process each group
  results_list <- vector("list", nrow(groups))
  
  for(i in seq_len(nrow(groups))) {
    group_data <- dt[groups[i], on = group_cols]
    
    tryCatch({
      # Run imputation for this group
      imputed_data <- impute_nondetect(group_data, 
                                       value_col = value_col,
                                       cens_col = cens_col, 
                                       parameter_col = parameter_col,
                                       min_value = min_value,
                                       verbose = FALSE,
                                       ...)
      results_list[[i]] <- imputed_data
      
    }, error = function(e) {
      # Return original data with error columns
      error_data <- copy(group_data)
      error_data[, (paste0(value_col, "_imputed")) := NA_real_]
      error_data[, (paste0(value_col, "_final")) := get(value_col)]
      error_data[, imputation_error := e$message]
      results_list[[i]] <- error_data
    })
  }
  
  # Combine results
  result_dt <- rbindlist(results_list, use.names = TRUE, fill = TRUE)
  return(result_dt)
}


results <- efficient_nested_impute_v3(river_data,
                                        wb_group_col = "wb_group",
                                        parameter_col = "param_name",
                                        unit_col = "param_unit",
                                        value_col = "value",
                                        cens_col = "censored",
                                        min_value = 0
)


cat("\n", timetaken(to), "\n")
