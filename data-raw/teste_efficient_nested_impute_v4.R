
library(data.table)
library(survlab)

t0 <- proc.time()


# SOLUTION 4: If you want to group by both wb_group AND parameter
efficient_nested_impute_v4 <- function(dt, 
                                       wb_group_col = "wb_group",
                                       parameter_col = "param_name", 
                                       unit_col = "param_unit",
                                       value_col = "value",
                                       cens_col = "censored",
                                       min_value = 0,
                                       ...) {
  
  # Get unique combinations of wb_group and parameter
  group_combinations <- unique(dt[, c(wb_group_col, parameter_col), with = FALSE])
  
  cat("Processing", nrow(group_combinations), "combinations...\n")
  
  results_list <- vector("list", nrow(group_combinations))
  
  for(i in seq_len(nrow(group_combinations))) {
    # Get data for this specific wb_group + parameter combination
    current_wb_group <- group_combinations[[wb_group_col]][i]
    current_param <- group_combinations[[parameter_col]][i]
    
    group_data <- dt[get(wb_group_col) == current_wb_group & 
                       get(parameter_col) == current_param]
    
    # cat("Processing:", current_wb_group, "-", current_param, "(", nrow(group_data), "rows )\n")
    
    results_list[[i]] <- tryCatch({
      impute_nondetect(group_data, 
                       value_col = value_col,
                       cens_col = cens_col, 
                       parameter_col = parameter_col,
                       min_value = min_value,
                       verbose = FALSE)
    }, error = function(e) {
      # cat("Error:", e$message, "\n")
      error_data <- copy(group_data)
      error_data[, (paste0(value_col, "_imputed")) := NA_real_]
      # error_data[, (paste0(value_col, "_final")) := get(value_col)]
      error_data[, (paste0(value_col, "_final")) := NA_real_]
      error_data[, imputation_error := as.character(e$message)]
      error_data
    })
  }
  
  # Combine all results
  result_dt <- rbindlist(results_list, use.names = TRUE, fill = TRUE)
  return(result_dt)
}


# This processes each wb_group + parameter combination separately
result <- efficient_nested_impute_v4(
  river_data,
  wb_group_col = "wb_group", # your actual wb_group column name
  parameter_col = "param_name",
  unit_col = "param_unit",
  value_col = "value",
  cens_col = "censored"
)

cat("\n", timetaken(t0), "\n")

