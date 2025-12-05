
library(data.table)
library(survlab)

t0 <- proc.time()


efficient_nested_impute_v5 <- function(dt, 
                                       wb_group_col = "wb_group",
                                       parameter_col = "param_name", 
                                       unit_col = "param_unit",
                                       value_col = "value",
                                       cens_col = "censored",
                                       min_value = 0,
                                       ...) {
  
  # Get unique combinations of wb_group and parameter
  group_combinations <- unique(dt[, c(wb_group_col, parameter_col), with = FALSE])
  n_total <- nrow(group_combinations)
  
  # cat("Processing", n_total, "combinations...\n")
  
  results_list <- vector("list", n_total)
  
  for(i in seq_len(n_total)) {
    current_wb_group <- group_combinations[[wb_group_col]][i]
    current_param <- group_combinations[[parameter_col]][i]
    
    group_data <- dt[get(wb_group_col) == current_wb_group & 
                       get(parameter_col) == current_param]
    
    results_list[[i]] <- tryCatch({
      impute_nondetect(group_data, 
                       value_col = value_col,
                       cens_col = cens_col, 
                       parameter_col = parameter_col,
                       min_value = min_value,
                       verbose = FALSE)
    }, error = function(e) {
      error_data <- copy(group_data)
      error_data[, (paste0(value_col, "_imputed")) := NA_real_]
      error_data[, (paste0(value_col, "_final")) := NA_real_]
      error_data[, imputation_error := as.character(e$message)]
      error_data
    })
    
    # One-line dynamic progress
    percent <- round(i / n_total * 100)
    cat(sprintf("\rProcessing %d (%d%%) of %d combinations...", i, percent, n_total))
    flush.console()
  }
  
  cat("\nDone.\n")
  return(rbindlist(results_list, use.names = TRUE, fill = TRUE))
}


# This processes each wb_group + parameter combination separately
result <- efficient_nested_impute_v5(
  river_data,
  wb_group_col = "wb_group", # your actual wb_group column name
  parameter_col = "param_name",
  unit_col = "param_unit",
  value_col = "value",
  cens_col = "censored"
)

cat("\n", timetaken(t0), "\n")

