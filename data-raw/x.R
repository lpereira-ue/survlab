
# First, let's create a debugging version to see what's happening
debug_nested_impute <- function(dt, 
                                wb_group_col = "wb_group",
                                parameter_col = "param_name", 
                                unit_col = "param_unit",
                                value_col = "value",
                                cens_col = "censored",
                                min_value = 0,
                                ...) {
  
  cat("=== DEBUGGING NESTED IMPUTE ===\n")
  cat("Input data dimensions:", nrow(dt), "x", ncol(dt), "\n")
  cat("Column names:", paste(names(dt), collapse = ", "), "\n")
  cat("Parameter column specified:", parameter_col, "\n")
  cat("Value column specified:", value_col, "\n")
  cat("Censored column specified:", cens_col, "\n")
  cat("WB group column specified:", wb_group_col, "\n")
  
  # Check if columns exist
  missing_cols <- setdiff(c(wb_group_col, parameter_col, value_col, cens_col), names(dt))
  if(length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check unique groups
  group_summary <- dt[, .(n_rows = .N), by = c(wb_group_col, parameter_col)]
  cat("Number of groups:", nrow(group_summary), "\n")
  print(head(group_summary))
  
  results <- dt[, {
    cat("Processing group:", get(wb_group_col)[1], "-", get(parameter_col)[1], "\n")
    cat("Group size:", nrow(.SD), "\n")
    cat("Group columns:", paste(names(.SD), collapse = ", "), "\n")
    
    # Check if required columns exist in .SD
    required_cols <- c(parameter_col, value_col, cens_col)
    missing_in_sd <- setdiff(required_cols, names(.SD))
    if(length(missing_in_sd) > 0) {
      cat("Missing columns in .SD:", paste(missing_in_sd, collapse = ", "), "\n")
    }
    
    tryCatch({
      # Run imputation for this group
      cat("Calling impute_nondetect...\n")
      result <- impute_nondetect(.SD, 
                                 value_col = value_col,
                                 cens_col = cens_col, 
                                 parameter_col = parameter_col,
                                 min_value = min_value,
                                 verbose = TRUE,  # Turn on verbose for debugging
                                 ...)
      cat("Imputation successful, result has", nrow(result), "rows\n")
      return(result)
      
    }, error = function(e) {
      cat("Error in imputation:", e$message, "\n")
      # Return original data with error columns
      result_dt <- copy(.SD)
      result_dt[, (paste0(value_col, "_imputed")) := NA_real_]
      result_dt[, (paste0(value_col, "_final")) := get(value_col)]
      result_dt[, imputation_error := e$message]
      return(result_dt)
    })
  }, by = c(wb_group_col, parameter_col)]
  
  return(results)
}

# Fixed version that handles column names properly
efficient_nested_impute_fixed <- function(dt, 
                                          wb_group_col = "wb_group",
                                          parameter_col = "param_name", 
                                          unit_col = "param_unit",
                                          value_col = "value",
                                          cens_col = "censored",
                                          min_value = 0,
                                          ...) {
  
  cat("Processing using data.table grouping (efficient for large datasets)...\n")
  
  # Validate input columns exist
  required_cols <- c(wb_group_col, parameter_col, value_col, cens_col)
  missing_cols <- setdiff(required_cols, names(dt))
  if(length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Method 1: If impute_nondetect expects specific column names, rename temporarily
  results <- dt[, {
    group_data <- copy(.SD)
    
    # Check if we need to rename columns for impute_nondetect
    # (some functions expect specific column names)
    temp_names <- names(group_data)
    
    tryCatch({
      # Call imputation function
      imputed_result <- impute_nondetect(group_data, 
                                         value_col = value_col,
                                         cens_col = cens_col, 
                                         parameter_col = parameter_col,
                                         min_value = min_value,
                                         verbose = FALSE,
                                         ...)
      
      return(imputed_result)
      
    }, error = function(e) {
      # Return original data with error information
      group_data[, (paste0(value_col, "_imputed")) := NA_real_]
      group_data[, (paste0(value_col, "_final")) := get(value_col)]
      group_data[, imputation_error := as.character(e$message)]
      return(group_data)
    })
  }, by = c(wb_group_col, parameter_col)]
  
  return(results)
}

# Alternative: Manual column renaming if impute_nondetect expects specific names
efficient_nested_impute_renamed <- function(dt, 
                                            wb_group_col = "wb_group",
                                            parameter_col = "param_name", 
                                            unit_col = "param_unit",
                                            value_col = "value",
                                            cens_col = "censored",
                                            min_value = 0,
                                            ...) {
  
  cat("Processing with column renaming approach...\n")
  
  results <- dt[, {
    group_data <- copy(.SD)
    
    # If impute_nondetect expects specific column names, rename them
    # This might be necessary if the function looks for literal column names
    old_names <- names(group_data)
    
    # Create a mapping for renaming (if needed)
    name_mapping <- c()
    if(parameter_col != "param_name" && parameter_col %in% old_names) {
      name_mapping["param_name"] <- parameter_col
    }
    if(value_col != "value" && value_col %in% old_names) {
      name_mapping["value"] <- value_col
    }
    if(cens_col != "censored" && cens_col %in% old_names) {
      name_mapping["censored"] <- cens_col
    }
    
    # Rename columns if needed
    if(length(name_mapping) > 0) {
      setnames(group_data, old = name_mapping, new = names(name_mapping))
    }
    
    tryCatch({
      # Call imputation with potentially renamed columns
      imputed_result <- impute_nondetect(group_data, 
                                         value_col = "value",  # Use standard names
                                         cens_col = "censored", 
                                         parameter_col = "param_name",
                                         min_value = min_value,
                                         verbose = FALSE,
                                         ...)
      
      # Rename back to original names if needed
      if(length(name_mapping) > 0) {
        setnames(imputed_result, old = names(name_mapping), new = name_mapping)
      }
      
      return(imputed_result)
      
    }, error = function(e) {
      # Rename back to original names before returning error
      if(length(name_mapping) > 0) {
        setnames(group_data, old = names(name_mapping), new = name_mapping)
      }
      
      group_data[, (paste0(value_col, "_imputed")) := NA_real_]
      group_data[, (paste0(value_col, "_final")) := get(value_col)]
      group_data[, imputation_error := as.character(e$message)]
      return(group_data)
    })
  }, by = c(wb_group_col, parameter_col)]
  
  return(results)
}

# Test function to check your data structure
check_data_structure <- function(dt, 
                                 wb_group_col = "wb_group",
                                 parameter_col = "param_name", 
                                 value_col = "value",
                                 cens_col = "censored") {
  
  cat("=== DATA STRUCTURE CHECK ===\n")
  cat("Dimensions:", nrow(dt), "rows x", ncol(dt), "columns\n")
  cat("Column names:", paste(names(dt), collapse = ", "), "\n\n")
  
  # Check if specified columns exist
  specified_cols <- c(wb_group_col, parameter_col, value_col, cens_col)
  existing_cols <- intersect(specified_cols, names(dt))
  missing_cols <- setdiff(specified_cols, names(dt))
  
  cat("Specified columns that exist:", paste(existing_cols, collapse = ", "), "\n")
  if(length(missing_cols) > 0) {
    cat("MISSING columns:", paste(missing_cols, collapse = ", "), "\n")
  }
  
  # Show summary by group
  if(all(c(wb_group_col, parameter_col) %in% names(dt))) {
    cat("\nGroup summary:\n")
    group_summary <- dt[, .(n_rows = .N), by = c(wb_group_col, parameter_col)]
    print(group_summary)
  }
  
  # Show sample of data
  cat("\nFirst few rows:\n")
  print(head(dt))
}


# This will show you exactly what's happening
result <- debug_nested_impute(data_rivers, 
                              wb_group_col = "wb_group",
                              parameter_col = "param_name",
                              value_col = "value", 
                              cens_col = "censored")

