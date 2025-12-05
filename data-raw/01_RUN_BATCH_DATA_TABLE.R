
library(data.table)
library(survlab)


# Alternative approach using data.table grouping (more efficient for very large datasets)
efficient_nested_impute <- function(
  dt,
  wb_group_col = "wb_group",
  parameter_col = "parameter",
  value_col = "value",
  cens_col = "censored",
  min_value = 0,
  ...
) {
  # This approach uses data.table's by= functionality for efficiency
  cat(
    "Processing using data.table grouping (efficient for large datasets)...\n"
  )

  results <- dt[,
    {
      tryCatch(
        {
          # Run imputation for this group
          impute_nondetect(
            .SD,
            value_col = value_col,
            cens_col = cens_col,
            parameter_col = parameter_col,
            min_value = min_value,
            verbose = FALSE,
            ...
          )
        },
        error = function(e) {
          # Return original data with error flag if imputation fails
          .SD[, `:=`(
            paste0(value_col, "_imputed") = NA_real_,
            paste0(value_col, "_final") = get(value_col),
            imputation_error = e$message
          )]
          return(.SD)
        }
      )
    },
    by = c(wb_group_col, parameter_col)
  ]

  return(results)
}


