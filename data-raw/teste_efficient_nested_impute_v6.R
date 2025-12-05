
library(data.table)
library(survlab)

t0 <- proc.time()
cat("\n")

efficient_nested_impute_v6 <- function(dt, 
  wb_group_col = "wb_group",
  parameter_col = "param_name", 
  unit_col = "param_unit",
  value_col = "value",
  cens_col = "censored",
  min_value = 0,
  ...) {

group_combinations <- unique(dt[, c(wb_group_col, parameter_col), with = FALSE])
n_total <- nrow(group_combinations)

# cat("Processing", n_total, "combinations...\n")

results_list <- vector("list", n_total)
summary_list <- vector("list", n_total)  # To collect attributes

for(i in seq_len(n_total)) {
current_wb_group <- group_combinations[[wb_group_col]][i]
current_param <- group_combinations[[parameter_col]][i]

group_data <- dt[get(wb_group_col) == current_wb_group & 
get(parameter_col) == current_param]

result <- tryCatch({
imputed <- impute_nondetect(group_data, 
value_col = value_col,
cens_col = cens_col, 
parameter_col = parameter_col,
min_value = min_value,
verbose = FALSE)

# Extract attributes
attrs <- attributes(imputed)
summary_list[[i]] <- data.table(
  wb_group = as.character(current_wb_group),
  parameter = as.character(attrs$parameter %||% current_param),
  unit = as.character(attrs$unit %||% NA_character_),
  # best_model = as.character(attrs$best_model %||% NA_character_),
  best_model = NA_character_,
  best_distribution = as.character(attrs$best_distribution %||% NA_character_),
  detection_limits = paste0(attrs$detection_limits %||% NA, collapse = ","),  # Always character
  max_detection_limit = as.numeric(attrs$max_detection_limit %||% NA_real_),
  aic = as.numeric(attrs$aic %||% NA_real_),
  sample_size = as.integer(attrs$sample_size %||% NA_integer_),
  censored_pct = as.numeric(attrs$censored_pct %||% NA_real_),
  min_value = as.numeric(attrs$min_value %||% min_value)
)

imputed
}, error = function(e) {
  summary_list[[i]] <- data.table(
    wb_group = as.character(current_wb_group),
    parameter = as.character(current_param),
    unit = NA_character_,
    best_model = NA_character_,
    best_distribution = NA_character_,
    detection_limits = NA_character_,
    max_detection_limit = NA_real_,
    aic = NA_real_,
    sample_size = as.integer(nrow(group_data)),
    censored_pct = NA_real_,
    min_value = as.numeric(min_value)
  )

error_data <- copy(group_data)
error_data[, (paste0(value_col, "_imputed")) := NA_real_]
error_data[, (paste0(value_col, "_final")) := NA_real_]
error_data[, imputation_error := as.character(e$message)]
error_data
})

results_list[[i]] <- result

cat(sprintf("\rProcessing %d (%d%%) of %d combinations ...", i, round(i / n_total * 100), n_total))
flush.console()
}

cat("\nDone.\n")

all_results <- rbindlist(results_list, use.names = TRUE, fill = TRUE)
summary_table <- rbindlist(summary_list, use.names = TRUE, fill = TRUE)

return(list(
data = all_results,
summary = summary_table
))
}



# This processes each wb_group + parameter combination separately
result <- efficient_nested_impute_v6(
  river_data,
  wb_group_col = "wb_group", # your actual wb_group column name
  parameter_col = "param_name",
  unit_col = "param_unit",
  value_col = "value",
  cens_col = "censored"
)

file_result <- here::here("OUTPUT/IMPUTED_CENSORED_DATA.xlsx")
openxlsx::write.xlsx(result, file_result, withFilter = TRUE, firstRow = TRUE)
cli::cli_alert_success("File {.file {file_result}} saved")

cat("\n", timetaken(t0), "\n")

