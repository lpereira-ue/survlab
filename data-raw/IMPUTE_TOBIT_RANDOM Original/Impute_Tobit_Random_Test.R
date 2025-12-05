
# Load required libraries
library(data.table)
library(survival)
library(truncnorm)
library(ggplot2)

cat("\n")

# Load functions
source("impute_tobit_random_function.R")

# Read your data
dt <- fread("multi_censored_lognormal_data.csv")

# Quick data exploration
dt[, .(
  total_obs = .N,
  censored = sum(censored == 0),
  observed = sum(censored == 1),
  min_value = min(value),
  max_value = max(value),
  max_loq_detected = max(value[censored == 0]) # Maximum limit of quantification (loq)
)]

# Apply the imputation function
set.seed(123)  # For reproducibility
dt_imputed <- impute_tobit_random(dt, value_col = "value", cens_col = "censored")

cat("\n")

# Validate the imputation
validate_imputation(dt_imputed)

# Check the results
dt_imputed[censored == 0, .(
  original_value = value,
  imputed_value = round(value_imputed, 3),
  final_value = round(value_final, 3)
)][1:10]  # Show first 10 censored observations

# Compare distributions before and after imputation
comparison_plot <- function(dt_imputed) {
  
  # Prepare data for plotting
  plot_data <- rbind(
    dt_imputed[censored == 1, .(value = value, type = "Observed")],
    dt_imputed[censored == 0, .(value = value_imputed, type = "Imputed")]
  )
  
  p1 <- ggplot(plot_data, aes(x = value, fill = type)) +
    geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
    geom_vline(xintercept = attr(dt_imputed, "loq"), 
               linetype = "dashed", color = "red", linewidth = 1) +
    labs(title = "Distribution Comparison: Observed vs Imputed Values",
         subtitle = paste("Red line shows LOQ =", attr(dt_imputed, "loq")),
         x = "Value", y = "Count", fill = "Type") +
    theme_minimal()
  
  print(p1)
  
  # Q-Q plot to check if imputed values follow expected distribution
  p2 <- ggplot(dt_imputed[censored == 0], aes(sample = value_imputed)) +
    stat_qq() + stat_qq_line() +
    labs(title = "Q-Q Plot of Imputed Values",
         subtitle = paste("Expected distribution:", attr(dt_imputed, "best_distribution"))) +
    theme_minimal()
  
  print(p2)
}

# Create comparison plots
comparison_plot(dt_imputed)


# Alternative: Multiple imputation approach
# Generate multiple sets of imputed values for uncertainty quantification
# multiple_imputation <- function(dt, n_imputations = 5) {
  
#   imputation_list <- list()
  
#   for (i in 1:n_imputations) {
#     set.seed(123 + i)  # Different seed for each imputation
#     dt_imp <- impute_tobit_random(dt, value_col = "value", cens_col = "censored")
    
#     # Store just the imputed values
#     imputation_list[[i]] <- dt_imp[censored == 0, value_imputed]
#   }
  
#   # Combine into data.table
#   imputation_dt <- as.data.table(do.call(cbind, imputation_list))
#   setnames(imputation_dt, paste0("imputation_", 1:n_imputations))
  
#   # Add row indices for censored observations
#   imputation_dt[, row_id := which(dt$censored == 0)]
  
#   return(imputation_dt)
# }

# # Example of multiple imputation
# multiple_imp <- multiple_imputation(dt, n_imputations = 5)
# print("Multiple imputations for first 5 censored observations:")
# print(multiple_imp[1:5])

# Summary: Key improvements over original function
cat("\n=== KEY IMPROVEMENTS ===\n")
cat("1. ✓ Each imputed value is unique (random sampling)\n")
cat("2. ✓ All imputed values are below LOQ (truncated distributions)\n")
cat("3. ✓ Automatic LOQ detection\n")
cat("4. ✓ Proper handling of different distributions\n")
cat("5. ✓ Built-in validation and diagnostics\n")
cat("6. ✓ data.table syntax throughout for fast speed and memory efficient\n")
# cat("7. ✓ Multiple imputation capability for uncertainty quantification\n")
