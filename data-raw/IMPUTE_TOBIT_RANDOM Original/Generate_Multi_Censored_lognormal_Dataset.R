library(data.table)

# Set seed for reproducibility
set.seed(42)

# Parameters for lognormal distribution
n <- 200
meanlog <- 2.5 # log-scale mean
sdlog <- 0.6 # log-scale standard deviation

# Generate the underlying lognormal data
true_values <- rlnorm(n, meanlog = meanlog, sdlog = sdlog)

# Define multiple censoring thresholds
# This simulates different detection limits or measurement constraints
censoring_thresholds <- c(8, 15, 25)
censoring_probs <- c(0.15, 0.10, 0.05) # Probability of each type of censoring

# Initialize the dataset
dt <- data.table(
  id = 1:n,
  true_value = true_values,
  value = true_values,
  censored = 1L # Start with all observations as uncensored
)

# Apply multiple types of censoring
for (i in seq_along(censoring_thresholds)) {
  threshold <- censoring_thresholds[i]
  prob <- censoring_probs[i]

  # Randomly select observations to censor at this threshold
  # Focus on values near the threshold for realistic censoring
  candidates <- which(dt$true_value <= threshold * 1.5 & dt$censored == 1)

  if (length(candidates) > 0) {
    n_to_censor <- rbinom(1, length(candidates), prob)
    if (n_to_censor > 0) {
      to_censor <- sample(candidates, min(n_to_censor, length(candidates)))
      dt[
        to_censor,
        `:=`(
          value = threshold,
          censored = 0L
        )
      ]
    }
  }
}

# Add some random left-censoring for values below detection
# This simulates instrument detection limits
detection_limit <- 5
below_detection <- dt$true_value < detection_limit & dt$censored == 1
if (sum(below_detection) > 0) {
  dt[
    below_detection,
    `:=`(
      value = detection_limit,
      censored = 0L
    )
  ]
}

# Summary statistics
cat("Dataset Summary:\n")
cat("Total observations:", nrow(dt), "\n")
cat("Censored observations:", sum(dt$censored == 0), "\n")
cat("Observed values:", sum(dt$censored == 1), "\n")
cat("Censoring rate:", round(mean(dt$censored == 0) * 100, 1), "%\n\n")

# Show unique censored values
censored_values <- unique(dt[censored == 0, value])
cat("Unique censored values:", sort(censored_values), "\n\n")

# Display first few rows
cat("First 10 rows:\n")
print(head(dt[, .(value, censored)], 10))

# Create summary by censoring status
summary_dt <- dt[,
  .(
    count = .N,
    min_value = min(value),
    max_value = max(value),
    mean_value = round(mean(value), 2)
  ),
  by = .(censored)
]

cat("\nSummary by censoring status:\n")
print(summary_dt)

# Save the dataset
final_dt <- dt[, .(value, censored)]
fwrite(final_dt, "multi_censored_lognormal_data.csv")

cat("\nDataset saved as 'multi_censored_lognormal_data.csv'\n")

# Optional: Create a plot to visualize the data
if (require(ggplot2, quietly = TRUE)) {
  p <- ggplot(dt, aes(x = value, fill = factor(censored))) +
    geom_histogram(alpha = 0.7, bins = 30) +
    scale_fill_manual(
      values = c("0" = "red", "1" = "blue"),
      labels = c("0" = "Censored", "1" = "Observed"),
      name = "Status"
    ) +
    labs(
      title = "Distribution of Multi-Censored Lognormal Data",
      x = "Value",
      y = "Frequency"
    ) +
    theme_minimal()

  print(p)

  # Also show the true vs observed values
  p2 <- ggplot(dt, aes(x = true_value, y = value, color = factor(censored))) +
    geom_point(alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    scale_color_manual(
      values = c("0" = "red", "1" = "blue"),
      labels = c("0" = "Censored", "1" = "Observed"),
      name = "Status"
    ) +
    labs(
      title = "True vs Observed Values",
      x = "True Value",
      y = "Observed Value"
    ) +
    theme_minimal()

  print(p2)
}