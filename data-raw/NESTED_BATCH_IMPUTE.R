library(data.table)
library(progress)

#' Nested Batch Imputation by Waterbody Group and Parameter
#'
#' Process environmental data with nested grouping: first by waterbody group,
#' then by parameter within each group. Each combination gets its own
#' distribution fitting.
#'
#' @param dt data.table containing environmental data
#' @param wb_group_col Column name for waterbody grouping (e.g., "wb_group")
#' @param parameter_col Column name containing parameter identifiers
#' @param value_col Column name containing concentration values
#' @param cens_col Column name containing censoring indicators (0=non-detect, 1=detect)
#' @param unit_col Optional column name containing units
#' @param min_value Minimum allowable concentration (default: 0)
#' @param show_progress Whether to show progress bar (default: TRUE)
#' @param verbose Whether to show detailed messages (default: FALSE)
#' @param ... Additional arguments passed to impute_nondetect
#'
#' @return List containing:
#'   - data: Combined imputed dataset for all groups
#'   - summary: Summary table with wb_group, parameter, and results
#'   - wb_group_summary: High-level summary by waterbody group
#'   - failed_combinations: Data.table of failed wb_group + parameter combinations
#'
nested_batch_impute <- function(
  dt,
  wb_group_col = "wb_group",
  parameter_col = "parameter",
  value_col = "value",
  cens_col = "censored",
  unit_col = NULL,
  min_value = 0,
  show_progress = TRUE,
  verbose = FALSE,
  ...
) {
  # Convert to data.table if needed
  if (!data.table::is.data.table(dt)) {
    dt <- data.table::as.data.table(dt)
  }

  # Get unique combinations
  combinations <- dt[, unique(.SD), .SDcols = c(wb_group_col, parameter_col)]
  setnames(combinations, c("wb_group", "parameter"))
  n_combinations <- nrow(combinations)

  # Get summary counts
  wb_groups <- dt[, unique(get(wb_group_col))]
  n_wb_groups <- length(wb_groups)

  cat(
    "Processing",
    n_combinations,
    "combinations across",
    n_wb_groups,
    "waterbody groups...\n"
  )

  # Initialize progress bar
  if (show_progress) {
    pb <- progress_bar$new(
      format = "  [:bar] :percent (:current/:total) | :wb_group - :param",
      total = n_combinations,
      clear = FALSE,
      width = 80
    )
  }

  # Initialize storage
  all_results <- list()
  failed_combinations <- data.table(
    wb_group = character(),
    parameter = character(),
    reason = character()
  )

  detailed_summary <- data.table(
    wb_group = character(),
    parameter = character(),
    n_observations = integer(),
    n_nondetects = integer(),
    censored_pct = numeric(),
    best_distribution = character(),
    aic = numeric(),
    min_imputed = numeric(),
    max_imputed = numeric(),
    status = character()
  )

  # Process each combination
  for (i in 1:n_combinations) {
    wb_grp <- combinations[i, wb_group]
    param <- combinations[i, parameter]

    if (show_progress) {
      pb$tick(
        tokens = list(
          wb_group = substr(wb_grp, 1, 8),
          param = substr(param, 1, 10)
        )
      )
    }

    # Filter data for this specific combination
    combo_data <- dt[get(wb_group_col) == wb_grp & get(parameter_col) == param]

    if (nrow(combo_data) == 0) {
      failed_combinations <- rbind(
        failed_combinations,
        data.table(
          wb_group = wb_grp,
          parameter = param,
          reason = "No data found"
        )
      )
      next
    }

    tryCatch(
      {
        # Run imputation for this wb_group + parameter combination
        result <- impute_nondetect(
          combo_data,
          value_col = value_col,
          cens_col = cens_col,
          parameter_col = parameter_col,
          unit_col = unit_col,
          min_value = min_value,
          verbose = verbose,
          ...
        )

        # Store successful result
        combo_key <- paste(wb_grp, param, sep = "_||_")
        all_results[[combo_key]] <- result

        # Calculate summary statistics
        imputed_col <- paste0(value_col, "_imputed")
        imputed_values <- result[
          get(cens_col) == 0 & !is.na(get(imputed_col)),
          get(imputed_col)
        ]

        detailed_summary <- rbind(
          detailed_summary,
          data.table(
            wb_group = wb_grp,
            parameter = param,
            n_observations = attr(result, "sample_size"),
            n_nondetects = sum(combo_data[[cens_col]] == 0),
            censored_pct = attr(result, "censored_pct"),
            best_distribution = attr(result, "best_distribution"),
            aic = round(attr(result, "aic"), 2),
            min_imputed = if (length(imputed_values) > 0) {
              round(min(imputed_values), 6)
            } else {
              NA_real_
            },
            max_imputed = if (length(imputed_values) > 0) {
              round(max(imputed_values), 6)
            } else {
              NA_real_
            },
            status = "Success"
          )
        )
      },
      error = function(e) {
        # Handle errors gracefully
        failed_combinations <<- rbind(
          failed_combinations,
          data.table(
            wb_group = wb_grp,
            parameter = param,
            reason = substr(e$message, 1, 100)
          )
        )

        detailed_summary <<- rbind(
          detailed_summary,
          data.table(
            wb_group = wb_grp,
            parameter = param,
            n_observations = nrow(combo_data),
            n_nondetects = sum(combo_data[[cens_col]] == 0),
            censored_pct = round(
              sum(combo_data[[cens_col]] == 0) / nrow(combo_data) * 100,
              1
            ),
            best_distribution = NA_character_,
            aic = NA_real_,
            min_imputed = NA_real_,
            max_imputed = NA_real_,
            status = paste("Error:", substr(e$message, 1, 30))
          )
        )
      }
    )
  }

  # Combine successful results
  if (length(all_results) > 0) {
    final_result <- rbindlist(all_results, fill = TRUE)
  } else {
    final_result <- NULL
    warning("No combinations were successfully processed!")
  }

  # Create waterbody group summary
  wb_summary <- detailed_summary[,
    .(
      n_parameters = .N,
      n_successful = sum(status == "Success"),
      n_failed = sum(status != "Success"),
      total_observations = sum(n_observations),
      avg_censored_pct = round(mean(censored_pct, na.rm = TRUE), 1),
      most_common_dist = names(sort(
        table(best_distribution),
        decreasing = TRUE
      ))[1]
    ),
    by = wb_group
  ]

  # Print summary
  cat("\n=== NESTED BATCH IMPUTATION SUMMARY ===\n")
  cat("Total combinations processed:", n_combinations, "\n")
  cat("Successful:", length(all_results), "\n")
  cat("Failed:", nrow(failed_combinations), "\n\n")

  cat("By waterbody group:\n")
  print(wb_summary)

  if (nrow(failed_combinations) > 0) {
    cat("\nSample of failed combinations:\n")
    print(head(failed_combinations, 10))
  }

  # Check for negative values
  if (any(detailed_summary$min_imputed < 0, na.rm = TRUE)) {
    neg_combos <- detailed_summary[min_imputed < 0 & !is.na(min_imputed)]
    warning(
      "Negative imputed values found for ",
      nrow(neg_combos),
      " combinations"
    )
  }

  return(list(
    data = final_result,
    summary = detailed_summary,
    wb_group_summary = wb_summary,
    failed_combinations = failed_combinations
  ))
}


# Convenience function for typical environmental workflow
process_environmental_by_waterbody <- function(
  dt,
  wb_group_col = "wb_group",
  parameter_col = "parameter",
  value_col = "value",
  cens_col = "censored",
  unit_col = "unit",
  strictly_positive = FALSE
) {
  min_val <- if (strictly_positive) 1e-10 else 0

  nested_batch_impute(
    dt = dt,
    wb_group_col = wb_group_col,
    parameter_col = parameter_col,
    value_col = value_col,
    cens_col = cens_col,
    unit_col = unit_col,
    min_value = min_val,
    verbose = FALSE,
    show_progress = TRUE
  )
}


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