#' Post-process ARD data to ensure completeness
#'
#' This function ensures that ARD data has complete statistics, especially
#' for count/percentage pairs where one might be missing. Uses global patterns
#' for flexible detection of percentage indicators and standard variable names.
#'
#' @param ard_data A data frame in ARD format
#' @param total_n Named vector of total N values by group
#'
#' @return A data frame with complete ARD statistics
#'
#' @export
post_process_ard <- function(ard_data, total_n = NULL) {

  # Ensure ard_data is a data frame
  if (is.matrix(ard_data)) {
    ard_data <- as.data.frame(ard_data, stringsAsFactors = FALSE)
  }

  # First, get unique identifier columns for matching
  # Include variable_label1 and context if present to distinguish sections
  id_cols <- c("group1", "group1_level", "variable")
  if ("variable_label1" %in% names(ard_data)) {
    id_cols <- c(id_cols, "variable_label1")
  }
  if ("context" %in% names(ard_data)) {
    id_cols <- c(id_cols, "context")
  }

  # Get global patterns for flexible detection
  percentage_patterns <- get_percentage_patterns()
  stat_names <- get_stat_names()

  # Create combined percentage pattern for detection
  percentage_regex <- paste(percentage_patterns, collapse = "|")

  # First, identify rows that need percentage pairs
  # Build the filter condition based on available columns
  has_variable_label1 <- "variable_label1" %in% names(ard_data)

  if (has_variable_label1) {
    needs_percentage <- ard_data |>
      filter(
        stat_name == stat_names$count &
        (grepl(percentage_regex, variable) |
         (!is.na(variable_label1) & grepl(percentage_regex, variable_label1)))
      ) |>
      select(all_of(id_cols))
  } else {
    needs_percentage <- ard_data |>
      filter(
        stat_name == stat_names$count &
        grepl(percentage_regex, variable)
      ) |>
      select(all_of(id_cols))
  }

  # Check which ones are missing percentage values
  # Use all identifier columns for proper matching
  missing_percentages <- needs_percentage |>
    anti_join(
      ard_data |> filter(stat_name == stat_names$percentage),
      by = id_cols
    )

  # Create percentage rows for missing values
  if (nrow(missing_percentages) > 0 && !is.null(total_n)) {
    new_percentage_rows <- missing_percentages |>
      left_join(
        ard_data |> filter(stat_name == stat_names$count),
        by = id_cols
      ) |>
      mutate(
        # Calculate percentage if we have totals
        n_value = as.numeric(stat),
        total = total_n[group1_level],
        calc_percent = if_else(
          !is.na(n_value) & !is.na(total) & total > 0,
          round(100 * n_value / total, 1),
          if_else(n_value == 0, 0, NA_real_)
        ),
        stat_name = stat_names$percentage,
        stat_label = "%",
        stat = as.character(calc_percent)
      ) |>
      select(-n_value, -total, -calc_percent)

    # Add the new percentage rows
    ard_data <- bind_rows(ard_data, new_percentage_rows)
  }

  # Also check for orphaned percentage values that need count pairs
  if (has_variable_label1) {
    needs_count <- ard_data |>
      filter(
        stat_name == stat_names$percentage &
        (grepl(percentage_regex, variable) |
         (!is.na(variable_label1) & grepl(percentage_regex, variable_label1)))
      ) |>
      select(all_of(id_cols))
  } else {
    needs_count <- ard_data |>
      filter(
        stat_name == stat_names$percentage &
        grepl(percentage_regex, variable)
      ) |>
      select(all_of(id_cols))
  }

  missing_counts <- needs_count |>
    anti_join(
      ard_data |> filter(stat_name == stat_names$count),
      by = id_cols
    )

  # For missing counts, we can't calculate them from percentages alone
  # but we should flag this as a data quality issue
  if (nrow(missing_counts) > 0) {
    warning("Found ", nrow(missing_counts), " percentage values without corresponding counts")
  }

  # Sort the data for better readability
  ard_data |>
    arrange(group1, group1_level, variable, stat_name)
}

#' Extract total N values from ARD data
#'
#' @param ard_data A data frame in ARD format
#'
#' @return A named vector of total N values by group
#'
#' @export
extract_total_n <- function(ard_data) {
  # Ensure ard_data is a data frame
  if (is.matrix(ard_data)) {
    ard_data <- as.data.frame(ard_data, stringsAsFactors = FALSE)
  }

  # Use global patterns for flexible variable and stat name detection
  variable_names <- get_variable_names()
  stat_names <- get_stat_names()

  bign_rows <- ard_data |>
    filter(variable == variable_names$bign, stat_name == stat_names$total)

  if (nrow(bign_rows) > 0) {
    totals <- setNames(
      as.numeric(bign_rows$stat),
      bign_rows$group1_level
    )
    return(totals)
  }

  return(NULL)
}