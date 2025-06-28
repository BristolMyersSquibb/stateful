#' Modern RTF Parsing Functions
#'
#' These functions work directly with RTF-parsed data from the new pipeline,
#' not HTML-converted data. They handle group pivoting, Big N extraction,
#' and statistical expansion for ARD compliance.
#'
#' @name rtf-parsing
NULL

#' Pivot data to group structure
#'
#' Transforms wide data format to long ARD format with proper group structure.
#' Detects Big N patterns and nested headers automatically.
#'
#' @param data A data frame from direct RTF parsing
#'
#' @return A data frame in ARD long format with group columns
#'
#' @keywords internal
pivot_group <- function(data) {
  # First check if column names contain BIGN patterns (traditional case)
  col_names <- names(data)[-1]  # Exclude first column
  col_names <- col_names[!is.na(col_names)]
  
  if (any(grepl("N\\s*=", col_names))) {
    # Traditional case: BIGN info is in column names
    return(
      data |>
        rename(variable = 1) |>
        tidyr::pivot_longer(
          !starts_with("variable"),
          names_to = "group1_level",
          values_to = "stat"
        ) |>
        mutate(group1 = "TRT", .before = 1) |>
        select(starts_with("group"), starts_with("variable"), starts_with("stat"))
    )
  }
  
  # Check for nested header pattern (using flexible BigN-based detection)
  nested_result <- detect_nested_pattern(data)
  if (!is.null(nested_result)) {
    return(nested_result)
  }
  
  # Look for any row that contains group headers with BIGN pattern
  header_row_idx <- NULL
  
  for (i in seq_len(nrow(data))) {
    row_values <- unlist(data[i, -1])  # Exclude first column (variable names)
    row_values <- row_values[!is.na(row_values) & row_values != ""]
    
    if (length(row_values) > 0 && any(grepl("N\\s*=", row_values))) {
      header_row_idx <- i
      break
    }
  }
  
  # If we found a header row with BIGN patterns, extract group levels
  if (!is.null(header_row_idx)) {
    # Extract group levels from the header row
    group_levels <- unlist(data[header_row_idx, -1])
    
    # Remove the group header row from data
    data_without_header <- data[-header_row_idx, ]
    
    # Pivot the data with extracted group levels
    result <- data_without_header |>
      rename(variable = 1) |>
      tidyr::pivot_longer(
        !starts_with("variable"),
        names_to = "column_name",  # Temporary name
        values_to = "stat"
      ) |>
      mutate(group1 = "TRT", .before = 1)
    
    # Map column positions to group levels
    col_positions <- match(result$column_name, names(data_without_header)[-1])
    result$group1_level <- group_levels[col_positions]
    
    # Clean up and select columns
    result <- result |>
      select(starts_with("group"), starts_with("variable"), starts_with("stat"))
      
    return(result)
  }
  
  # Fallback to original behavior if no group headers detected
  data |>
    rename(variable = 1) |>
    tidyr::pivot_longer(
      !starts_with("variable"),
      names_to = "group1_level",
      values_to = "stat"
    ) |>
    mutate(group1 = "TRT", .before = 1) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

#' Separate Big N information from group levels
#'
#' Extracts Big N values from group level labels and optionally creates
#' separate ARD rows for the N values following ARD standards.
#' Supports multiple group levels (group1, group2, group3) and handles
#' variable hierarchy with up to 5 nested levels.
#'
#' @param data A data frame in ARD format
#' @param n_pattern Pattern to match Big N values (flexible parameter)
#' @param create_bign_rows Whether to create separate rows for Big N values
#'
#' @return A data frame with Big N information extracted and processed
#'
#' @keywords internal
separate_bign <- function(data, n_pattern = "N = ", create_bign_rows = TRUE) {
  
  # Identify all group columns present in the data
  group_cols <- names(data)[grepl("^group\\d+$", names(data))]
  group_level_cols <- names(data)[grepl("^group\\d+_level$", names(data))]
  
  # Extract Big N information from group1_level (primary treatment groups)
  if ("group1_level" %in% names(data)) {
    big_n_info <- data |>
      distinct(group1_level) |>
      filter(grepl("N\\s*=", group1_level)) |>
      mutate(
        group_label = stringr::str_extract(group1_level, ".*?(?=\\(?N\\s*=)"),
        n_value = stringr::str_extract(group1_level, "N\\s*=\\s*(\\d+)") |>
          stringr::str_extract("\\d+")
      ) |>
      mutate(
        group_label = stringr::str_trim(group_label),
        n_value = as.numeric(n_value)
      ) |>
      filter(!is.na(n_value))
    
    # Always store Big N info as an attribute for metadata
    attr(data, "big_n_info") <- big_n_info
    
    # Clean the group1_level values (remove Big N part)
    data_cleaned <- data |>
      mutate(
        group1_level = stringr::str_extract(
          group1_level,
          ".*?(?=\\(?N\\s*=)"
        )
      )
  } else {
    big_n_info <- data.frame()
    data_cleaned <- data
  }
  
  # Parse variable hierarchy to support nested variable structure
  data_cleaned <- parse_variable_hierarchy(data_cleaned)
  
  # Remove rows that were only Big N headers (no actual data)
  data_cleaned <- data_cleaned |>
    filter(!is.na(group1) | !is.na(variable))

  # Create Big N rows if requested (ARD standard)
  if (create_bign_rows && nrow(big_n_info) > 0) {
    # Create ARD-compliant Big N rows
    bign_cols <- c("group1", "group1_level", "variable", "stat_name", "stat_label", "stat")
    
    bign_rows <- big_n_info |>
      mutate(
        group1 = "TRT",
        group1_level = stringr::str_trim(group_label),
        variable = "BIGN",
        stat_name = "N", 
        stat_label = "N",
        stat = as.character(n_value)  # Ensure stat is character
      ) |>
      select(all_of(bign_cols[bign_cols %in% names(.)]))
    
    # Add any additional group columns that exist in the original data
    for (col in group_level_cols) {
      if (col != "group1_level" && col %in% names(data_cleaned)) {
        bign_rows[[col]] <- NA_character_
      }
    }
    
    for (col in group_cols) {
      if (col != "group1" && col %in% names(data_cleaned)) {
        bign_rows[[col]] <- NA_character_
      }
    }
    
    # Ensure all columns are consistent types before binding
    data_cleaned <- data_cleaned |>
      mutate(stat = as.character(stat))
    
    # Add Big N rows at the beginning of the dataset
    data_with_bign <- bind_rows(bign_rows, data_cleaned)
    
    # Preserve the big_n_info attribute
    attr(data_with_bign, "big_n_info") <- big_n_info
    
    return(data_with_bign)
  }

  return(data_cleaned)
}

#' Parse variable hierarchy for nested structures
#'
#' Supports up to 5 levels of variable nesting (variable1, variable2, etc.)
#' for complex table structures with multiple classification levels.
#'
#' @param data A data frame in ARD format
#'
#' @return A data frame with parsed variable hierarchy
#'
#' @keywords internal
parse_variable_hierarchy <- function(data) {
  
  if (!"variable" %in% names(data)) {
    return(data)
  }
  
  # Check for hierarchical variables (indented with spaces, backslashes, or other indicators)
  hierarchical_vars <- data$variable[grepl("^\\s+|^\\\\\\\\|^\\t", data$variable)]
  
  if (length(hierarchical_vars) == 0) {
    return(data)
  }
  
  # Parse variable hierarchy
  data_with_hierarchy <- data |>
    mutate(
      # Detect indentation level based on leading characters
      indent_level = case_when(
        grepl("^\\s{8,}|^\\\\\\\\\\\\\\\\\\\\", variable) ~ 5,  # Level 5: 8+ spaces or 4+ backslashes
        grepl("^\\s{6,7}|^\\\\\\\\\\\\\\\\", variable) ~ 4,      # Level 4: 6-7 spaces or 3 backslashes  
        grepl("^\\s{4,5}|^\\\\\\\\\\\\", variable) ~ 3,          # Level 3: 4-5 spaces or 2 backslashes
        grepl("^\\s{2,3}|^\\\\\\\\", variable) ~ 2,              # Level 2: 2-3 spaces or 1 backslash
        grepl("^\\s+|^\\\\", variable) ~ 1,                      # Level 1: any indentation
        TRUE ~ 0                                                 # Level 0: no indentation
      ),
      # Clean variable name
      variable_clean = stringr::str_trim(stringr::str_replace_all(variable, "^\\s+|^\\\\+", ""))
    )
  
  # Create variable hierarchy columns
  current_hierarchy <- rep(NA_character_, 5)
  
  for (i in seq_len(nrow(data_with_hierarchy))) {
    level <- data_with_hierarchy$indent_level[i]
    var_name <- data_with_hierarchy$variable_clean[i]
    
    if (level == 0) {
      # Top level - reset hierarchy
      current_hierarchy <- rep(NA_character_, 5)
      current_hierarchy[1] <- var_name
    } else {
      # Nested level - update hierarchy
      current_hierarchy[level + 1] <- var_name
      # Clear deeper levels
      if (level < 4) {
        current_hierarchy[(level + 2):5] <- NA_character_
      }
    }
    
    # Assign hierarchy to current row
    data_with_hierarchy$variable1[i] <- current_hierarchy[1]
    data_with_hierarchy$variable2[i] <- current_hierarchy[2]
    data_with_hierarchy$variable3[i] <- current_hierarchy[3]
    data_with_hierarchy$variable4[i] <- current_hierarchy[4]
    data_with_hierarchy$variable5[i] <- current_hierarchy[5]
  }
  
  # Update main variable column to use the cleaned name
  data_with_hierarchy$variable <- data_with_hierarchy$variable_clean
  
  # Remove temporary columns
  data_with_hierarchy <- data_with_hierarchy |>
    select(-indent_level, -variable_clean)
  
  return(data_with_hierarchy)
}

#' Expand combined statistics into separate ARD rows
#'
#' Parses combined statistics like "137 (39.0%)" into separate rows
#' following ARD standard where each statistic gets its own row.
#'
#' @param data A data frame in ARD format with combined statistics
#' @param custom_patterns Optional custom statistical patterns
#'
#' @return A data frame with expanded statistics
#'
#' @keywords internal
expand_statistics <- function(data, custom_patterns = NULL) {
  if (!"stat" %in% names(data)) {
    return(data)
  }
  
  # Process each row and expand statistics
  expanded_rows <- purrr::map_dfr(seq_len(nrow(data)), function(i) {
    row <- data[i, ]
    stat_value <- row$stat
    
    # Skip processing if stat_name is already set (e.g., Big N rows)
    if (!is.na(row$stat_name) && !is.null(row$stat_name) && row$stat_name != "") {
      return(row)
    }
    
    # Parse the statistic using the pseudo-pattern system
    parsed_stats <- parse_stat_value_pseudo(stat_value, custom_patterns)
    
    # If parsing resulted in multiple rows, replicate the other columns
    if (nrow(parsed_stats) > 1) {
      other_cols <- row[, !names(row) %in% c("stat", "stat_name", "stat_label")]
      expanded <- purrr::map_dfr(seq_len(nrow(parsed_stats)), function(j) {
        cbind(other_cols, parsed_stats[j, ], stringsAsFactors = FALSE)
      })
      return(expanded)
    } else {
      # Single statistic, update the existing row
      row$stat_name <- parsed_stats$stat_name[1]
      row$stat_label <- parsed_stats$stat_label[1]
      row$stat <- parsed_stats$stat[1]
      return(row)
    }
  })
  
  # Bind rows together and ensure it's a data frame
  result <- do.call(rbind, expanded_rows)
  
  # Convert to data frame if it's a matrix, preserving row names as column names
  if (is.matrix(result)) {
    # Check if the matrix has row names that should be column names
    if (!is.null(rownames(result))) {
      col_names <- rownames(result)
      result <- as.data.frame(t(result), stringsAsFactors = FALSE)
      names(result) <- col_names
    } else {
      result <- as.data.frame(result, stringsAsFactors = FALSE)
    }
  }
  
  return(result)
}

#' Detect flexible nested headers in table data
#'
#' Identifies tables with nested headers using BigN patterns to determine group levels.
#' Supports up to 3 nested header levels: group1 (BigN), group2, group3.
#' No hardcoded text patterns - works with any nested structure.
#'
#' @param data A data frame from direct RTF parsing
#' @param max_header_levels Maximum number of header levels to detect (default: 3)
#'
#' @return A data frame with nested group structure or NULL if not detected
#'
#' @keywords internal
detect_nested_pattern <- function(data, max_header_levels = 3) {
  
  # Look for rows containing BigN patterns (these define group1)
  group_row_indices <- list()
  
  for (i in seq_len(min(12, nrow(data)))) {  # Check first 12 rows
    row_values <- unlist(data[i, -1])
    row_values <- row_values[!is.na(row_values) & row_values != ""]
    
    # Check if this row contains BigN patterns (these are group1 headers)
    if (length(row_values) > 0 && any(grepl("N\\s*=", row_values))) {
      group_row_indices[[1]] <- i
      
      # Check subsequent rows for additional header levels (group2, group3, etc.)
      for (level in 2:max_header_levels) {
        for (j in (i + level - 1):min(i + level + 2, nrow(data))) {  # Check next few rows
          if (j > nrow(data)) break
          
          next_row_values <- unlist(data[j, -1])
          next_row_values <- next_row_values[!is.na(next_row_values) & next_row_values != ""]
          
          # If we find another row with non-empty values that doesn't look like data,
          # and has similar number of columns, it's likely another header level
          if (length(next_row_values) > 0 && 
              length(next_row_values) >= length(row_values) / 3 &&  # More flexible threshold
              !any(grepl("^\\d+\\s*\\(", next_row_values)) &&  # Not statistics
              !(j %in% unlist(group_row_indices))) {  # Not already assigned
            group_row_indices[[level]] <- j
            break
          }
        }
      }
      break
    }
  }
  
  # Need at least 2 header levels for nested structure
  if (length(group_row_indices) < 2) {
    return(NULL)
  }
  
  # Extract group information from all header rows
  group_values <- list()
  for (level in seq_along(group_row_indices)) {
    row_idx <- group_row_indices[[level]]
    group_values[[level]] <- unlist(data[row_idx, -1])
  }
  
  # Remove all header rows from data
  header_rows <- unlist(group_row_indices)
  data_clean <- data[-header_rows, ]
  
  # Create mapping from column positions
  result_data <- data_clean |>
    rename(variable = 1) |>
    tidyr::pivot_longer(
      cols = -variable,
      names_to = "column_name",
      values_to = "stat"
    ) |>
    mutate(group1 = "TRT", .before = 1)
  
  # Map column positions to all group levels
  col_positions <- match(result_data$column_name, names(data_clean)[-1])
  
  # Create flexible group levels for all detected levels
  for (level in seq_along(group_values)) {
    level_col_name <- paste0("group", level, "_level")
    result_data[[level_col_name]] <- group_values[[level]][col_positions]
    
    # Add group name column for levels > 1
    if (level > 1) {
      group_col_name <- paste0("group", level)
      result_data[[group_col_name]] <- paste0("GROUP", level)
    }
  }
  
  # Build dynamic filter for non-empty values
  group_level_cols <- paste0("group", seq_along(group_values), "_level")
  filter_conditions <- paste0("!is.na(", group_level_cols, ") & ", group_level_cols, " != \"\"")
  combined_filter <- paste(filter_conditions, collapse = " & ")
  
  # Clean up group levels by removing NA values and empty strings
  result_data <- result_data |>
    filter(eval(parse(text = combined_filter)))
  
  # Select relevant columns dynamically
  group_cols <- c("group1", paste0("group", 2:length(group_values)))
  group_level_cols <- paste0("group", seq_along(group_values), "_level")
  select_cols <- c(group_cols, group_level_cols, "variable", "stat")
  
  result_data <- result_data |>
    select(all_of(select_cols[select_cols %in% names(result_data)]))
  
  return(result_data)
}

