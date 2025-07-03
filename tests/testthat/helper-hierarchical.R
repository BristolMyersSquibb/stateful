# Helper functions for testing hierarchical structure

#' Compare two ARD data structures for hierarchical consistency
#' 
#' @param actual Current ARD data.frame
#' @param expected Expected ARD data.frame
#' @param tolerance Numeric tolerance for stat values
#' @return List of comparison results
compare_ard_structures <- function(actual, expected, tolerance = 1e-6) {
  result <- list(
    columns_match = identical(sort(names(actual)), sort(names(expected))),
    row_count_match = nrow(actual) == nrow(expected),
    group_levels_match = TRUE,
    variables_match = TRUE,
    stats_match = TRUE,
    issues = character()
  )
  
  if (!result$columns_match) {
    missing_cols <- setdiff(names(expected), names(actual))
    extra_cols <- setdiff(names(actual), names(expected))
    if (length(missing_cols) > 0) {
      result$issues <- c(result$issues, paste("Missing columns:", paste(missing_cols, collapse = ", ")))
    }
    if (length(extra_cols) > 0) {
      result$issues <- c(result$issues, paste("Extra columns:", paste(extra_cols, collapse = ", ")))
    }
  }
  
  if (result$columns_match && result$row_count_match) {
    # Check group levels
    if ("group1_level" %in% names(actual)) {
      actual_groups <- sort(unique(actual$group1_level))
      expected_groups <- sort(unique(expected$group1_level))
      if (!identical(actual_groups, expected_groups)) {
        result$group_levels_match <- FALSE
        result$issues <- c(result$issues, "group1_level values differ")
      }
    }
    
    # Check variables
    actual_vars <- sort(unique(actual$variable))
    expected_vars <- sort(unique(expected$variable))
    if (!identical(actual_vars, expected_vars)) {
      result$variables_match <- FALSE
      missing_vars <- setdiff(expected_vars, actual_vars)
      extra_vars <- setdiff(actual_vars, expected_vars)
      if (length(missing_vars) > 0) {
        result$issues <- c(result$issues, paste("Missing variables:", paste(missing_vars, collapse = ", ")))
      }
      if (length(extra_vars) > 0) {
        result$issues <- c(result$issues, paste("Extra variables:", paste(extra_vars, collapse = ", ")))
      }
    }
  }
  
  result
}

#' Validate ARD structure compliance
#'
#' @param ard_data ARD data.frame to validate
#' @return List with validation results
validate_ard_compliance <- function(ard_data) {
  required_cols <- c("group1", "group1_level", "variable", "stat", "stat_name", "stat_label")
  optional_cols <- c("group2", "group2_level", "variable_level")
  
  result <- list(
    is_valid = TRUE,
    has_required_columns = all(required_cols %in% names(ard_data)),
    has_data = nrow(ard_data) > 0,
    stat_values_valid = TRUE,
    issues = character()
  )
  
  # Check required columns
  missing_required <- setdiff(required_cols, names(ard_data))
  if (length(missing_required) > 0) {
    result$has_required_columns <- FALSE
    result$is_valid <- FALSE
    result$issues <- c(result$issues, paste("Missing required columns:", paste(missing_required, collapse = ", ")))
  }
  
  if (result$has_data && result$has_required_columns) {
    # Check stat values can be converted to numeric where appropriate
    stat_numeric <- suppressWarnings(as.numeric(ard_data$stat))
    non_numeric_stats <- ard_data$stat[is.na(stat_numeric) & !is.na(ard_data$stat)]
    
    # Allow some non-numeric stats (like combined values before parsing)
    if (length(non_numeric_stats) > nrow(ard_data) * 0.5) {
      result$stat_values_valid <- FALSE
      result$issues <- c(result$issues, "Too many non-numeric stat values")
    }
  }
  
  result
}

#' Extract hierarchical relationships from ARD data
#'
#' @param ard_data ARD data.frame
#' @return List of parent-child relationships
extract_hierarchy_from_ard <- function(ard_data) {
  hierarchy <- list()
  
  # Look for variables with \\ prefix (current format)
  hierarchical_vars <- unique(ard_data$variable[grepl("^\\\\\\\\", ard_data$variable)])
  
  # Look for variable_level field (improved format)
  if ("variable_level" %in% names(ard_data)) {
    variable_levels <- unique(ard_data$variable_level[!is.na(ard_data$variable_level) & ard_data$variable_level != ""])
    hierarchy$variable_levels <- variable_levels
    
    # Extract parent-child relationships from variable_level
    for (level in variable_levels) {
      if (grepl(" - ", level)) {
        parts <- strsplit(level, " - ", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          parent <- parts[1]
          child <- paste(parts[-1], collapse = " - ")
          if (!parent %in% names(hierarchy$relationships)) {
            hierarchy$relationships[[parent]] <- character()
          }
          hierarchy$relationships[[parent]] <- unique(c(hierarchy$relationships[[parent]], child))
        }
      }
    }
  }
  
  hierarchy$hierarchical_vars <- hierarchical_vars
  hierarchy$total_hierarchical <- length(hierarchical_vars)
  
  hierarchy
}

#' Check if hierarchical structure is properly formatted
#'
#' @param ard_data ARD data.frame
#' @return Logical indicating if hierarchy is well-formed
is_hierarchy_well_formed <- function(ard_data) {
  hierarchy <- extract_hierarchy_from_ard(ard_data)
  
  # If we have variable_level field, check it's properly formatted
  if ("variable_level" %in% names(ard_data) && length(hierarchy$variable_levels) > 0) {
    # All hierarchical entries should have proper variable_level
    hierarchical_rows <- ard_data[grepl("^\\\\\\\\", ard_data$variable) | 
                                 (!is.na(ard_data$variable_level) & ard_data$variable_level != ""), ]
    
    if (nrow(hierarchical_rows) > 0) {
      # Check that variable names are clean (no \\ prefix) when variable_level exists
      clean_vars <- !grepl("^\\\\\\\\", hierarchical_rows$variable)
      proper_levels <- !is.na(hierarchical_rows$variable_level) & hierarchical_rows$variable_level != ""
      
      return(all(clean_vars) && all(proper_levels))
    }
  }
  
  # For current format, just check we have some hierarchical structure
  hierarchy$total_hierarchical > 0
}

#' Create a test RTF with controlled hierarchical structure
#'
#' @param parent_label Parent category label
#' @param child_labels Vector of child category labels  
#' @param group_headers Vector of group headers
#' @param values Matrix of values (rows = variables, cols = groups)
#' @return Character string with RTF content
create_test_rtf_with_hierarchy <- function(parent_label, child_labels, group_headers, values) {
  # This is a simplified RTF generator for testing
  # In practice, you'd want a more complete RTF structure
  
  rtf_lines <- c(
    "{\\rtf1\\ansi\\ansicpg1252",
    "\\trowd\\trkeep\\trhdr\\trqc\\trgaph0",
    paste0("\\pard\\plain\\intbl\\qc\\f1{", parent_label, "\\cell}"),
    "\\row"
  )
  
  # Add child rows with indentation
  for (i in seq_along(child_labels)) {
    child_line <- paste0("\\pard\\plain\\intbl\\ql\\f1{\\~\\~", child_labels[i], "\\cell}")
    rtf_lines <- c(rtf_lines, child_line)
    
    # Add values for this child
    for (j in seq_along(group_headers)) {
      if (i <= nrow(values) && j <= ncol(values)) {
        value_line <- paste0("\\pard\\plain\\intbl\\qc\\f1{", values[i, j], "\\cell}")
        rtf_lines <- c(rtf_lines, value_line)
      }
    }
    rtf_lines <- c(rtf_lines, "\\row")
  }
  
  rtf_lines <- c(rtf_lines, "}")
  paste(rtf_lines, collapse = "\n")
}