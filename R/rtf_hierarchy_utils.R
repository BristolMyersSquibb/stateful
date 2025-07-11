#' RTF Row Hierarchy Detection Utilities
#'
#' Utilities for detecting and processing hierarchical row structures in RTF tables.
#' Handles both label-type and group-type hierarchies based on presence of statistics.
#'
#' @name rtf-hierarchy-utils
NULL

#' Detect and process row hierarchies in table data
#'
#' Analyzes table data to identify hierarchical row structures and processes them
#' according to their type (label or group). 
#'
#' @param table_data Data frame with columns: variable, raw_stat, source_row, source_col, etc.
#' @return Enhanced data frame with hierarchy information added
#' @export
detect_row_hierarchy <- function(table_data) {
  if (nrow(table_data) == 0) {
    return(table_data)
  }
  
  # Add hierarchy detection columns
  table_data$hierarchy_level <- 0
  table_data$hierarchy_type <- "none"
  table_data$hierarchy_parent <- ""
  table_data$variable_level <- table_data$variable
  table_data$rowgroup1 <- ""
  table_data$rowgroup1_level <- ""
  table_data$rowgroup2 <- ""
  table_data$rowgroup2_level <- ""
  
  # Group by source_row to analyze each row
  rows_by_source <- split(table_data, table_data$source_row)
  
  # Identify indentation patterns
  hierarchy_info <- identify_indentation_patterns(rows_by_source)
  
  # Process hierarchies
  processed_data <- process_hierarchical_rows(table_data, hierarchy_info)
  
  processed_data
}

#' Identify indentation patterns in table rows
#'
#' Analyze variable names to detect indentation markers and hierarchy levels
#'
#' @param rows_by_source List of data frames grouped by source_row
#' @return List with hierarchy information for each row
#' @keywords internal
identify_indentation_patterns <- function(rows_by_source) {
  hierarchy_info <- list()
  
  for (row_key in names(rows_by_source)) {
    row_data <- rows_by_source[[row_key]]
    
    # Get the variable name (from first cell)
    variable_name <- row_data$variable[1]
    
    # Detect indentation level based on leading tildes
    indent_level <- detect_indentation_level(variable_name)
    
    # Classify hierarchy type
    hierarchy_type <- classify_hierarchy_type(row_data)
    
    # Clean variable name
    clean_variable <- clean_variable_name(variable_name)
    
    hierarchy_info[[row_key]] <- list(
      source_row = as.numeric(row_key),
      variable_name = variable_name,
      clean_variable = clean_variable,
      indent_level = indent_level,
      hierarchy_type = hierarchy_type,
      has_stats = hierarchy_type == "group",
      is_indented = indent_level > 0
    )
  }
  
  hierarchy_info
}

#' Detect indentation level from variable name
#'
#' Count leading tilde characters to determine hierarchy level
#'
#' @param variable_name Character string with potential indentation markers
#' @return Integer indicating indentation level (0 = no indent, 1+ = indented)
#' @keywords internal
detect_indentation_level <- function(variable_name) {
  if (is.null(variable_name) || nchar(variable_name) == 0) {
    return(0)
  }
  
  # Count leading tildes (~)
  tilde_match <- regexpr("^(~+)", variable_name)
  
  if (tilde_match > 0) {
    tilde_count <- attr(tilde_match, "match.length")
    # Each pair of tildes (~) represents one indentation level
    return(floor(tilde_count / 2))
  }
  
  return(0)
}

#' Classify hierarchy type based on presence of statistics
#'
#' Determine if this is a label-type (no stats) or group-type (with stats) hierarchy
#'
#' @param row_data Data frame for a single row
#' @return Character "label", "group", or "none"
#' @keywords internal
classify_hierarchy_type <- function(row_data) {
  # Check if row has any non-empty stat columns (excluding first column which is variable)
  stat_data <- row_data[!is.na(row_data$source_col) & row_data$source_col > 1, ]
  
  if (nrow(stat_data) == 0) {
    return("none")
  }
  
  # Check for meaningful statistics (not just separators or empty)
  has_meaningful_stats <- any(
    trimws(stat_data$raw_stat) != "" & 
    stat_data$raw_stat != "\\\\\\\\\\\\~" & 
    stat_data$raw_stat != "~~~" &
    !is.na(stat_data$raw_stat)
  )
  
  if (has_meaningful_stats) {
    return("group")
  } else {
    return("label")
  }
}

#' Clean variable name by removing indentation markers
#'
#' Remove leading tildes and other RTF formatting
#'
#' @param variable_name Character string with potential formatting
#' @return Clean variable name
#' @keywords internal
clean_variable_name <- function(variable_name) {
  if (is.null(variable_name) || nchar(variable_name) == 0) {
    return("")
  }
  
  # Remove leading tildes
  clean_name <- gsub("^~+", "", variable_name)
  
  # Remove other RTF artifacts
  clean_name <- gsub("\\\\\\\\", "", clean_name)
  
  # Trim whitespace
  trimws(clean_name)
}

#' Process hierarchical rows based on their type
#'
#' Apply different processing logic for label-type vs group-type hierarchies
#'
#' @param table_data Original table data
#' @param hierarchy_info Hierarchy information from identify_indentation_patterns
#' @return Enhanced table data with hierarchy processing applied
#' @keywords internal
process_hierarchical_rows <- function(table_data, hierarchy_info) {
  # Sort hierarchy info by source_row for sequential processing
  sorted_info <- hierarchy_info[order(sapply(hierarchy_info, function(x) x$source_row))]
  
  # Track current parent for each level
  current_parents <- list()
  current_group_parents <- list()
  
  # Process each row in order
  for (info in sorted_info) {
    row_indices <- which(table_data$source_row == info$source_row)
    
    if (length(row_indices) == 0) next
    
    # Update hierarchy tracking
    table_data[row_indices, "hierarchy_level"] <- info$indent_level
    table_data[row_indices, "hierarchy_type"] <- info$hierarchy_type
    
    if (info$indent_level == 0) {
      # Top-level row - potential parent
      if (info$hierarchy_type == "label") {
        current_parents[[1]] <- info$clean_variable
      } else if (info$hierarchy_type == "group") {
        current_group_parents[[1]] <- info$clean_variable
        # Set as rowgroup1
        table_data[row_indices, "rowgroup1"] <- "rowgroup1"
        table_data[row_indices, "rowgroup1_level"] <- info$clean_variable
      }
      
      # Clean variable name
      table_data[row_indices, "variable"] <- info$clean_variable
      table_data[row_indices, "variable_level"] <- info$clean_variable
      
    } else {
      # Indented row - child
      if (info$hierarchy_type == "label") {
        # Label-type: append parent to variable_level
        parent_label <- current_parents[[info$indent_level]] %||% current_parents[[1]] %||% ""
        
        if (parent_label != "") {
          variable_level <- paste(parent_label, info$clean_variable, sep = " - ")
        } else {
          variable_level <- info$clean_variable
        }
        
        table_data[row_indices, "variable"] <- info$clean_variable
        table_data[row_indices, "variable_level"] <- variable_level
        table_data[row_indices, "hierarchy_parent"] <- parent_label
        
      } else if (info$hierarchy_type == "group") {
        # Group-type: create rowgroup hierarchy
        parent_group <- current_group_parents[[info$indent_level]] %||% current_group_parents[[1]] %||% ""
        
        if (info$indent_level == 1) {
          # Child of rowgroup1
          table_data[row_indices, "rowgroup1"] <- "rowgroup1"
          table_data[row_indices, "rowgroup1_level"] <- parent_group
          table_data[row_indices, "rowgroup2"] <- "rowgroup2"
          table_data[row_indices, "rowgroup2_level"] <- info$clean_variable
          
        } else if (info$indent_level == 2) {
          # Child of rowgroup2 (grandchild)
          parent_level1 <- current_group_parents[[1]] %||% ""
          parent_level2 <- current_group_parents[[2]] %||% ""
          
          table_data[row_indices, "rowgroup1"] <- "rowgroup1"
          table_data[row_indices, "rowgroup1_level"] <- parent_level1
          table_data[row_indices, "rowgroup2"] <- "rowgroup2"
          table_data[row_indices, "rowgroup2_level"] <- parent_level2
        }
        
        table_data[row_indices, "variable"] <- info$clean_variable
        table_data[row_indices, "variable_level"] <- info$clean_variable
        table_data[row_indices, "hierarchy_parent"] <- parent_group
        
        # Update group parent tracking
        current_group_parents[[info$indent_level + 1]] <- info$clean_variable
      }
    }
  }
  
  table_data
}

#' Null coalescing operator
#'
#' Return first non-null value
#'
#' @param x First value
#' @param y Second value  
#' @return First non-null value
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    y
  } else {
    x
  }
}

#' Validate hierarchy structure
#'
#' Check that hierarchy structure is well-formed and consistent
#'
#' @param table_data Data frame with hierarchy information
#' @return List with validation results and any issues found
#' @export
validate_hierarchy_structure <- function(table_data) {
  issues <- list()
  
  # Check for orphaned children (indented rows without parents)
  indented_rows <- table_data[table_data$hierarchy_level > 0, ]
  
  if (nrow(indented_rows) > 0) {
    for (i in seq_len(nrow(indented_rows))) {
      row <- indented_rows[i, ]
      
      if (row$hierarchy_parent == "" && row$hierarchy_type == "label") {
        issues <- append(issues, paste("Orphaned label-type child:", row$variable))
      }
    }
  }
  
  # Check for consistent group assignments
  group_rows <- table_data[table_data$hierarchy_type == "group", ]
  
  if (nrow(group_rows) > 0) {
    # Validate rowgroup assignments are consistent
    for (level in c("rowgroup1", "rowgroup2")) {
      level_col <- paste0(level, "_level")
      
      if (level_col %in% names(group_rows)) {
        inconsistent <- group_rows[
          group_rows[[level]] != "" & 
          group_rows[[level_col]] == "", 
        ]
        
        if (nrow(inconsistent) > 0) {
          issues <- append(issues, paste("Inconsistent", level, "assignments found"))
        }
      }
    }
  }
  
  list(
    is_valid = length(issues) == 0,
    issues = issues,
    hierarchy_summary = summarize_hierarchy(table_data)
  )
}

#' Summarize hierarchy structure
#'
#' Create summary statistics about the hierarchy structure
#'
#' @param table_data Data frame with hierarchy information
#' @return List with summary statistics
#' @keywords internal
summarize_hierarchy <- function(table_data) {
  list(
    total_rows = nrow(table_data),
    hierarchical_rows = sum(table_data$hierarchy_level > 0),
    label_type_rows = sum(table_data$hierarchy_type == "label"),
    group_type_rows = sum(table_data$hierarchy_type == "group"),
    max_hierarchy_level = max(table_data$hierarchy_level, na.rm = TRUE),
    rowgroup1_levels = length(unique(table_data$rowgroup1_level[table_data$rowgroup1_level != ""])),
    rowgroup2_levels = length(unique(table_data$rowgroup2_level[table_data$rowgroup2_level != ""]))
  )
}