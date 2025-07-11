#' RTF Header Structure Analysis Utilities
#'
#' Utilities for analyzing multi-level header structures in RTF tables
#' and building column group mappings
#'
#' @name rtf-header-utils
NULL

#' Build header structure from header rows
#'
#' Process multi-level headers to create column mapping structure.
#' Identifies spanning cells vs actual group levels and builds hierarchical
#' column mappings for colgroup1, colgroup2, colgroup3.
#'
#' @param header_rows List of header row data, where each row contains cell information
#' @return List with column mappings and group structures
#' @export
build_header_structure <- function(header_rows) {
  if (length(header_rows) == 0) {
    return(list(
      levels = 0,
      column_mappings = list(),
      colgroup1_map = list(),
      colgroup2_map = list(),
      colgroup3_map = list()
    ))
  }
  
  # Initialize structure
  structure <- list(
    levels = length(header_rows),
    column_mappings = list(),
    colgroup1_map = list(),
    colgroup2_map = list(),
    colgroup3_map = list()
  )
  
  # Process each header level
  for (level in seq_along(header_rows)) {
    header_data <- header_rows[[level]]
    
    if (level == 1) {
      # First level = colgroup1 (treatment arms)
      structure$colgroup1_map <- build_level_mapping(header_data, level)
      structure$column_mappings[[level]] <- structure$colgroup1_map
      
    } else if (level == 2) {
      # Second level = colgroup2 (sub-categories)
      structure$colgroup2_map <- build_level_mapping(header_data, level)
      structure$column_mappings[[level]] <- structure$colgroup2_map
      
      # Map colgroup2 to parent colgroup1
      structure$colgroup2_map <- map_to_parent_level(
        structure$colgroup2_map, 
        structure$colgroup1_map
      )
      
    } else if (level == 3) {
      # Third level = colgroup3 (further sub-categories)
      structure$colgroup3_map <- build_level_mapping(header_data, level)
      structure$column_mappings[[level]] <- structure$colgroup3_map
      
      # Map colgroup3 to parent colgroup2
      structure$colgroup3_map <- map_to_parent_level(
        structure$colgroup3_map, 
        structure$colgroup2_map
      )
    }
  }
  
  structure
}

#' Build mapping for a single header level
#'
#' Process cells in a header row to create position-based mappings
#'
#' @param header_data List of cell objects from a header row
#' @param level Integer indicating header level (1, 2, 3)
#' @return List with position mappings for this level
#' @keywords internal
build_level_mapping <- function(header_data, level) {
  if (length(header_data) == 0) {
    return(list())
  }
  
  mapping <- list()
  
  for (i in seq_along(header_data)) {
    cell <- header_data[[i]]
    
    # Skip empty cells (separators)
    cell_text <- trimws(cell$text)
    if (cell_text == "" || cell_text == "~~~" || cell_text == "\\\\\\") {
      next
    }
    
    # Create mapping entry
    entry <- list(
      text = cell_text,
      start_pos = cell$start_pos,
      end_pos = cell$end_pos,
      col_index = cell$col_index,
      level = level,
      width = cell$end_pos - cell$start_pos
    )
    
    mapping[[length(mapping) + 1]] <- entry
  }
  
  mapping
}

#' Map child level to parent level
#'
#' For multi-level headers, determine which parent cell each child cell belongs to
#'
#' @param child_map List of child level mappings
#' @param parent_map List of parent level mappings
#' @return Enhanced child_map with parent references
#' @keywords internal
map_to_parent_level <- function(child_map, parent_map) {
  if (length(child_map) == 0 || length(parent_map) == 0) {
    return(child_map)
  }
  
  for (i in seq_along(child_map)) {
    child <- child_map[[i]]
    
    # Find which parent cell this child belongs to
    parent_idx <- find_parent_cell(child, parent_map)
    
    if (!is.null(parent_idx)) {
      child_map[[i]]$parent_index <- parent_idx
      child_map[[i]]$parent_text <- parent_map[[parent_idx]]$text
    } else {
      child_map[[i]]$parent_index <- NA
      child_map[[i]]$parent_text <- ""
    }
  }
  
  child_map
}

#' Find parent cell for a child cell
#'
#' Determine which parent cell contains or spans over a child cell
#'
#' @param child_cell Child cell object
#' @param parent_map List of parent cell mappings
#' @return Index of parent cell or NULL if not found
#' @keywords internal
find_parent_cell <- function(child_cell, parent_map) {
  child_center <- (child_cell$start_pos + child_cell$end_pos) / 2
  
  for (i in seq_along(parent_map)) {
    parent <- parent_map[[i]]
    
    # Check if child center falls within parent span
    if (child_center >= parent$start_pos && child_center <= parent$end_pos) {
      return(i)
    }
  }
  
  # If no exact match, find closest parent
  min_distance <- Inf
  closest_parent <- NULL
  
  for (i in seq_along(parent_map)) {
    parent <- parent_map[[i]]
    parent_center <- (parent$start_pos + parent$end_pos) / 2
    distance <- abs(child_center - parent_center)
    
    if (distance < min_distance) {
      min_distance <- distance
      closest_parent <- i
    }
  }
  
  closest_parent
}

#' Map data cell to column groups
#'
#' For a data cell at a specific position, determine its colgroup assignments
#'
#' @param cell_position Numeric position of the data cell
#' @param header_structure Header structure object from build_header_structure
#' @return List with colgroup1, colgroup2, colgroup3 assignments
#' @export
map_cell_to_colgroups <- function(cell_position, header_structure) {
  result <- list(
    colgroup1 = "",
    colgroup1_level = "",
    colgroup2 = "",
    colgroup2_level = "",
    colgroup3 = "",
    colgroup3_level = ""
  )
  
  # Find colgroup1 (treatment arm)
  if (length(header_structure$colgroup1_map) > 0) {
    cg1_match <- find_containing_cell(cell_position, header_structure$colgroup1_map)
    if (!is.null(cg1_match)) {
      result$colgroup1 <- "colgroup1"
      result$colgroup1_level <- cg1_match$text
    }
  }
  
  # Find colgroup2 (sub-category)
  if (length(header_structure$colgroup2_map) > 0) {
    cg2_match <- find_containing_cell(cell_position, header_structure$colgroup2_map)
    if (!is.null(cg2_match)) {
      result$colgroup2 <- "colgroup2"
      result$colgroup2_level <- cg2_match$text
      
      # If colgroup1 wasn't found but we have parent info, use it
      if (result$colgroup1 == "" && !is.na(cg2_match$parent_text)) {
        result$colgroup1 <- "colgroup1"
        result$colgroup1_level <- cg2_match$parent_text
      }
    }
  }
  
  # Find colgroup3 (further sub-category)
  if (length(header_structure$colgroup3_map) > 0) {
    cg3_match <- find_containing_cell(cell_position, header_structure$colgroup3_map)
    if (!is.null(cg3_match)) {
      result$colgroup3 <- "colgroup3"
      result$colgroup3_level <- cg3_match$text
      
      # Propagate parent information if needed
      if (result$colgroup2 == "" && !is.na(cg3_match$parent_text)) {
        result$colgroup2 <- "colgroup2"
        result$colgroup2_level <- cg3_match$parent_text
      }
    }
  }
  
  result
}

#' Find containing cell for a position
#'
#' Find which cell in a mapping contains a given position
#'
#' @param position Numeric position to search for
#' @param cell_mapping List of cell mappings to search in
#' @return Cell object if found, NULL otherwise
#' @keywords internal
find_containing_cell <- function(position, cell_mapping) {
  for (cell in cell_mapping) {
    if (position >= cell$start_pos && position <= cell$end_pos) {
      return(cell)
    }
  }
  
  # If no exact match, find closest
  min_distance <- Inf
  closest_cell <- NULL
  
  for (cell in cell_mapping) {
    cell_center <- (cell$start_pos + cell$end_pos) / 2
    distance <- abs(position - cell_center)
    
    if (distance < min_distance) {
      min_distance <- distance
      closest_cell <- cell
    }
  }
  
  closest_cell
}

#' Detect spanning headers
#'
#' Identify which headers span multiple columns (are parent headers)
#'
#' @param header_structure Header structure object
#' @return List indicating which headers are spanning
#' @export
detect_spanning_headers <- function(header_structure) {
  spanning <- list()
  
  # Check colgroup1 spanning over colgroup2
  if (length(header_structure$colgroup1_map) > 0 && length(header_structure$colgroup2_map) > 0) {
    spanning$colgroup1_spans <- list()
    
    for (i in seq_along(header_structure$colgroup1_map)) {
      cg1_cell <- header_structure$colgroup1_map[[i]]
      
      # Count how many colgroup2 cells fall within this colgroup1
      children <- 0
      for (cg2_cell in header_structure$colgroup2_map) {
        if (cg2_cell$start_pos >= cg1_cell$start_pos && cg2_cell$end_pos <= cg1_cell$end_pos) {
          children <- children + 1
        }
      }
      
      spanning$colgroup1_spans[[i]] <- list(
        text = cg1_cell$text,
        children_count = children,
        is_spanning = children > 1
      )
    }
  }
  
  # Similar check for colgroup2 spanning over colgroup3
  if (length(header_structure$colgroup2_map) > 0 && length(header_structure$colgroup3_map) > 0) {
    spanning$colgroup2_spans <- list()
    
    for (i in seq_along(header_structure$colgroup2_map)) {
      cg2_cell <- header_structure$colgroup2_map[[i]]
      
      children <- 0
      for (cg3_cell in header_structure$colgroup3_map) {
        if (cg3_cell$start_pos >= cg2_cell$start_pos && cg3_cell$end_pos <= cg2_cell$end_pos) {
          children <- children + 1
        }
      }
      
      spanning$colgroup2_spans[[i]] <- list(
        text = cg2_cell$text,
        children_count = children,
        is_spanning = children > 1
      )
    }
  }
  
  spanning
}