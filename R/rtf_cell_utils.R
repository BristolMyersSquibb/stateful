#' RTF Cell Position and Extraction Utilities
#'
#' Enhanced utilities for extracting cell content and positions from RTF table rows
#'
#' @name rtf-cell-utils
NULL

#' Extract cells from RTF table row with position information
#'
#' Enhanced version of cell extraction that captures cell positions from \\cellxN markers
#' and calculates cell spans for accurate column mapping.
#'
#' @param row_content Character string containing complete RTF row content
#' @return List of cell objects with text, start_pos, end_pos, and col_index
#' @export
extract_cells_from_row_enhanced <- function(row_content) {
  # Extract cell positions from \cellxN markers
  cellx_positions <- extract_cellx_positions(row_content)
  
  # Extract cell content using \cell} markers
  cell_contents <- extract_cell_contents(row_content)
  
  # Combine positions and content
  cells <- list()
  
  if (length(cell_contents) > 0) {
    # If no cellx positions found, use sequential positions
    if (length(cellx_positions) == 0) {
      for (i in seq_along(cell_contents)) {
        cells[[i]] <- list(
          text = cell_contents[i],
          start_pos = (i - 1) * 1000,  # Use arbitrary spacing
          end_pos = i * 1000,
          col_index = i,
          width = 1000
        )
      }
    } else {
      for (i in seq_along(cell_contents)) {
        start_pos <- if (i == 1) 0 else cellx_positions[i - 1]
        end_pos <- if (i <= length(cellx_positions)) cellx_positions[i] else max(cellx_positions, na.rm = TRUE)
        
        cells[[i]] <- list(
          text = cell_contents[i],
          start_pos = start_pos,
          end_pos = end_pos,
          col_index = i,
          width = end_pos - start_pos
        )
      }
    }
  }
  
  cells
}

#' Extract cell positions from RTF row content
#'
#' Parse \\cellxN markers to get cell boundary positions in twips
#'
#' @param row_content Character string containing RTF row content
#' @return Numeric vector of cell positions in twips
#' @keywords internal
extract_cellx_positions <- function(row_content) {
  # Find all \cellxN markers using regex
  cellx_matches <- regmatches(row_content, gregexpr("\\\\cellx(\\d+)", row_content))[[1]]
  
  if (length(cellx_matches) == 0) {
    return(numeric(0))
  }
  
  # Extract numeric positions
  positions <- as.numeric(gsub("\\\\cellx(\\d+)", "\\1", cellx_matches))
  
  # Sort positions to ensure correct order
  sort(positions)
}

#' Extract cell content from RTF row
#'
#' Parse cell content between cell markers with improved RTF cleanup
#'
#' @param row_content Character string containing RTF row content
#' @return Character vector of cell contents
#' @keywords internal
extract_cell_contents <- function(row_content) {
  # Find all \cell} markers first
  cell_ends <- gregexpr("\\\\cell\\}", row_content)[[1]]
  
  if (length(cell_ends) == 0 || cell_ends[1] == -1) {
    return(character(0))
  }
  
  cell_texts <- character()
  
  for (i in seq_along(cell_ends)) {
    # For each \cell}, work backwards to find the start of cell content
    end_pos <- cell_ends[i]
    
    # Extract substring up to this \cell}
    before_cell <- substr(row_content, 1, end_pos - 1)
    
    # Find the last occurrence of common cell start patterns
    # Look for patterns like \f1{, \ql{, \qc{, \qr{, or just {
    start_patterns <- c("\\\\f\\d+\\{", "\\\\q[lcr]\\{", "\\{")
    
    start_pos <- 0
    for (pattern in start_patterns) {
      matches <- gregexpr(pattern, before_cell)[[1]]
      if (length(matches) > 0 && matches[1] != -1 && matches[length(matches)] > start_pos) {
        start_pos <- matches[length(matches)]
        # Adjust to skip the pattern itself and get to the content
        if (grepl("\\\\", pattern)) {
          # Skip the RTF command and the {
          match_len <- attr(matches, "match.length")[length(matches)]
          if (!is.null(match_len) && match_len > 0) {
            start_pos <- start_pos + match_len
          }
        } else {
          # Just skip the {
          start_pos <- start_pos + 1
        }
        break  # Use the first matching pattern
      }
    }
    
    # Extract cell content
    if (start_pos > 0 && start_pos < end_pos) {
      cell_content <- substr(row_content, start_pos, end_pos - 1)
      
      # First check for RTF indentation markers BEFORE cleaning
      indent_level <- 0
      
      # Check for \li (left indent) marker
      li_match <- regexpr("\\\\li(\\d+)", cell_content)
      if (li_match > 0) {
        indent_value <- as.numeric(sub("\\\\li", "", regmatches(cell_content, li_match)))
        # Convert twips to indent level (each level ~180-200 twips)
        indent_level <- round(indent_value / 180)
      }
      
      # Clean up RTF formatting
      clean_text <- cleanup_rtf_content(cell_content)
      
      # Add tilde markers for indentation
      if (indent_level > 0) {
        # Add pairs of tildes for each indent level
        clean_text <- paste0(strrep("~~", indent_level), clean_text)
      }
      
      # Always add the cell (even if empty) to maintain position mapping
      cell_texts <- c(cell_texts, clean_text)
    }
  }
  
  cell_texts
}

#' Clean up RTF formatting from cell content
#'
#' Remove RTF control words and formatting to get clean text content
#'
#' @param text Character string with RTF formatting
#' @return Character string with clean text
#' @keywords internal
cleanup_rtf_content <- function(text) {
  if (is.null(text) || length(text) == 0 || nchar(text) == 0) {
    return("")
  }
  
  # First convert {\line} to space before removing braces
  text <- gsub("\\{\\\\line\\}", " ", text)
  
  # Remove RTF control words (backslash followed by letters and optional digits)
  text <- gsub("\\\\[a-zA-Z]+\\d*", "", text)
  
  # Convert escaped tildes to regular tildes before removing other control symbols
  text <- gsub("\\\\~", "~", text)
  
  # Remove other RTF control symbols (backslash followed by single character, except tildes)
  text <- gsub("\\\\[^~]", "", text)
  
  # Remove extra braces
  text <- gsub("\\{|\\}", "", text)
  
  # Remove multiple spaces and trim
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  
  text
}

#' Find cells overlapping with a position range
#'
#' Given a list of cells and a position range, find which cells overlap
#' This is used for mapping data cells to header cells
#'
#' @param cells List of cell objects with start_pos and end_pos
#' @param start_pos Numeric start position to check
#' @param end_pos Numeric end position to check
#' @return Indices of overlapping cells
#' @export
find_overlapping_cells <- function(cells, start_pos, end_pos) {
  if (length(cells) == 0) {
    return(integer(0))
  }
  
  overlapping <- integer()
  
  for (i in seq_along(cells)) {
    cell <- cells[[i]]
    
    # Check if ranges overlap
    # Two ranges [a,b] and [c,d] overlap if max(a,c) < min(b,d)
    if (max(cell$start_pos, start_pos) < min(cell$end_pos, end_pos)) {
      overlapping <- c(overlapping, i)
    }
  }
  
  overlapping
}

#' Calculate cell span coverage
#'
#' For a given cell, calculate what percentage of other cells it covers
#' Used to identify spanning headers
#'
#' @param spanning_cell Cell object that might be spanning
#' @param covered_cells List of cell objects that might be covered
#' @return Numeric value between 0 and 1 indicating coverage
#' @export
calculate_cell_coverage <- function(spanning_cell, covered_cells) {
  if (length(covered_cells) == 0) {
    return(0)
  }
  
  total_coverage <- 0
  spanning_start <- spanning_cell$start_pos
  spanning_end <- spanning_cell$end_pos
  spanning_width <- spanning_end - spanning_start
  
  for (cell in covered_cells) {
    # Calculate overlap
    overlap_start <- max(spanning_start, cell$start_pos)
    overlap_end <- min(spanning_end, cell$end_pos)
    
    if (overlap_end > overlap_start) {
      overlap_width <- overlap_end - overlap_start
      total_coverage <- total_coverage + overlap_width
    }
  }
  
  # Return coverage as proportion of spanning cell width
  if (spanning_width > 0) {
    total_coverage / spanning_width
  } else {
    0
  }
}