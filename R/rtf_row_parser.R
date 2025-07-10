#' RTF Row-Based Parser Functions
#'
#' Implements proper row-based extraction from \trowd to \row
#' with position-based column mapping
#'
#' @keywords internal

#' Extract complete RTF row from \trowd to \row
#'
#' @param lines All RTF lines
#' @param start_idx Index of line containing \trowd
#' @return List with row content and end index
#' @keywords internal
extract_complete_rtf_row <- function(lines, start_idx) {
  if (start_idx > length(lines)) return(NULL)
  
  # Collect all lines from \trowd to \row
  row_lines <- character()
  i <- start_idx
  
  while (i <= length(lines)) {
    row_lines <- c(row_lines, lines[i])
    
    # Check for row end
    if (grepl("{\\row}", lines[i], fixed = TRUE)) {
      return(list(
        lines = row_lines,
        content = paste(row_lines, collapse = "\n"),
        start_idx = start_idx,
        end_idx = i
      ))
    }
    
    i <- i + 1
    
    # Safety check
    if (i > start_idx + 100) {
      warning("Row extends beyond 100 lines, may be malformed")
      return(NULL)
    }
  }
  
  return(NULL)
}

#' Extract all cells from a complete RTF row
#'
#' @param row_content Complete row content from \trowd to \row
#' @return Data frame with cell text and positions
#' @keywords internal
extract_cells_from_rtf_row <- function(row_content) {
  cells <- list()
  
  # Split by \pard to find cell definitions
  cell_parts <- strsplit(row_content, "\\pard", fixed = TRUE)[[1]]
  
  for (part in cell_parts) {
    # Skip empty parts
    if (nchar(trimws(part)) == 0) next
    
    # Look for cell content and position
    if (grepl("\\intbl", part, fixed = TRUE) && grepl("\\cell", part, fixed = TRUE)) {
      
      # Extract cell text
      cell_text <- extract_cell_text_from_part(part)
      
      # Extract cell position from subsequent \cellx marker
      cell_position <- extract_cellx_position(part)
      
      # Include cells even if empty (but not NULL)
      if (!is.null(cell_text)) {
        cells[[length(cells) + 1]] <- list(
          text = cell_text,
          position = cell_position
        )
      }
    }
  }
  
  # Convert to data frame
  if (length(cells) > 0) {
    df <- data.frame(
      text = sapply(cells, function(x) x$text),
      end_pos = sapply(cells, function(x) ifelse(is.na(x$position), NA, x$position)),
      stringsAsFactors = FALSE
    )
    
    # Calculate start positions
    df$start_pos <- c(0, df$end_pos[-length(df$end_pos)])
    
    # Add column index
    df$col_index <- seq_len(nrow(df))
    
    return(df[, c("col_index", "text", "start_pos", "end_pos")])
  }
  
  return(data.frame(col_index = integer(), text = character(), 
                   start_pos = numeric(), end_pos = numeric()))
}

#' Extract cell text from RTF part
#'
#' @param part RTF content part
#' @return Clean cell text
#' @keywords internal
extract_cell_text_from_part <- function(part) {
  # Check for empty cell pattern first
  if (grepl("\\{\\\\cell\\}", part, fixed = TRUE)) {
    return("")  # Return empty string for empty cells
  }
  
  # Multiple patterns to try
  patterns <- list(
    # Pattern 1: f1{content\cell}
    list(pattern = "f1\\{(.+?)\\\\cell", group = 1),
    # Pattern 2: {content\cell}
    list(pattern = "\\{([^{}]+?)\\\\cell", group = 1),
    # Pattern 3: plain content before \cell
    list(pattern = "([^\\{]+?)\\\\cell", group = 1)
  )
  
  for (p in patterns) {
    matches <- regmatches(part, regexec(p$pattern, part, perl = TRUE))
    if (length(matches[[1]]) > p$group) {
      text <- matches[[1]][p$group + 1]
      # Clean up the text
      text <- cleanup_cell_text(text)
      # Return text even if empty after cleanup
      return(text)
    }
  }
  
  # Fallback: extract any text before \cell
  if (grepl("\\\\cell", part, fixed = TRUE)) {
    text <- sub(".*?([^\\\\]+)\\\\cell.*", "\\1", part, perl = TRUE)
    return(cleanup_cell_text(text))
  }
  
  return(NULL)
}

#' Clean up RTF cell text
#'
#' @param text Raw cell text
#' @return Clean text
#' @keywords internal
cleanup_cell_text <- function(text) {
  if (is.null(text) || length(text) == 0) return("")
  
  # Replace {\line} with newline
  text <- gsub("\\{\\\\line\\}", "\n", text, fixed = TRUE)
  
  # Remove RTF control words
  text <- gsub("\\\\[a-zA-Z]+\\d*\\s*", " ", text)
  
  # Remove extra braces
  text <- gsub("[{}]", "", text)
  
  # Clean up whitespace
  text <- trimws(text)
  text <- gsub("\\s+", " ", text)
  
  return(text)
}

#' Extract cell position from \cellx marker
#'
#' @param content RTF content containing cellx
#' @return Numeric position or NA
#' @keywords internal
extract_cellx_position <- function(content) {
  # Look for \cellxN pattern
  cellx_match <- regexpr("\\\\cellx(\\d+)", content, perl = TRUE)
  
  if (cellx_match > 0) {
    match_text <- regmatches(content, cellx_match)
    position <- as.numeric(gsub("\\\\cellx", "", match_text))
    return(position)
  }
  
  return(NA)
}

#' Parse RTF table with complete row extraction
#'
#' @param rtf_file Path to RTF file
#' @return List with parsed table structure
#' @export
parse_rtf_table_rows <- function(rtf_file) {
  lines <- readLines(rtf_file, warn = FALSE)
  
  # Initialize results
  result <- list(
    pre_header = character(),
    header_rows = list(),
    data_rows = list(),
    footnotes = character()
  )
  
  # State tracking
  state <- 0  # 0=pre_header, 1=header, 2=data, 3=footnotes
  i <- 1
  
  while (i <= length(lines)) {
    line <- lines[i]
    
    # Check for page break - reset to state 0
    if (grepl("\\\\page", line, fixed = TRUE)) {
      state <- 0
      i <- i + 1
      next
    }
    
    # Check for state transitions
    if (grepl("\\trowd", line, fixed = TRUE)) {
      # Found start of a row
      row_data <- extract_complete_rtf_row(lines, i)
      
      if (!is.null(row_data)) {
        # Extract cells from the row
        cells <- extract_cells_from_rtf_row(row_data$content)
        
        # Determine row type and state
        if (grepl("\\trhdr", row_data$content, fixed = TRUE)) {
          # Header row
          state <- 1
          result$header_rows[[length(result$header_rows) + 1]] <- list(
            cells = cells,
            row_index = i
          )
        } else if (state == 0) {
          # Still in pre-header
          result$pre_header <- c(result$pre_header, row_data$content)
        } else if (state >= 1) {
          # Check for state transitions based on borders and content
          
          # Check for border markers that indicate end of data
          # Need to check without the extra backslash escaping
          has_thick_border <- grepl("\\brdrw15", row_data$content, fixed = TRUE)
          has_medium_border <- grepl("\\brdrw10", row_data$content, fixed = TRUE)
          has_thin_border <- grepl("\\brdrw1", row_data$content, fixed = TRUE)
          
          # Check if this is a full-width separator row
          # Look for a row with minimal content and full-width border
          is_separator_row <- FALSE
          if (has_thick_border && grepl("\\sl-5", row_data$content, fixed = TRUE)) {
            # This is the specific pattern for separator rows
            is_separator_row <- TRUE
          }
          
          # State transitions based on borders
          if (state == 2 && is_separator_row) {
            # Transition to footnotes when we see the separator row
            state <- 3
            # Don't add this row as data - it's the separator
          } else if (state == 3) {
            # In footnote state - collect as footnotes, not data
            if (nrow(cells) > 0) {
              for (j in 1:nrow(cells)) {
                if (nchar(trimws(cells$text[j])) > 0) {
                  result$footnotes <- c(result$footnotes, cells$text[j])
                }
              }
            }
          } else {
            # Regular data row
            state <- 2
            
            # Only add as data if it has actual content
            if (nrow(cells) > 0 && any(nchar(trimws(cells$text)) > 0)) {
              result$data_rows[[length(result$data_rows) + 1]] <- list(
                cells = cells,
                row_index = i
              )
            }
          }
        }
        
        # Jump to end of row
        i <- row_data$end_idx
      }
    } else {
      # Non-row content
      if (state == 0) {
        result$pre_header <- c(result$pre_header, line)
      } else if (state == 3) {
        # Already in footnote state
        result$footnotes <- c(result$footnotes, line)
      }
    }
    
    i <- i + 1
  }
  
  return(result)
}

#' Build header structure with position-based mapping
#'
#' @param header_rows List of header rows with cells
#' @return Header structure for column mapping
#' @keywords internal
build_header_structure_from_rows <- function(header_rows) {
  if (length(header_rows) == 0) return(list())
  
  # Build multi-level structure
  structure <- list(
    levels = length(header_rows),
    mappings = list()
  )
  
  # Find the actual header rows (should be first 2 with trhdr)
  actual_header_rows <- list()
  for (row in header_rows) {
    if (nrow(row$cells) > 1) {  # Skip title/empty rows
      actual_header_rows[[length(actual_header_rows) + 1]] <- row
    }
    if (length(actual_header_rows) >= 2) break  # Only need first 2 levels
  }
  
  structure$levels <- length(actual_header_rows)
  
  # Process header levels - POSITION BASED, NO CONTENT MATCHING
  for (level_idx in seq_along(actual_header_rows)) {
    row <- actual_header_rows[[level_idx]]
    if (nrow(row$cells) > 0) {
      for (i in seq_len(nrow(row$cells))) {
        cell <- row$cells[i, ]
        if (nchar(trimws(cell$text)) == 0) next
        
        # Map purely by level - no content checking
        mapping_key <- paste0("L", level_idx, "_", cell$start_pos, "_", cell$end_pos)
        structure$mappings[[mapping_key]] <- list(
          level = level_idx,
          text = cell$text,
          start_pos = cell$start_pos,
          end_pos = cell$end_pos,
          group = paste0("colgroup", level_idx)  # Level 1 -> colgroup1, Level 2 -> colgroup2
        )
      }
    }
  }
  
  return(structure)
}

#' Map data row to column groups based on position
#'
#' @param data_cells Data frame of cells from a data row
#' @param header_structure Header structure with mappings
#' @return Data frame with column group assignments
#' @keywords internal
map_data_to_column_groups <- function(data_cells, header_structure) {
  if (nrow(data_cells) == 0) return(data_cells)
  
  # Add column group columns
  data_cells$colgroup1 <- NA
  data_cells$colgroup1_level <- NA
  data_cells$colgroup2 <- NA
  data_cells$colgroup2_level <- NA
  data_cells$colgroup3 <- NA
  data_cells$colgroup3_level <- NA
  
  # Map each cell based on position
  for (i in seq_len(nrow(data_cells))) {
    cell <- data_cells[i, ]
    
    # Find overlapping header mappings for each group level
    for (group in c("colgroup1", "colgroup2")) {
      best_match <- NULL
      
      for (mapping in header_structure$mappings) {
        if (mapping$group != group) next
        
        # Check if cell position overlaps with header position
        if (!is.na(cell$start_pos) && !is.na(cell$end_pos)) {
          # For colgroup1, use broader overlap (cell within header span)
          if (group == "colgroup1" && 
              cell$start_pos >= mapping$start_pos && 
              cell$end_pos <= mapping$end_pos) {
            best_match <- mapping
          }
          # For colgroup2, use overlap check (cell within header)
          else if (group == "colgroup2" &&
                   cell$start_pos >= mapping$start_pos &&
                   cell$end_pos <= mapping$end_pos) {
            best_match <- mapping
          }
        }
      }
      
      if (!is.null(best_match)) {
        group_col <- best_match$group
        level_col <- paste0(best_match$group, "_level")
        
        data_cells[i, group_col] <- best_match$group
        data_cells[i, level_col] <- best_match$text
      }
    }
  }
  
  return(data_cells)
}