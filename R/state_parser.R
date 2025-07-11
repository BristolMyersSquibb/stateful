#' State Parser (Stage 1) - Simple Border-Based RTF Parsing
#'
#' Simple state-based RTF parser using only thick borders (brdrw15) for state transitions
#'
#' @name state-parser
NULL

#' Parse RTF using simple border-based state machine (Stage 1)
#'
#' Extracts table structure using simple state transitions based on thick borders.
#' States: 0=pre_header, 1=header, 2=table_body, 3=footnotes
#' Transitions only on brdrw15 borders. Page breaks reset to state 0.
#'
#' @param rtf_file Path to RTF file
#' @return List with structured ARD data and metadata
#' @export
state_parser_stage1 <- function(rtf_file) {
  message("State Parser Stage 1: Position-based column mapping...")
  
  tryCatch({
    result <- parse_rtf_table_states(rtf_file)
    message("Stage 1 complete: ", nrow(result$data), " rows extracted")
    
    # Build proper colgroup structure based on cell positions
    if (nrow(result$data) > 0) {
      result <- build_colgroup_structure_from_positions(result, rtf_file)
    }
    
    result
    
  }, error = function(e) {
    warning("State Parser Stage 1 failed: ", e$message)
    return(list(
      data = data.frame(
        variable = character(), 
        raw_stat = character(), 
        source_row = integer(), 
        source_col = integer(),
        state_source = character()
      ),
      metadata = list(
        title = "No title found",
        footnotes = "No footnotes found",
        processing_date = Sys.time(),
        stage = "state_parser_stage1",
        method = "simple_border_parser", 
        status = "error",
        error = e$message
      ),
      stage = "state_parser_stage1"
    ))
  })
}

#' Build colgroup structure from cell positions and header analysis
#'
#' @param result Parse result from parse_rtf_table_states
#' @param rtf_file Path to RTF file for header analysis
#' @return Enhanced result with proper colgroup structure
#' @keywords internal
build_colgroup_structure_from_positions <- function(result, rtf_file) {
  # Step 1: Extract header structure with cell positions
  header_structure <- extract_header_structure_with_positions(rtf_file)
  
  # Step 2: Build colgroup mappings based on position overlap
  colgroup_mappings <- build_position_based_mappings(header_structure)
  
  # Step 3: Apply mappings to data
  if (nrow(result$data) > 0) {
    result$data <- apply_colgroup_mappings_to_data(result$data, colgroup_mappings)
  }
  
  # Step 4: Update metadata with header structure info
  result$header_structure <- header_structure
  result$colgroup_mappings <- colgroup_mappings
  
  return(result)
}

#' Extract header structure with cell positions from RTF
#'
#' @param rtf_file Path to RTF file
#' @return List with header levels and cell position information
#' @keywords internal
extract_header_structure_with_positions <- function(rtf_file) {
  lines <- readLines(rtf_file, warn = FALSE)
  
  header_rows <- list()
  in_header <- FALSE
  current_row <- NULL
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # Detect header row start
    if (grepl("\\\\trhdr", line)) {
      in_header <- TRUE
      current_row <- list(cells = list(), start_line = i)
      next
    }
    
    # Extract cells from header rows
    if (in_header && grepl("\\\\intbl", line) && grepl("\\\\cell", line)) {
      cell_info <- extract_cell_with_position_fixed(line, lines, i)
      if (!is.null(cell_info)) {
        current_row$cells[[length(current_row$cells) + 1]] <- cell_info
      }
    }
    
    # End of row
    if (in_header && grepl("\\\\row}", line)) {
      if (!is.null(current_row) && length(current_row$cells) > 0) {
        header_rows[[length(header_rows) + 1]] <- current_row
      }
      in_header <- FALSE
      current_row <- NULL
    }
  }
  
  return(list(
    levels = length(header_rows),
    header_rows = header_rows
  ))
}

#' Extract cell content and position from RTF line (corrected version)
#'
#' @param line RTF line containing cell information
#' @param lines All RTF lines (for position lookup)
#' @param line_index Current line index
#' @return List with cell text and position, or NULL if not found
#' @keywords internal
extract_cell_with_position_fixed <- function(line, lines, line_index) {
  # Extract cell content from RTF using multiple patterns
  
  # Pattern 1: f1{content}\cell (for treatment groups like "Nivo (Arm A)")
  pattern1 <- "f1\\{(.+?)\\\\cell"
  match1 <- regexpr(pattern1, line, perl = TRUE)
  
  if (match1 != -1) {
    start_pos <- attr(match1, "capture.start")
    length_pos <- attr(match1, "capture.length")
    
    if (length(start_pos) > 0 && start_pos > 0) {
      cell_text <- substr(line, start_pos, start_pos + length_pos - 1)
      cell_text <- cleanup_rtf_content(cell_text)
      
      # Extract position
      position <- extract_cell_position_from_context(line, lines, line_index)
      
      return(list(
        text = cell_text,
        position = position,
        line_num = line_index
      ))
    }
  }
  
  # Pattern 2: Standard {content}\cell pattern
  pattern2 <- "\\{([^{}]*?)\\\\cell"
  match2 <- regexpr(pattern2, line, perl = TRUE)
  
  if (match2 != -1) {
    start_pos <- attr(match2, "capture.start")
    length_pos <- attr(match2, "capture.length")
    
    if (length(start_pos) > 0 && start_pos > 0) {
      cell_text <- substr(line, start_pos, start_pos + length_pos - 1)
      cell_text <- cleanup_rtf_content(cell_text)
      
      # Extract position
      position <- extract_cell_position_from_context(line, lines, line_index)
      
      return(list(
        text = cell_text,
        position = position,
        line_num = line_index
      ))
    }
  }
  
  return(NULL)
}

#' Extract cell position from RTF context
#' @param line Current line
#' @param lines All lines
#' @param line_index Current line index
#' @return Cell position or NA
#' @keywords internal
extract_cell_position_from_context <- function(line, lines, line_index) {
  # Look for \cellx marker in current line first
  cellx_pattern <- "\\\\cellx(\\d+)"
  cellx_match <- regexpr(cellx_pattern, line, perl = TRUE)
  
  if (cellx_match != -1) {
    return(as.numeric(gsub("\\\\cellx", "", regmatches(line, cellx_match))))
  }
  
  # Look in next few lines
  for (j in (line_index + 1):min(line_index + 5, length(lines))) {
    if (j > length(lines)) break
    
    next_line <- lines[j]
    cellx_match <- regexpr(cellx_pattern, next_line, perl = TRUE)
    
    if (cellx_match != -1) {
      return(as.numeric(gsub("\\\\cellx", "", regmatches(next_line, cellx_match))))
    }
    
    # Stop if we hit another cell or row end
    if (grepl("\\\\cell|\\\\row}", next_line)) break
  }
  
  return(NA_real_)
}

#' Build position-based column mappings
#'
#' @param header_structure Header structure with positions
#' @return List of column mappings for colgroup1/2/3
#' @keywords internal
build_position_based_mappings <- function(header_structure) {
  if (length(header_structure$header_rows) == 0) {
    return(list())
  }
  
  mappings <- list()
  
  # Process header rows to build colgroup hierarchy
  for (level in seq_along(header_structure$header_rows)) {
    row <- header_structure$header_rows[[level]]
    
    # Sort cells by position to ensure correct order
    cells_with_positions <- row$cells[!sapply(row$cells, function(x) is.na(x$position))]
    if (length(cells_with_positions) == 0) next
    
    # Sort by position
    positions <- sapply(cells_with_positions, function(x) x$position)
    sorted_indices <- order(positions)
    sorted_cells <- cells_with_positions[sorted_indices]
    
    # Calculate start positions based on previous cell positions
    prev_pos <- 0
    
    for (i in seq_along(sorted_cells)) {
      cell <- sorted_cells[[i]]
      
      # Skip empty cells
      if (nchar(trimws(cell$text)) == 0) {
        prev_pos <- cell$position
        next
      }
      
      start_pos <- prev_pos
      end_pos <- cell$position
      
      # Create mapping entry
      mapping_key <- paste0("level", level, "_pos_", start_pos, "_", end_pos)
      
      # Position-based colgroup assignment - NO CONTENT MATCHING
      # Simply map based on header level:
      # Level 1 → colgroup1 (primary grouping)
      # Level 2 → colgroup2 (secondary grouping)  
      # Level 3 → colgroup3 (tertiary grouping)
      colgroup_name <- paste0("colgroup", level)
      colgroup_level_name <- paste0("colgroup", level, "_level")
      
      mappings[[mapping_key]] <- list(
        start_pos = start_pos,
        end_pos = end_pos,
        level = level,
        colgroup = colgroup_name,
        colgroup_level = colgroup_level_name,
        text = trimws(cell$text),
        original_position = cell$position
      )
      
      prev_pos <- cell$position
    }
  }
  
  return(mappings)
}

#' Apply colgroup mappings to data
#'
#' @param data Data frame from parser
#' @param colgroup_mappings Position-based mappings
#' @return Data frame with colgroup columns added
#' @keywords internal
apply_colgroup_mappings_to_data <- function(data, colgroup_mappings) {
  if (length(colgroup_mappings) == 0 || nrow(data) == 0) {
    return(data)
  }
  
  # Add colgroup columns
  data$colgroup1 <- NA_character_
  data$colgroup1_level <- NA_character_
  data$colgroup2 <- NA_character_
  data$colgroup2_level <- NA_character_
  data$colgroup3 <- NA_character_
  data$colgroup3_level <- NA_character_
  
  # Filter out header rows from data - we only want actual data rows
  # Use structural markers only - NO CONTENT MATCHING
  data_rows_mask <- !is.na(data$raw_stat) & nchar(trimws(data$raw_stat)) > 0 & 
                    data$state_source == "table_body"
  
  if (!any(data_rows_mask)) {
    return(data)
  }
  
  # Map data rows to colgroups based on cell position  
  for (i in which(data_rows_mask)) {
    row_data <- data[i, ]
    
    # Use cell position information to find matching colgroup
    cell_start <- if ("cell_start_pos" %in% names(row_data)) row_data$cell_start_pos else 0
    cell_end <- if ("cell_end_pos" %in% names(row_data)) row_data$cell_end_pos else row_data$source_col * 3000
    
    # Find matching mappings based on position overlap
    for (mapping_key in names(colgroup_mappings)) {
      mapping <- colgroup_mappings[[mapping_key]]
      
      # Check if data cell position overlaps with header cell position
      if (cell_start >= mapping$start_pos && cell_end <= mapping$end_pos) {
        
        # Apply mapping to data frame
        data[i, mapping$colgroup] <- mapping$colgroup
        data[i, mapping$colgroup_level] <- mapping$text
        break
      }
    }
  }
  
  return(data)
}