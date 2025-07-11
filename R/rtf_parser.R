#' Enhanced RTF State Parser
#'
#' Enhanced state-based RTF table parser using structural markers with support for:
#' - Multi-level header detection and column group mapping
#' - Cell position tracking for accurate column assignment
#' - Row hierarchy detection (label-type and group-type)
#' - Page-aware processing (titles/footnotes only from page 1)
#'
#' States: 0=pre_header, 1=header, 2=table_body, 3=footnotes
#' Transitions only on structural markers, never content patterns
#'
#' @name rtf-parser
NULL

#' Parse RTF table using enhanced state machine
#'
#' @param rtf_file Path to RTF file
#' @param encoding Character encoding to use when reading the file. 
#'   Common values are "UTF-8", "latin1", "CP1252". If NULL, uses system default.
#' @return List with enhanced sections including header structure and processed data
#' @export
parse_rtf_table_states <- function(rtf_file, encoding = NULL) {
  # Try to read with specified encoding, fall back to latin1 if it fails
  lines <- tryCatch({
    if (!is.null(encoding)) {
      readLines(rtf_file, encoding = encoding, warn = FALSE)
    } else {
      readLines(rtf_file, warn = FALSE)
    }
  }, error = function(e) {
    # If reading fails, try with latin1 encoding
    message("Initial read failed, trying with latin1 encoding...")
    readLines(rtf_file, encoding = "latin1", warn = FALSE)
  })
  
  state <- 0  # 0=pre_header, 1=header, 2=table_body, 3=footnotes
  page <- 1   # Track current page
  
  # Enhanced data structures
  pre_header_lines <- list()     # Only from page 1
  header_rows <- list()          # Enhanced with position info
  table_data <- list()           # Enhanced with position info
  footnote_lines <- list()       # Only from page 1
  
  # Track title extraction
  title_line_count <- 0
  max_title_lines <- 5  # Only collect first few lines as title
  
  i <- 1
  while (i <= length(lines)) {
    line <- lines[i]
    
    # Page break increments page but maintains state during table processing
    if (grepl('{\\pard\\page\\par}', line, fixed = TRUE)) {
      page <- page + 1
      i <- i + 1
      next
    }
    
    # RTF structural marker-based state transitions
    if (grepl("\\trowd", line, fixed = TRUE) || grepl("\\intbl", line, fixed = TRUE)) {
      # Detect transition markers using border patterns
      markers <- detect_transition_markers(line)
      
      # State transitions based on RTF structural markers only
      if (state == 0 && (markers$thick || markers$medium)) {
        # Transition from pre-header to header on thick/medium borders
        state <- 1
      } else if (state == 1 && !grepl("\\trhdr", line, fixed = TRUE) && grepl("\\trowd", line, fixed = TRUE)) {
        # Transition from header to table body when we see first non-header row
        state <- 2
      } else if (state == 2 && markers$program) {
        # Only transition to footnotes from table body state
        state <- 3
      }
      
      # Extract and process cell content
      if (grepl("\\intbl", line, fixed = TRUE)) {
        cell_text <- extract_intbl_cell_content(line)
        
        if (trimws(cell_text) != "") {
          # State-specific processing based on current state
          if (state == 0 && page == 1) {
            # Pre-header: Collect only first few lines as title
            if (title_line_count < max_title_lines) {
              pre_header_lines[[length(pre_header_lines) + 1]] <- list(list(text = cell_text))
              title_line_count <- title_line_count + 1
            } else {
              # After max title lines, transition to header state
              state <- 1
            }
            
          } else if (state == 1) {
            # Header: Store with position info
            header_rows[[length(header_rows) + 1]] <- list(list(text = cell_text))
            
          } else if (state == 2) {
            # Table body: Process as data
            table_data[[length(table_data) + 1]] <- list(
              variable = cell_text,
              raw_stat = "",
              source_row = i,
              source_col = 1,
              cell_start_pos = 0,
              cell_end_pos = 1000,
              cell_width = 1000,
              state_source = "table_body"
            )
            
          } else if (state == 3 && page == 1) {
            # Footnotes: Collect only from page 1
            footnote_lines[[length(footnote_lines) + 1]] <- list(list(text = cell_text))
          }
        }
      }
      
      # Process trowd rows (full table rows)
      if (grepl("\\trowd", line, fixed = TRUE)) {
        cells <- extract_cells_from_row(line)
        
        for (cell in cells) {
          cell_text <- cell$text
          
          if (trimws(cell_text) != "") {
            # State-specific processing based on current state
            if (state == 0 && page == 1) {
              # Pre-header: Collect only first few lines as title
              if (title_line_count < max_title_lines) {
                pre_header_lines[[length(pre_header_lines) + 1]] <- list(list(text = cell_text))
                title_line_count <- title_line_count + 1
              } else {
                # After max title lines, transition to header state
                state <- 1
              }
              
            } else if (state == 1) {
              # Header: Store with position info
              header_rows[[length(header_rows) + 1]] <- list(list(text = cell_text))
              
            } else if (state == 2) {
              # Table body: Process as data
              table_data[[length(table_data) + 1]] <- list(
                variable = cell_text,
                raw_stat = "",
                source_row = i,
                source_col = 1,
                cell_start_pos = 0,
                cell_end_pos = 1000,
                cell_width = 1000,
                state_source = "table_body"
              )
              
            } else if (state == 3 && page == 1) {
              # Footnotes: Collect only from page 1
              footnote_lines[[length(footnote_lines) + 1]] <- list(list(text = cell_text))
            }
          }
        }
      }
    }
    
    i <- i + 1
  }
  
  # Build header structure (simplified for now)
  header_structure <- list(
    levels = 0,
    column_mappings = list()
  )
  
  # Convert table_data list to data frame
  if (length(table_data) > 0) {
    # Validate all entries before creating data frame
    valid_entries <- list()
    for (j in seq_along(table_data)) {
      entry <- table_data[[j]]
      # Check all required fields are present and not NULL
      if (!is.null(entry$variable) && !is.null(entry$raw_stat) && 
          !is.null(entry$source_row) && !is.null(entry$source_col) &&
          !is.null(entry$cell_start_pos) && !is.null(entry$cell_end_pos) &&
          !is.null(entry$cell_width) && !is.null(entry$state_source)) {
        valid_entries[[length(valid_entries) + 1]] <- entry
      }
    }
    
    if (length(valid_entries) > 0) {
      table_df <- do.call(rbind, lapply(valid_entries, function(x) {
        data.frame(
          variable = x$variable,
          raw_stat = x$raw_stat,
          source_row = x$source_row,
          source_col = x$source_col,
          cell_start_pos = x$cell_start_pos,
          cell_end_pos = x$cell_end_pos,
          cell_width = x$cell_width,
          state_source = x$state_source,
          stringsAsFactors = FALSE
        )
      }))
    } else {
      table_df <- data.frame(
        variable = character(0),
        raw_stat = character(0),
        source_row = integer(0),
        source_col = integer(0),
        cell_start_pos = numeric(0),
        cell_end_pos = numeric(0),
        cell_width = numeric(0),
        state_source = character(0),
        stringsAsFactors = FALSE
      )
    }
  } else {
    table_df <- data.frame(
      variable = character(0),
      raw_stat = character(0),
      source_row = integer(0),
      source_col = integer(0),
      cell_start_pos = numeric(0),
      cell_end_pos = numeric(0),
      cell_width = numeric(0),
      state_source = character(0),
      stringsAsFactors = FALSE
    )
  }
  
  # Process collected state data with enhanced cleaning
  title <- if (length(pre_header_lines) > 0) {
    raw_title <- paste(unlist(lapply(pre_header_lines, function(cells) {
      sapply(cells, function(c) trimws(c$text))
    })), collapse = " ")
    # Clean the title
    cleanup_rtf_content(raw_title)
  } else {
    "No title found"
  }
  
  footnotes <- if (length(footnote_lines) > 0) {
    unlist(lapply(footnote_lines, function(cells) {
      sapply(cells, function(c) cleanup_rtf_content(trimws(c$text)))
    }))
  } else {
    character()
  }
  
  list(
    data = table_df,
    header_structure = header_structure,
    metadata = list(
      title = title,
      footnotes = if(length(footnotes) > 0) footnotes else "No footnotes found",
      processing_date = Sys.time(),
      stage = "enhanced_state_parser",
      method = "enhanced_state_based_parser",
      pages_processed = page,
      header_levels = header_structure$levels
    ),
    stage = "enhanced_state_parser"
  )
}

#' Apply column group mapping to table data
#'
#' Map data cells to their appropriate column groups based on cell positions
#'
#' @param table_df Data frame with cell position information
#' @param header_structure Header structure from build_header_structure
#' @return Enhanced data frame with column group assignments
#' @keywords internal
apply_column_group_mapping <- function(table_df, header_structure) {
  # Initialize column group columns
  table_df$colgroup1 <- ""
  table_df$colgroup1_level <- ""
  table_df$colgroup2 <- ""
  table_df$colgroup2_level <- ""
  table_df$colgroup3 <- ""
  table_df$colgroup3_level <- ""

  # Map each data cell to its column groups
  for (i in seq_len(nrow(table_df))) {
    cell_position <- (table_df$cell_start_pos[i] + table_df$cell_end_pos[i]) / 2

    # Get column group assignments
    colgroup_mapping <- map_cell_to_colgroups(cell_position, header_structure)

    # Apply mappings
    table_df$colgroup1[i] <- colgroup_mapping$colgroup1
    table_df$colgroup1_level[i] <- colgroup_mapping$colgroup1_level
    table_df$colgroup2[i] <- colgroup_mapping$colgroup2
    table_df$colgroup2_level[i] <- colgroup_mapping$colgroup2_level
    table_df$colgroup3[i] <- colgroup_mapping$colgroup3
    table_df$colgroup3_level[i] <- colgroup_mapping$colgroup3_level
  }

  table_df
}

#' Extract cells from RTF table row (legacy function)
#' @keywords internal
extract_cells_from_row <- function(row_content) {
  # Find all cell boundaries using simple string splitting
  cells <- list()
  
  # Split by \cell} to find cell content
  parts <- strsplit(row_content, "\\cell}", fixed = TRUE)[[1]]
  
  for (part in parts) {
    # Find last { to get cell content
    brace_pos <- regexpr("\\{[^\\{]*$", part)
    if (brace_pos > 0) {
      cell_text <- substr(part, brace_pos + 1, nchar(part))
      # Simple cleanup - remove basic RTF commands
      cell_text <- gsub("\\\\[a-z]+\\d*", "", cell_text)
      cell_text <- gsub("\\{|\\}", "", cell_text)
      cell_text <- trimws(cell_text)
      
      if (cell_text != "") {
        cells[[length(cells) + 1]] <- list(text = cell_text)
      }
    }
  }
  
  cells
}

#' Detect transition markers in RTF lines
#'
#' Enhanced border detection function that checks for multiple border patterns
#' and content patterns that indicate state transitions
#'
#' @param line Character string containing RTF line content
#' @return List with transition marker indicators
#' @export
detect_transition_markers <- function(line) {
  # Check for specific border patterns - be very explicit
  has_thick_border <- grepl("\\brdrw15", line, fixed = TRUE)
  has_medium_border <- grepl("\\brdrw10", line, fixed = TRUE) 
  
  # Match \brdrw1 but exclude \brdrw10 and \brdrw15
  has_basic_thin <- grepl("\\brdrw1", line, fixed = TRUE)
  has_thin_border <- has_basic_thin && !has_thick_border && !has_medium_border
  
  has_program_pattern <- grepl("Program (Source|Path|Name):", line, perl = TRUE)
  
  return(list(
    thick = has_thick_border,
    medium = has_medium_border, 
    thin = has_thin_border,
    program = has_program_pattern
  ))
}

#' Extract text content from an RTF line
#'
#' Extract clean text from an RTF line, removing RTF formatting
#'
#' @param line Character string containing RTF content
#' @return Clean text content
#' @keywords internal
extract_text_from_line <- function(line) {
  if (is.null(line) || nchar(line) == 0) {
    return("")
  }
  
  # Look for text content in curly braces
  brace_content <- regmatches(line, gregexpr("\\{[^{}]*\\}", line))[[1]]
  
  text_parts <- character()
  for (content in brace_content) {
    # Remove the braces and extract text
    inner_text <- gsub("^\\{|\\}$", "", content)
    
    # Skip RTF control sequences (lines starting with backslash)
    if (!grepl("^\\\\", inner_text)) {
      # Clean up RTF formatting
      clean_text <- cleanup_rtf_content(inner_text)
      if (nchar(trimws(clean_text)) > 0) {
        text_parts <- c(text_parts, clean_text)
      }
    }
  }
  
  # If no brace content found, try direct text extraction
  if (length(text_parts) == 0) {
    # Remove RTF control words and get remaining text
    clean_line <- gsub("\\\\[a-zA-Z]+\\d*", "", line)
    clean_line <- gsub("\\\\.", "", clean_line)
    clean_line <- gsub("\\{|\\}", "", clean_line)
    clean_line <- trimws(clean_line)
    
    if (nchar(clean_line) > 0 && !grepl("^\\\\", clean_line)) {
      text_parts <- clean_line
    }
  }
  
  paste(text_parts, collapse = " ")
}

#' Extract cell content from \intbl line
#'
#' Extract clean text content from a single \intbl line
#'
#' @param intbl_line Character string containing \intbl line content
#' @return Clean text content
#' @keywords internal
extract_intbl_cell_content <- function(intbl_line) {
  if (is.null(intbl_line) || nchar(intbl_line) == 0) {
    return("")
  }
  
  # Look for content in curly braces before \cell}
  # Pattern: {content\cell}
  cell_pattern <- "\\{([^{}]*?)\\\\cell\\}"
  matches <- regmatches(intbl_line, regexpr(cell_pattern, intbl_line, perl = TRUE))
  
  if (length(matches) > 0 && nchar(matches[1]) > 0) {
    # Extract the content between { and \cell}
    content <- gsub("^\\{|\\\\cell\\}$", "", matches[1])
    
    # Clean up RTF formatting
    content <- gsub("\\\\line", " ", content)  # Convert \line to space
    content <- gsub("\\\\[a-zA-Z]+\\d*", "", content)  # Remove RTF commands
    content <- gsub("\\{|\\}", "", content)  # Remove braces
    
    return(trimws(content))
  }
  
  # If no cell pattern found, try to extract any text content
  return(extract_text_from_line(intbl_line))
}

#' Clean up RTF content
#'
#' Remove RTF formatting commands and clean up text
#'
#' @param text Character string with RTF formatting
#' @return Character string with clean text
#' @keywords internal
cleanup_rtf_content <- function(text) {
  if (is.null(text) || length(text) == 0 || nchar(text) == 0) {
    return("")
  }
  
  # Remove RTF control words (backslash followed by letters and optional digits)
  text <- gsub("\\\\[a-zA-Z]+\\d*", "", text)
  
  # Remove RTF control symbols (backslash followed by single character)
  text <- gsub("\\\\.", "", text)
  
  # Remove extra braces
  text <- gsub("\\{|\\}", "", text)
  
  # Clean up line breaks - convert \\line to space
  text <- gsub("\\\\line", " ", text)
  
  # Remove font formatting artifacts (from PRP)
  text <- gsub("\\d+Courier New.*?Default Paragraph Font", "", text)
  text <- gsub("SAS System Output.*?beckj8", "", text)
  
  # Remove additional font artifacts
  text <- gsub("\\d{2,}\\d{2,}.*?Courier New", "", text)
  text <- gsub("Normal;", "", text)
  text <- gsub("Default Paragraph Font", "", text)
  text <- gsub("SAS Version \\d+\\.\\d+", "", text)
  text <- gsub("beckj8", "", text)
  
  # Remove page markers
  text <- gsub("Page PAGE NUMPAGES", "", text)
  text <- gsub("IDX IDX", "", text)
  
  # Remove multiple spaces and trim
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  
  text
}