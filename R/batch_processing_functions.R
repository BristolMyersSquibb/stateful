# Batch processing functions for RTF conversion
# These functions are used by both the batch script and the Shiny app

# Source required dependencies
# Handle both development and deployment paths
if (file.exists('R/rtf_cell_utils.R')) {
  source('R/rtf_cell_utils.R')
  source('R/rtf_row_parser.R')
} else if (file.exists('rtf_cell_utils.R')) {
  source('rtf_cell_utils.R')
  source('rtf_row_parser.R')
} else {
  # In package context, these should already be loaded
}

# Enhanced parse function that properly extracts titles
parse_rtf_with_titles <- function(rtf_file, encoding = NULL) {
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
  
  # Initialize results
  result <- list(
    title_rows = list(),
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
        cells <- extract_cells_from_row_enhanced(row_data$content)
        
        # Determine row type and state
        if (grepl("\\trhdr", row_data$content, fixed = TRUE)) {
          # Header row - check if this is a title row or column header
          if (length(cells) == 1) {
            if (cells[[1]]$width > 10000) {
              # This is likely a title row (full-width, single cell)
              result$title_rows[[length(result$title_rows) + 1]] <- cells[[1]]$text
              if (state == 0) state <- 1
            }
          } else {
            # Regular column header
            state <- 1
            result$header_rows[[length(result$header_rows) + 1]] <- list(
              cells = cells,
              row_index = i
            )
          }
        } else if (state == 0) {
          # Pre-header content - check for title rows
          if (length(cells) == 1 && nchar(trimws(cells[[1]]$text)) > 0) {
            # Check if this looks like a title (contains "Table" or is centered)
            cell_text <- cells[[1]]$text
            if (grepl("Table\\s+[0-9]", cell_text) || grepl("\\\\qc", row_data$content)) {
              # This is a title row
              result$title_rows[[length(result$title_rows) + 1]] <- cell_text
            }
          }
        } else if (state >= 1) {
          # Check for separator row pattern
          is_separator <- grepl("\\\\sl-5", row_data$content, fixed = TRUE) &&
                         grepl("\\\\brdrw15", row_data$content, fixed = TRUE) &&
                         length(cells) == 1
          
          if (state == 2 && is_separator) {
            # Transition to footnotes
            state <- 3
            # Don't add separator as data
          } else if (state == 3) {
            # In footnote state
            if (length(cells) > 0) {
              for (j in 1:length(cells)) {
                if (nchar(trimws(cells[[j]]$text)) > 0) {
                  result$footnotes <- c(result$footnotes, cells[[j]]$text)
                }
              }
            }
          } else {
            # Regular data row
            if (state == 1 && length(cells) > 1) {
              # First multi-cell row after header marks data start
              state <- 2
            }
            
            # Add as data row
            if (length(cells) > 0 && any(sapply(cells, function(c) nchar(trimws(c$text)) > 0))) {
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
    }
    
    i <- i + 1
  }
  
  return(result)
}

# Process a single RTF file
process_rtf_file <- function(rtf_path, json_path, encoding = NULL) {
  cat('\n========================================\n')
  cat('Processing:', basename(rtf_path), '\n')
  cat('========================================\n')
  
  # Parse with enhanced parser - try with encoding if specified
  result <- tryCatch({
    parse_rtf_with_titles(rtf_path, encoding = encoding)
  }, error = function(e) {
    if (is.null(encoding)) {
      # If no encoding specified and it failed, try with latin1
      message("Trying with latin1 encoding...")
      parse_rtf_with_titles(rtf_path, encoding = "latin1")
    } else {
      stop(e)
    }
  })
  
  # Extract and combine title
  title <- NULL
  if (length(result$title_rows) > 0) {
    # Join multiple title rows with newline as shown in example
    title <- paste(unique(trimws(result$title_rows)), collapse = "\n")
  }
  cat('Title:', ifelse(is.null(title), "Not found", title), '\n')
  
  # Extract footnotes
  footnotes <- result$footnotes
  cat('Found', length(footnotes), 'footnotes\n')
  
  # Build header structure from column headers
  header_structure <- list(
    colgroup1_levels = character(),
    colgroup2_levels = character()
  )
  
  # Track which positions have empty headers (spacer columns)
  empty_header_positions <- c()
  
  # Extract header information from header rows
  # For multi-level headers, we need to process multiple header rows
  if (length(result$header_rows) > 0) {
    # First, check if we have multi-level headers by comparing header rows to data columns
    n_data_cols <- 0
    if (length(result$data_rows) > 0) {
      # Get max number of data columns from first few data rows
      for (i in 1:min(5, length(result$data_rows))) {
        n_cols <- length(result$data_rows[[i]]$cells) - 1  # Exclude variable column
        if (n_cols > n_data_cols) n_data_cols <- n_cols
      }
    }
    
    # IMPORTANT: Only process the FIRST occurrence of headers
    # Multi-page documents repeat headers, but we only want the first set
    header_level <- 1
    headers_processed <- 0
    max_headers_to_process <- 2  # We expect at most 2 levels of headers
    
    for (header_idx in seq_along(result$header_rows)) {
      if (headers_processed >= max_headers_to_process) break
      
      header_row <- result$header_rows[[header_idx]]
      # Skip if only one cell (likely title row)
      if (length(header_row$cells) > 1) {
        n_header_cells <- length(header_row$cells) - 1  # Exclude first cell
        
        if (header_level == 1) {
          # First header row - this should be treatment groups
          # Skip the first cell (variable column header)
          if (length(header_row$cells) > 1) {
            # Treatment groups start from cell 2
            # But check if this looks like a treatment header first
            first_data_cell <- if(length(header_row$cells) > 1) header_row$cells[[2]]$text else ""
            if (grepl("Arm|Treatment|Group", first_data_cell, ignore.case = TRUE) || 
                length(header_row$cells) < 5) {
              # This is a treatment group header (few cells spanning multiple columns)
              # Store all cells including empty ones to maintain position mapping
              all_headers <- character()
              for (i in 2:length(header_row$cells)) {
                cell <- header_row$cells[[i]]
                header_text <- trimws(gsub("\\s+", " ", cell$text))
                all_headers <- c(all_headers, header_text)
              }
              # Store the complete header structure for later mapping
              header_structure$colgroup1_levels <- all_headers
              header_level <- 2
              headers_processed <- headers_processed + 1
            }
          }
        } else if (header_level == 2) {
          # Second header row - this should be grade levels
          # We need to map these to the treatment groups
          grade_headers <- character()
          for (i in 2:length(header_row$cells)) {
            cell <- header_row$cells[[i]]
            header_text <- trimws(gsub("\\s+", " ", cell$text))
            grade_headers <- c(grade_headers, header_text)
          }
          
          # Now create the full mapping
          # We need to map grade headers to treatment groups accounting for empty spacer cells
          final_colgroup1 <- character()
          final_colgroup2 <- character()
          
          # First, extract just the non-empty treatment groups and their positions
          treatment_groups <- character()
          treatment_positions <- numeric()
          
          for (i in seq_along(header_structure$colgroup1_levels)) {
            if (nchar(header_structure$colgroup1_levels[i]) > 0) {
              treatment_groups <- c(treatment_groups, header_structure$colgroup1_levels[i])
              treatment_positions <- c(treatment_positions, i)
            }
          }
          
          # Now map grade headers to treatments
          treatment_idx <- 1
          current_treatment <- treatment_groups[1]
          grade_count <- 0
          
          for (i in seq_along(grade_headers)) {
            grade <- grade_headers[i]
            
            if (nchar(grade) == 0) {
              # Empty grade cell - check if we should move to next treatment
              # Move to next treatment after we've seen 3 grades
              if (grade_count == 3 && treatment_idx < length(treatment_groups)) {
                treatment_idx <- treatment_idx + 1
                current_treatment <- treatment_groups[treatment_idx]
                grade_count <- 0
              }
            } else {
              # Real grade - add mapping
              final_colgroup1 <- c(final_colgroup1, current_treatment)
              final_colgroup2 <- c(final_colgroup2, grade)
              grade_count <- grade_count + 1
            }
          }
          
          header_structure$colgroup1_levels <- final_colgroup1
          header_structure$colgroup2_levels <- final_colgroup2
          headers_processed <- headers_processed + 1
          break  # We're done with headers
        }
      }
    }
    
    # Truncate headers to match actual data columns if needed
    if (n_data_cols > 0) {
      if (length(header_structure$colgroup1_levels) > n_data_cols) {
        header_structure$colgroup1_levels <- header_structure$colgroup1_levels[1:n_data_cols]
      }
      if (length(header_structure$colgroup2_levels) > n_data_cols) {
        header_structure$colgroup2_levels <- header_structure$colgroup2_levels[1:n_data_cols]
      }
    }
  }
  
  # Identify empty columns by checking both colgroup levels
  if (length(header_structure$colgroup2_levels) > 0) {
    # Remove columns where colgroup2 is empty (these are spacer columns)
    non_empty_mask <- nchar(header_structure$colgroup2_levels) > 0
    header_structure$colgroup1_levels <- header_structure$colgroup1_levels[non_empty_mask]
    header_structure$colgroup2_levels <- header_structure$colgroup2_levels[non_empty_mask]
    
    # Track which positions were removed (1-based indexing for data cells)
    empty_header_positions <- which(!non_empty_mask)
    
    if (length(empty_header_positions) > 0) {
      cat('Removed', length(empty_header_positions), 'empty spacer columns\n')
    }
  }
  
  cat('Processing', length(result$data_rows), 'data rows...\n')
  cat('colgroup1_levels:', length(header_structure$colgroup1_levels), 'values:', paste(header_structure$colgroup1_levels, collapse="|"), '\n')
  cat('colgroup2_levels:', length(header_structure$colgroup2_levels), 'values:', paste(header_structure$colgroup2_levels, collapse="|"), '\n')
  
  # Process all data rows including hierarchical structure
  ard_data <- list()
  current_parent <- NULL
  current_rowgroup1 <- NULL
  current_rowgroup1_level <- NULL
  
  for (row_idx in seq_along(result$data_rows)) {
    row <- result$data_rows[[row_idx]]
    
    if (length(row$cells) == 0) next
    
    # Extract variable name
    variable <- row$cells[[1]]$text
    # Check for tildes - RTF uses \~ for escaped tildes
    is_indented <- grepl("^~~", variable)
    clean_variable <- gsub("^~~", "", variable)
    clean_variable <- trimws(clean_variable)
    
    # Skip any footnote-like content that made it to data
    # Use structural check: single/dual cell rows with very long text
    if (length(row$cells) <= 2) {
      if (nchar(variable) > 100 && !grepl("^~", variable)) {
        footnotes <- c(footnotes, variable)
        next
      }
      # Also skip "Program Source" rows - these are footnotes
      if (grepl("^Program Source:", variable)) {
        if (length(row$cells) > 1) {
          footnotes <- c(footnotes, paste(variable, row$cells[[2]]$text))
        } else {
          footnotes <- c(footnotes, variable)
        }
        next
      }
    }
    
    if (length(row$cells) > 1) {
      data_cells <- row$cells[2:length(row$cells)]
      
      # Check if this is a parent row (no real statistics)
      has_stats <- FALSE
      for (i in 1:length(data_cells)) {
        cell_text <- trimws(data_cells[[i]]$text)
        if (nchar(cell_text) > 0 && 
            !grepl("^\\\\~+$", cell_text) && 
            grepl("[0-9]", cell_text)) {
          has_stats <- TRUE
          break
        }
      }
      
      # Handle different row types based on your example
      if (!has_stats && !is_indented) {
        # This is a parent label row (no stats)
        current_parent <- clean_variable
        next  # Don't include parent rows in data
      } else {
        # Determine variable name
        if (is_indented && !is.null(current_parent)) {
          # Indented row inherits from parent label (use newline as separator per example)
          final_variable <- paste(current_parent, clean_variable, sep = "\n ")
        } else {
          final_variable <- clean_variable
          if (!is_indented) current_parent <- NULL  # Reset parent when we hit non-indented row
        }
        
        # Process data cells
        col_index <- 0  # Index into the filtered header arrays
        for (i in 1:length(data_cells)) {
          cell <- data_cells[[i]]
          
          # Skip cells that correspond to empty header columns
          if (i %in% empty_header_positions) {
            next
          }
          
          # Skip spacer cells (~~~)
          cell_text <- trimws(cell$text)
          if (grepl("^~~~+$", cell_text) || grepl("^\\\\~+$", cell_text)) {
            next
          }
          
          # Increment column index for non-empty columns
          col_index <- col_index + 1
          
          # Skip if we've run out of header mappings (data beyond expected columns)
          if (col_index > length(header_structure$colgroup1_levels)) {
            cat("WARNING: Data cell", i, "exceeds header columns. Text:", substr(cell$text, 1, 20), "...\n")
            next
          }
          
          ard_record <- list(
            variable = final_variable,
            stat = cell$text,
            colgroup1 = "colgroup1",
            colgroup1_level = header_structure$colgroup1_levels[col_index]
          )
          
          # Only add colgroup2 fields if we have colgroup2 headers
          if (length(header_structure$colgroup2_levels) > 0) {
            ard_record$colgroup2 <- "colgroup2"
            ard_record$colgroup2_level <- if(col_index <= length(header_structure$colgroup2_levels)) 
                                            header_structure$colgroup2_levels[col_index] 
                                          else ""
          }
          
          ard_data[[length(ard_data) + 1]] <- ard_record
        }
      }
    }
  }
  
  # Convert to data frame
  if (length(ard_data) > 0) {
    ard_df <- do.call(rbind, lapply(ard_data, as.data.frame, stringsAsFactors = FALSE))
    
    # Remove spacer rows
    ard_df <- ard_df[ard_df$stat != "\\~\\~\\~", ]
  } else {
    ard_df <- data.frame()
  }
  
  cat('Generated', nrow(ard_df), 'ARD records\n')
  
  # Deduplicate footnotes
  unique_footnotes <- unique(footnotes)
  
  # Save final JSON with complete metadata
  json_output <- list(
    data = ard_df,
    metadata = list(
      source_file = basename(rtf_path),
      parser = 'RTF row-based parser with hierarchical support',
      timestamp = Sys.time(),
      title = title,
      footnotes = unique_footnotes
    )
  )
  
  jsonlite::write_json(json_output, json_path, auto_unbox = TRUE, pretty = TRUE)
  cat('Saved to:', basename(json_path), '\n')
}