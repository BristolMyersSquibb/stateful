#' State Parser (Stage 1) - Structural RTF Parsing
#'
#' Stage 1 parser that uses state-based RTF parsing to extract raw structural 
#' data without pattern interpretation. Raw statistics are preserved exactly.
#'
#' @name state-parser
NULL

#' Parse RTF using state-based approach (Stage 1)
#'
#' Extracts table structure using RTF state machine. Returns raw statistics
#' without pattern interpretation for manual review and double programming.
#'
#' @param rtf_file Path to RTF file
#' @return List with structured ARD data and metadata
#' @export
state_parser_stage1 <- function(rtf_file) {
  message("State Parser Stage 1: Extracting structural data...")
  
  tryCatch({
    # Use existing state-based parser
    state_data <- parse_rtf_table_states(rtf_file)
    
    if (is.null(state_data) || is.null(state_data$table)) {
      warning("Could not parse RTF file using state parser - returning empty result")
      return(list(
        data = data.frame(
          variable = character(), variable_level = character(),
          group1 = character(), raw_stat = character(), 
          source_row = integer(), source_col = integer(),
          state_source = character()
        ),
        metadata = list(
          processing_date = Sys.time(),
          stage = "state_parser_stage1", 
          method = "rtf_state_machine",
          status = "failed",
          error = "No table data found"
        ),
        stage = "state_parser_stage1"
      ))
    }
    
    # Convert to structured ARD with raw stats
    structured_ard <- convert_state_data_to_ard(state_data)
    
    message("Stage 1 complete: ", nrow(structured_ard$data), " rows extracted")
    structured_ard
    
  }, error = function(e) {
    warning("State Parser Stage 1 failed: ", e$message)
    return(list(
      data = data.frame(
        variable = character(), variable_level = character(),
        group1 = character(), raw_stat = character(), 
        source_row = integer(), source_col = integer(),
        state_source = character()
      ),
      metadata = list(
        processing_date = Sys.time(),
        stage = "state_parser_stage1",
        method = "rtf_state_machine", 
        status = "error",
        error = e$message
      ),
      stage = "state_parser_stage1"
    ))
  })
}

#' Convert state parser data to structured ARD
#'
#' Processes state parser output to create flat ARD structure suitable
#' for manual review and export.
#'
#' @param state_data Output from parse_rtf_table_states()
#' @keywords internal
convert_state_data_to_ard <- function(state_data) {
  
  # Extract treatment groups from header analysis
  header_groups <- extract_treatment_groups(state_data$header)
  
  # Get raw hierarchy for 2-level grouping
  raw_hierarchy <- state_data$header$analysis$column_hierarchy
  
  # Process table rows
  ard_rows <- process_table_rows(state_data$table$rows, header_groups, raw_hierarchy)
  
  # Create metadata
  metadata <- create_stage1_metadata(state_data)
  
  list(
    data = ard_rows,
    metadata = metadata,
    stage = "state_parser_stage1"
  )
}

#' Extract treatment groups from header analysis
#' @keywords internal
extract_treatment_groups <- function(header_data) {
  if (is.null(header_data$analysis$column_hierarchy)) {
    return(data.frame(column_index = integer(), group_name = character()))
  }
  
  hierarchy <- header_data$analysis$column_hierarchy
  
  # Check if we have nested headers (both level_1 and level_2)
  # AND if level_1 has meaningful spanning (not just repeated table titles)
  has_nested <- !is.null(hierarchy$level_1) && !is.null(hierarchy$level_2) &&
                any(hierarchy$level_1 != "" & !is.na(hierarchy$level_1)) &&
                any(hierarchy$level_2 != "" & !is.na(hierarchy$level_2)) &&
                has_meaningful_spanning(hierarchy)
  
  if (has_nested) {
    # Build hierarchical group names using position-based inheritance
    group_names <- build_nested_group_names(hierarchy)
  } else {
    # Use level_2 if available, otherwise level_1 (existing logic)
    group_names <- if (!is.null(hierarchy$level_2)) {
      hierarchy$level_2
    } else {
      hierarchy$level_1
    }
  }
  
  data.frame(
    column_index = hierarchy$column_index,
    group_name = group_names,
    stringsAsFactors = FALSE
  )
}

#' Build nested group names using position-based inheritance
#' @keywords internal
build_nested_group_names <- function(hierarchy) {
  group_names <- character(nrow(hierarchy))
  
  for (i in seq_len(nrow(hierarchy))) {
    level_1 <- hierarchy$level_1[i]
    level_2 <- hierarchy$level_2[i]
    
    # Clean the level names
    level_1_clean <- if (!is.null(level_1) && level_1 != "" && !is.na(level_1)) {
      trimws(level_1)
    } else {
      ""
    }
    
    level_2_clean <- if (!is.null(level_2) && level_2 != "" && !is.na(level_2)) {
      trimws(level_2)
    } else {
      ""
    }
    
    # If level_1 is empty, inherit from previous non-empty level_1 using position
    if (level_1_clean == "" && i > 1) {
      # Find the spanning parent by looking at positions
      current_pos <- hierarchy$left_pos[i]
      
      # Look backwards for a level_1 that spans this position
      for (j in (i-1):1) {
        if (hierarchy$level_1[j] != "" && !is.na(hierarchy$level_1[j])) {
          prev_left <- hierarchy$left_pos[j]
          prev_right <- hierarchy$right_pos[j]
          
          # Check if current column is within the span of this level_1
          if (current_pos >= prev_left && current_pos <= prev_right) {
            level_1_clean <- trimws(hierarchy$level_1[j])
            break
          }
        }
      }
    }
    
    # Construct the final group name
    if (level_1_clean != "" && level_2_clean != "") {
      group_names[i] <- paste(level_1_clean, level_2_clean, sep = " - ")
    } else if (level_1_clean != "") {
      group_names[i] <- level_1_clean
    } else if (level_2_clean != "") {
      group_names[i] <- level_2_clean
    } else {
      group_names[i] <- paste0("Column_", hierarchy$column_index[i])
    }
  }
  
  return(group_names)
}

#' Check if hierarchy has meaningful spanning (not just repeated table titles)
#' @keywords internal
has_meaningful_spanning <- function(hierarchy) {
  # Check if level_1 has different values or shows spanning behavior
  level_1_values <- hierarchy$level_1[!is.na(hierarchy$level_1) & hierarchy$level_1 != ""]
  
  # If all level_1 values are the same (repeated table title), it's not meaningful spanning
  if (length(unique(level_1_values)) <= 1) {
    return(FALSE)
  }
  
  # Check for actual spanning: same level_1 value spans multiple level_2 columns
  for (val in unique(level_1_values)) {
    indices <- which(hierarchy$level_1 == val)
    if (length(indices) > 1) {
      # Check if these columns have different level_2 values (true spanning)
      level_2_vals <- hierarchy$level_2[indices]
      if (length(unique(level_2_vals[!is.na(level_2_vals) & level_2_vals != ""])) > 1) {
        return(TRUE)
      }
    }
  }
  
  return(FALSE)
}

#' Process table rows to create flat ARD structure
#' @keywords internal
process_table_rows <- function(table_rows, header_groups, raw_hierarchy = NULL) {
  
  ard_list <- list()
  
  # Extract variable information with hierarchy - with error handling
  variable_data <- tryCatch({
    extract_variable_hierarchy(table_rows)
  }, error = function(e) {
    warning("Hierarchy extraction failed: ", e$message)
    data.frame(
      row_index = integer(), variable_text = character(),
      variable_level = character(), has_values = logical(),
      is_indented = logical()
    )
  })
  
  for (i in seq_along(table_rows)) {
    row <- table_rows[[i]]
    
    if (is.null(row$cells) || length(row$cells) == 0) next
    
    # Get variable name from first cell
    if (length(row$cells) < 1) next
    variable_name <- row$cells[[1]]$text
    if (is.null(variable_name) || variable_name == "") next
    
    # Get hierarchical variable path
    var_info <- variable_data[variable_data$row_index == i, ]
    if (nrow(var_info) > 0) {
      variable_level <- var_info$variable_level[1]
    } else {
      variable_level <- clean_variable_text(variable_name)
    }
    
    # Process each data cell (skip first cell which is variable name)
    if (length(row$cells) > 1) {
      for (j in 2:length(row$cells)) {
        if (j > length(row$cells)) next
        
        cell <- row$cells[[j]]
        if (is.null(cell) || is.null(cell$text) || cell$text == "") next
        
        # Get group information
        group_info <- get_group_info(j, header_groups, raw_hierarchy)
        
        # Create ARD row with all possible columns
        ard_row <- data.frame(
          variable = variable_name,
          variable_level = variable_level,
          group1 = if(is.null(group_info$group1)) NA_character_ else group_info$group1,
          group1_level = if(is.null(group_info$group1_level)) NA_character_ else group_info$group1_level,
          group2 = if(is.null(group_info$group2)) NA_character_ else group_info$group2,
          group2_level = if(is.null(group_info$group2_level)) NA_character_ else group_info$group2_level,
          group3 = if(is.null(group_info$group3)) NA_character_ else group_info$group3,
          raw_stat = cell$text,
          source_row = row$index,
          source_col = j,
          state_source = "table_body",
          stringsAsFactors = FALSE
        )
        
        ard_list[[length(ard_list) + 1]] <- ard_row
      }
    }
  }
  
  if (length(ard_list) == 0) {
    return(data.frame(
      variable = character(), variable_level = character(),
      group1 = character(), raw_stat = character(), source_row = integer(),
      source_col = integer(), state_source = character()
    ))
  }
  
  result <- do.call(rbind, ard_list)
  
  # Only include group2 and group3 columns if they contain non-NA values
  has_group2 <- any(!is.na(result$group2) & result$group2 != "")
  has_group3 <- any(!is.na(result$group3) & result$group3 != "")
  
  if (!has_group2) {
    result$group2 <- NULL
  }
  if (!has_group3) {
    result$group3 <- NULL
  }
  
  result
}

#' Get group information for column index
#' @keywords internal
get_group_info <- function(column_index, header_groups, raw_hierarchy = NULL) {
  
  # Try to use raw hierarchy for proper 2-level grouping
  if (!is.null(raw_hierarchy)) {
    matching_hierarchy <- raw_hierarchy[raw_hierarchy$column_index == column_index, ]
    
    if (nrow(matching_hierarchy) > 0) {
      level_1 <- matching_hierarchy$level_1[1]
      level_2 <- matching_hierarchy$level_2[1]
      
      # Clean the levels
      level_1_clean <- if (!is.null(level_1) && level_1 != "" && !is.na(level_1)) {
        clean_group_text(level_1)
      } else {
        ""
      }
      
      level_2_clean <- if (!is.null(level_2) && level_2 != "" && !is.na(level_2)) {
        clean_group_text(level_2)
      } else {
        ""
      }
      
      # If level_1 is empty, try to inherit from previous columns
      if (level_1_clean == "" && nrow(raw_hierarchy) > 1) {
        current_pos <- matching_hierarchy$left_pos[1]
        
        # Look backwards for spanning level_1
        for (j in 1:nrow(raw_hierarchy)) {
          if (raw_hierarchy$level_1[j] != "" && !is.na(raw_hierarchy$level_1[j])) {
            prev_left <- raw_hierarchy$left_pos[j]
            prev_right <- raw_hierarchy$right_pos[j]
            
            if (current_pos >= prev_left && current_pos <= prev_right) {
              level_1_clean <- clean_group_text(raw_hierarchy$level_1[j])
              break
            }
          }
        }
      }
      
      # Create proper ARD structure with generic names
      if (level_1_clean != "" && level_2_clean != "") {
        return(list(
          group1 = "TRT",
          group1_level = level_1_clean,
          group2 = "COL2", 
          group2_level = level_2_clean,
          group3 = NA_character_
        ))
      } else if (level_1_clean != "") {
        return(list(
          group1 = "TRT",
          group1_level = level_1_clean,
          group2 = NA_character_,
          group3 = NA_character_
        ))
      }
    }
  }
  
  # Fallback to original logic
  matching_group <- header_groups[header_groups$column_index == column_index, ]
  
  if (nrow(matching_group) > 0) {
    group_name <- clean_group_text(matching_group$group_name[1])
  } else {
    group_name <- paste0("Column_", column_index)
  }
  
  list(
    group1 = group_name,
    group2 = NA_character_,
    group3 = NA_character_
  )
}

#' Clean group text by removing RTF artifacts
#' @keywords internal
clean_group_text <- function(text) {
  if (is.null(text) || is.na(text) || text == "") return("")
  
  # Remove curly braces and clean up spacing
  cleaned <- gsub("\\{", "", text)
  cleaned <- gsub("\\}", "", cleaned)
  cleaned <- gsub("\\s+", " ", cleaned)
  cleaned <- trimws(cleaned)
  
  return(cleaned)
}

#' Clean variable text
#' @keywords internal
clean_variable_text <- function(text) {
  if (is.null(text)) return("")
  
  # Convert to character if not already (handles factors, numbers, etc.)
  if (!is.character(text)) {
    text <- as.character(text)
  }
  
  # Handle empty or NA values
  if (length(text) == 0 || is.na(text) || text == "NA") return("")
  
  # Remove RTF escape sequences including curly braces
  cleaned <- gsub("\\{\\s*\\}", " ", text)  # Remove { } sequences
  cleaned <- gsub("\\\\~", "", cleaned)     # Remove RTF non-breaking spaces
  cleaned <- gsub("\\\\[\\\\]", " ", cleaned)  # Remove other RTF escapes
  cleaned <- gsub("\\s+", " ", cleaned)     # Collapse multiple spaces
  cleaned <- trimws(cleaned)
  cleaned
}

#' Create Stage 1 metadata
#' @keywords internal
create_stage1_metadata <- function(state_data) {
  # Create basic metadata
  metadata <- list(
    processing_date = Sys.time(),
    stage = "state_parser_stage1",
    method = "rtf_state_machine"
  )
  
  # Extract all pre-header text lines exactly as they are
  if (!is.null(state_data$pre_header) && !is.null(state_data$pre_header$text)) {
    lines <- state_data$pre_header$text
    # Add each line as title1, title2, etc. (including empty lines)
    for (i in seq_along(lines)) {
      metadata[[paste0("title", i)]] <- lines[i]
    }
  }
  
  # If no pre-header, check first few table rows
  if (is.null(metadata$title1) && !is.null(state_data$table$rows)) {
    line_count <- 0
    for (i in 1:min(5, length(state_data$table$rows))) {
      row <- state_data$table$rows[[i]]
      if (!is.null(row$cells) && length(row$cells) > 0) {
        text <- row$cells[[1]]$text
        if (!is.null(text)) {
          line_count <- line_count + 1
          metadata[[paste0("title", line_count)]] <- text
        }
      }
    }
  }
  
  # Add footnotes exactly as they are
  metadata$footnotes <- extract_footnotes_from_state_data(state_data)
  
  metadata
}

#' Extract title from state data
#' @keywords internal
extract_title_from_state_data <- function(state_data) {
  # Try to find title in pre_header section
  if (!is.null(state_data$pre_header) && !is.null(state_data$pre_header$text)) {
    # Look for lines that might be titles (non-empty, not just protocol info)
    title_lines <- state_data$pre_header$text[state_data$pre_header$text != ""]
    
    # Common patterns: Table X.X.X or descriptive titles
    for (line in title_lines) {
      if (grepl("^Table\\s+[0-9\\.]+", line, ignore.case = TRUE) ||
          grepl("Summary|Analysis|Report", line, ignore.case = TRUE)) {
        return(line)
      }
    }
    
    # Return first non-empty line if no pattern matches
    if (length(title_lines) > 0) {
      return(title_lines[1])
    }
  }
  
  # Try table rows if no pre_header
  if (!is.null(state_data$table) && !is.null(state_data$table$rows)) {
    # Sometimes titles are in the first few table rows
    for (i in 1:min(5, length(state_data$table$rows))) {
      row <- state_data$table$rows[[i]]
      if (!is.null(row$cells) && length(row$cells) > 0) {
        text <- row$cells[[1]]$text
        if (!is.null(text) && grepl("^Table\\s+[0-9\\.]+", text, ignore.case = TRUE)) {
          return(text)
        }
      }
    }
  }
  
  "No title found"
}

#' Extract footnotes from state data
#' @keywords internal
extract_footnotes_from_state_data <- function(state_data) {
  if (!is.null(state_data$footnotes$text)) {
    return(state_data$footnotes$text)
  }
  character(0)
}

#' Extract variable hierarchy from table rows
#' @keywords internal
extract_variable_hierarchy <- function(table_rows) {
  
  variable_info <- list()
  current_parent <- ""
  
  for (i in seq_along(table_rows)) {
    row <- table_rows[[i]]
    
    if (is.null(row$cells) || length(row$cells) == 0) next
    if (length(row$cells) < 1) next
    
    variable_text <- row$cells[[1]]$text
    if (is.null(variable_text) || variable_text == "") next
    
    # Clean the variable text
    clean_text <- clean_variable_text(variable_text)
    
    # Simple rule: Check if this row has values or not
    has_values <- FALSE
    if (length(row$cells) > 1) {
      # Check if any data cells have non-empty values
      for (j in 2:length(row$cells)) {
        if (!is.null(row$cells[[j]]$text) && trimws(row$cells[[j]]$text) != "") {
          has_values <- TRUE
          break
        }
      }
    }
    
    # Check if variable starts with spaces or tildes (indicating indentation)
    # Need to check the raw field since tildes are removed from text
    raw_text <- if (!is.null(row$cells[[1]]$raw)) row$cells[[1]]$raw else variable_text
    is_indented <- grepl("^\\s+", variable_text) || grepl("\\\\~", raw_text)
    
    # Apply simple hierarchical rule
    if (!has_values && !is_indented) {
      # This is a parent category (no values, not indented)
      current_parent <- clean_text
      variable_level <- clean_text
    } else if (is_indented && current_parent != "") {
      # This is an indented child
      variable_level <- paste(current_parent, clean_text, sep = " - ")
    } else {
      # Regular variable or reset parent
      variable_level <- clean_text
      if (!has_values) {
        current_parent <- clean_text
      }
    }
    
    variable_info[[length(variable_info) + 1]] <- data.frame(
      row_index = i,
      variable_text = clean_text,
      variable_level = variable_level,
      has_values = has_values,
      is_indented = is_indented,
      stringsAsFactors = FALSE
    )
  }
  
  if (length(variable_info) == 0) {
    return(data.frame(
      row_index = integer(), variable_text = character(),
      variable_level = character(), has_values = logical(),
      is_indented = logical()
    ))
  }
  
  do.call(rbind, variable_info)
}

#' Extract RTF indentation level from cell
#' @keywords internal
extract_cell_indentation <- function(cell) {
  if (is.null(cell)) {
    return(0)
  }
  
  # Check for RTF indentation in multiple possible locations
  rtf_sources <- c(cell$rtf_content, cell$content, cell$raw_content)
  
  for (rtf_content in rtf_sources) {
    if (!is.null(rtf_content)) {
      rtf_text <- as.character(rtf_content)
      
      # Look for RTF indentation markers (in order of specificity)
      if (grepl("\\\\li1080\\b", rtf_text)) return(3)
      if (grepl("\\\\li720\\b", rtf_text)) return(2)  
      if (grepl("\\\\li360\\b", rtf_text)) return(1)
      if (grepl("\\\\li0\\b", rtf_text)) return(0)
      
      # Look for generic \\li followed by number
      li_match <- regexpr("\\\\li(\\d+)", rtf_text, perl = TRUE)
      if (li_match[1] > 0) {
        li_value <- as.numeric(substr(rtf_text, 
                                     attr(li_match, "capture.start"), 
                                     attr(li_match, "capture.start") + attr(li_match, "capture.length") - 1))
        if (!is.na(li_value)) {
          return(min(li_value %/% 360, 3))  # 360 twips per indent level
        }
      }
    }
  }
  
  # Fallback: detect by leading spaces or RTF tilde patterns in text
  if (!is.null(cell$text)) {
    text <- as.character(cell$text)
    
    # RTF uses \~ for non-breaking spaces as indentation
    tilde_count <- length(gregexpr("\\\\~", text)[[1]])
    if (tilde_count > 0 && tilde_count != -1) {
      return(min(tilde_count %/% 2, 3))  # Every 2 tildes = 1 indent level
    }
    
    # Look for leading spaces or specific indentation patterns
    if (grepl("^\\s{4,}", text)) return(2)  # 4+ spaces = level 2
    if (grepl("^\\s{2,3}", text)) return(1)  # 2-3 spaces = level 1  
    if (grepl("^\\s", text)) return(1)       # Any leading space = level 1
  }
  
  return(0)
}