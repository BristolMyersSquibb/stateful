#' RTF to ARD Conversion Utilities
#'
#' Convert enhanced RTF parsing results to Analysis Results Data (ARD) format
#' with support for multi-level column groups and row hierarchies
#'
#' @name rtf-to-ard
NULL

#' Convert RTF parsing results to ARD format
#'
#' Transform the enhanced output from parse_rtf_table_states into standard ARD format
#' with proper column group mappings and hierarchy handling
#'
#' @param rtf_result List output from parse_rtf_table_states
#' @param options List of configuration options (optional)
#' @return Data frame in ARD format
#' @export
rtf_to_ard <- function(rtf_result, options = NULL) {
  if (is.null(rtf_result$data) || nrow(rtf_result$data) == 0) {
    return(create_empty_ard())
  }
  
  table_data <- rtf_result$data
  
  # Convert to ARD format
  ard_data <- convert_to_ard_format(table_data, options)
  
  # Parse statistics if needed
  ard_data <- parse_ard_statistics(ard_data)
  
  # Add metadata columns
  ard_data <- add_ard_metadata(ard_data, rtf_result$metadata)
  
  ard_data
}

#' Convert table data to ARD format
#'
#' Transform enhanced table data with column groups into standard ARD structure
#'
#' @param table_data Data frame from enhanced parser
#' @param options Configuration options
#' @return Data frame in ARD format
#' @keywords internal
convert_to_ard_format <- function(table_data, options = NULL) {
  if (nrow(table_data) == 0) {
    return(create_empty_ard())
  }
  
  # Apply hierarchy detection first
  table_data <- detect_row_hierarchy(table_data)
  
  # Initialize ARD structure
  ard_data <- data.frame(
    colgroup1 = table_data$colgroup1 %||% "",
    colgroup1_level = table_data$colgroup1_level %||% "",
    colgroup2 = table_data$colgroup2 %||% "",
    colgroup2_level = table_data$colgroup2_level %||% "",
    colgroup3 = table_data$colgroup3 %||% "",
    colgroup3_level = table_data$colgroup3_level %||% "",
    rowgroup1 = table_data$rowgroup1 %||% "",
    rowgroup1_level = table_data$rowgroup1_level %||% "",
    rowgroup2 = table_data$rowgroup2 %||% "",
    rowgroup2_level = table_data$rowgroup2_level %||% "",
    variable = table_data$variable,
    variable_level = table_data$variable_level %||% table_data$variable,
    stat = table_data$raw_stat,
    stat_name = "raw",
    stat_label = "Raw Statistic",
    stringsAsFactors = FALSE
  )
  
  # Clean up empty statistics
  ard_data <- ard_data[
    !is.na(ard_data$stat) & 
    trimws(ard_data$stat) != "" & 
    ard_data$stat != "\\\\\\\\\\\\~" &
    ard_data$stat != "~~~",
  ]
  
  ard_data
}

#' Parse ARD statistics into component parts
#'
#' Split combined statistics like "137 (39.0%)" into separate rows
#' for n and pct statistics
#'
#' @param ard_data Data frame in ARD format
#' @return Enhanced ARD data with parsed statistics
#' @keywords internal
parse_ard_statistics <- function(ard_data) {
  if (nrow(ard_data) == 0) {
    return(ard_data)
  }
  
  # Initialize list to collect parsed rows
  parsed_rows <- list()
  
  for (i in seq_len(nrow(ard_data))) {
    row <- ard_data[i, ]
    stat_value <- trimws(row$stat)
    
    # Try to parse common patterns
    parsed_stats <- parse_statistic_value(stat_value)
    
    if (length(parsed_stats) == 0) {
      # No parsing possible, keep original
      parsed_rows[[length(parsed_rows) + 1]] <- row
    } else {
      # Create separate rows for each parsed statistic
      for (stat in parsed_stats) {
        new_row <- row
        new_row$stat <- stat$value
        new_row$stat_name <- stat$name
        new_row$stat_label <- stat$label
        
        parsed_rows[[length(parsed_rows) + 1]] <- new_row
      }
    }
  }
  
  # Combine all rows
  if (length(parsed_rows) > 0) {
    do.call(rbind, parsed_rows)
  } else {
    ard_data
  }
}

#' Parse individual statistic value
#'
#' Attempt to parse a statistic string into component parts
#'
#' @param stat_value Character string with statistic
#' @return List of parsed statistics with value, name, and label
#' @keywords internal
parse_statistic_value <- function(stat_value) {
  if (is.null(stat_value) || nchar(trimws(stat_value)) == 0) {
    return(list())
  }
  
  parsed_stats <- list()
  
  # Pattern 1: "n (pct%)" format like "137 (39.0%)"
  pattern1 <- "^\\s*(\\d+(?:\\.\\d+)?)\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*%?\\s*\\)\\s*$"
  if (grepl(pattern1, stat_value)) {
    matches <- regmatches(stat_value, regexec(pattern1, stat_value))[[1]]
    if (length(matches) >= 3) {
      # Extract n and percentage
      n_value <- matches[2]
      pct_value <- matches[3]
      
      parsed_stats[[1]] <- list(
        value = n_value,
        name = "n",
        label = "Count"
      )
      
      parsed_stats[[2]] <- list(
        value = pct_value,
        name = "pct",
        label = "Percentage"
      )
      
      return(parsed_stats)
    }
  }
  
  # Pattern 2: Simple percentage like "39.0%"
  pattern2 <- "^\\s*(\\d+(?:\\.\\d+)?)\\s*%\\s*$"
  if (grepl(pattern2, stat_value)) {
    matches <- regmatches(stat_value, regexec(pattern2, stat_value))[[1]]
    if (length(matches) >= 2) {
      parsed_stats[[1]] <- list(
        value = matches[2],
        name = "pct",
        label = "Percentage"
      )
      
      return(parsed_stats)
    }
  }
  
  # Pattern 3: Simple number
  pattern3 <- "^\\s*(\\d+(?:\\.\\d+)?)\\s*$"
  if (grepl(pattern3, stat_value)) {
    matches <- regmatches(stat_value, regexec(pattern3, stat_value))[[1]]
    if (length(matches) >= 2) {
      parsed_stats[[1]] <- list(
        value = matches[2],
        name = "n",
        label = "Count"
      )
      
      return(parsed_stats)
    }
  }
  
  # If no pattern matches, return empty list
  list()
}

#' Add metadata columns to ARD data
#'
#' Add processing metadata and other information to ARD output
#'
#' @param ard_data Data frame in ARD format
#' @param metadata Metadata from RTF parsing
#' @return Enhanced ARD data with metadata
#' @keywords internal
add_ard_metadata <- function(ard_data, metadata) {
  if (nrow(ard_data) == 0) {
    return(ard_data)
  }
  
  # Add metadata columns
  ard_data$processing_stage <- metadata$stage %||% "enhanced_state_parser"
  ard_data$processing_method <- metadata$method %||% "enhanced_state_based_parser"
  ard_data$header_levels <- metadata$header_levels %||% 0
  
  ard_data
}

#' Create empty ARD data frame
#'
#' Create an empty ARD data frame with all required columns
#'
#' @return Empty data frame in ARD format
#' @keywords internal
create_empty_ard <- function() {
  data.frame(
    colgroup1 = character(0),
    colgroup1_level = character(0),
    colgroup2 = character(0),
    colgroup2_level = character(0),
    colgroup3 = character(0),
    colgroup3_level = character(0),
    rowgroup1 = character(0),
    rowgroup1_level = character(0),
    rowgroup2 = character(0),
    rowgroup2_level = character(0),
    variable = character(0),
    variable_level = character(0),
    stat = character(0),
    stat_name = character(0),
    stat_label = character(0),
    processing_stage = character(0),
    processing_method = character(0),
    header_levels = integer(0),
    stringsAsFactors = FALSE
  )
}

#' Convert ARD to JSON format
#'
#' Convert ARD data frame to JSON format matching expected output structure
#'
#' @param ard_data Data frame in ARD format
#' @param metadata Additional metadata to include
#' @return List suitable for JSON conversion
#' @export
ard_to_json <- function(ard_data, metadata = NULL) {
  result <- list(
    ard_data = ard_data,
    metadata = metadata %||% list(),
    summary = list(
      total_rows = nrow(ard_data),
      variables_count = length(unique(ard_data$variable)),
      colgroup1_levels = length(unique(ard_data$colgroup1_level[ard_data$colgroup1_level != ""])),
      colgroup2_levels = length(unique(ard_data$colgroup2_level[ard_data$colgroup2_level != ""])),
      colgroup3_levels = length(unique(ard_data$colgroup3_level[ard_data$colgroup3_level != ""])),
      rowgroup1_levels = length(unique(ard_data$rowgroup1_level[ard_data$rowgroup1_level != ""])),
      rowgroup2_levels = length(unique(ard_data$rowgroup2_level[ard_data$rowgroup2_level != ""])),
      hierarchical_variables = sum(ard_data$variable_level != ard_data$variable),
      processing_date = Sys.time()
    )
  )
  
  result
}

#' Convert state parser output to ARD format (legacy interface)
#'
#' Legacy function that maintains backward compatibility with existing code
#'
#' @param table_sections Output from parse_rtf_table_states
#' @param config Configuration options
#' @return Data frame in ARD format
#' @export
state_table_to_ard <- function(table_sections, config = list()) {
  # Use enhanced RTF to ARD conversion
  rtf_to_ard(table_sections, config)
}

#' Extract metadata from state parser output
#'
#' Extract processing metadata from RTF parsing results
#'
#' @param table_sections Output from parse_rtf_table_states
#' @param rtf_file Original RTF file path
#' @return List with metadata information
#' @export
extract_state_metadata <- function(table_sections, rtf_file) {
  metadata <- table_sections$metadata %||% list()
  
  # Add file information
  metadata$source_file <- basename(rtf_file)
  metadata$file_path <- rtf_file
  
  # Add processing timestamp if not present
  if (is.null(metadata$processing_date)) {
    metadata$processing_date <- Sys.time()
  }
  
  metadata
}

#' Enhanced RTF to ARD JSON conversion
#'
#' Main function that combines RTF parsing and ARD conversion with JSON output
#'
#' @param rtf_file Path to RTF file
#' @param output_file Path for JSON output (optional, returns data if NULL)
#' @param options Configuration options
#' @return JSON structure if output_file is NULL, otherwise writes to file
#' @export
rtf_to_ard_json_enhanced <- function(rtf_file, output_file = NULL, options = NULL) {
  # Parse RTF with enhanced parser
  rtf_result <- parse_rtf_table_states(rtf_file)
  
  # Convert to ARD
  ard_data <- rtf_to_ard(rtf_result, options)
  
  # Create JSON structure
  json_result <- ard_to_json(ard_data, rtf_result$metadata)
  
  # Write to file or return
  if (is.null(output_file)) {
    return(json_result)
  } else {
    jsonlite::write_json(json_result, output_file, pretty = TRUE, auto_unbox = TRUE)
    return(invisible(json_result))
  }
}