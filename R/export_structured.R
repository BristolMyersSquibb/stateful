#' Export Functions for Structured ARD (Stage 1)
#'
#' Functions to export the reliable structured ARD data to various formats
#' suitable for double programming and further processing.
#'
#' @name export-structured
NULL

#' Export structured ARD to CSV format
#'
#' Exports the reliable structured ARD data to CSV format optimized for
#' SAS import and double programming workflows.
#'
#' @param structured_ard Output from parse_rtf_to_structured_ard()
#' @param file_path Output CSV file path
#' @param include_metadata Whether to include metadata as comments (default: TRUE)
#'
#' @return Invisibly returns the file path
#' @export
export_structured_ard_csv <- function(structured_ard, file_path, include_metadata = TRUE) {
  
  if (is.null(structured_ard$data) || nrow(structured_ard$data) == 0) {
    stop("No structured ARD data to export")
  }
  
  # Prepare data for CSV export
  export_data <- structured_ard$data
  
  # Ensure all columns are character for SAS compatibility
  export_data <- export_data |>
    mutate(across(everything(), as.character)) |>
    mutate(across(everything(), ~ ifelse(is.na(.), "", .)))
  
  # Write CSV with SAS-friendly settings
  write.csv(export_data, file_path, row.names = FALSE, na = "")
  
  # Add metadata as comments if requested
  if (include_metadata && !is.null(structured_ard$metadata)) {
    add_csv_metadata_comments(file_path, structured_ard$metadata)
  }
  
  message("Structured ARD exported to CSV: ", basename(file_path))
  message("  Rows: ", nrow(export_data))
  message("  Columns: ", ncol(export_data))
  message("  Confidence: ", round(structured_ard$confidence_score * 100, 1), "%")
  
  return(invisible(file_path))
}

#' Export structured ARD to JSON format
#'
#' Exports the reliable structured ARD data to JSON format with full metadata.
#'
#' @param structured_ard Output from parse_rtf_to_structured_ard()
#' @param file_path Output JSON file path
#' @param pretty Whether to use pretty formatting (default: TRUE)
#'
#' @return Invisibly returns the file path
#' @export
export_structured_ard_json <- function(structured_ard, file_path, pretty = TRUE) {
  
  if (is.null(structured_ard$data) || nrow(structured_ard$data) == 0) {
    stop("No structured ARD data to export")
  }
  
  # Create comprehensive JSON structure
  json_output <- list(
    stage = "reliable_structured_ard",
    data = structured_ard$data,
    metadata = structured_ard$metadata,
    quality = list(
      confidence_score = structured_ard$confidence_score,
      method = structured_ard$method,
      row_count = nrow(structured_ard$data),
      column_count = ncol(structured_ard$data),
      variable_levels = sum(!is.na(structured_ard$data$variable_level_2)),
      group_levels = sum(!is.na(structured_ard$data$group2_level)),
      export_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    column_info = get_column_info(structured_ard$data)
  )
  
  # Write JSON
  jsonlite::write_json(json_output, file_path, pretty = pretty, auto_unbox = TRUE)
  
  message("Structured ARD exported to JSON: ", basename(file_path))
  message("  Rows: ", nrow(structured_ard$data))
  message("  Quality score: ", round(structured_ard$confidence_score * 100, 1), "%")
  
  return(invisible(file_path))
}

#' Create structured ARD from RTF file and export directly
#'
#' Convenience function that combines parsing and export in one step.
#'
#' @param rtf_file Path to RTF file
#' @param output_file Output file path (extension determines format)
#' @param use_nested_logic Apply nested row logic (default: TRUE)
#' @param fallback_to_position Use position-based fallback (default: TRUE)
#' @param ... Additional arguments passed to export functions
#'
#' @return Invisibly returns the structured ARD object
#' @export
rtf_to_structured_export <- function(rtf_file, output_file, 
                                   use_nested_logic = TRUE, 
                                   fallback_to_position = TRUE, ...) {
  
  # Parse RTF to structured ARD
  structured_ard <- parse_rtf_to_structured_ard(
    rtf_file = rtf_file,
    use_nested_logic = use_nested_logic,
    fallback_to_position = fallback_to_position
  )
  
  # Determine output format from extension
  file_ext <- tools::file_ext(output_file)
  
  if (tolower(file_ext) == "csv") {
    export_structured_ard_csv(structured_ard, output_file, ...)
  } else if (tolower(file_ext) == "json") {
    export_structured_ard_json(structured_ard, output_file, ...)
  } else {
    stop("Unsupported output format. Use .csv or .json extension.")
  }
  
  return(invisible(structured_ard))
}

#' Add metadata comments to CSV file
#'
#' Adds metadata information as comments at the top of CSV file.
#' This helps with traceability in double programming workflows.
#'
#' @param file_path Path to CSV file
#' @param metadata Metadata list from structured ARD
#' @keywords internal
add_csv_metadata_comments <- function(file_path, metadata) {
  
  # Read existing CSV content
  csv_content <- readLines(file_path)
  
  # Create metadata comments
  comments <- c(
    "# Reliable Structured ARD Export",
    paste("# Export Date:", metadata$conversion_date %||% Sys.time()),
    paste("# Conversion Method:", metadata$conversion_method %||% "reliable_structured_ard")
  )
  
  if (!is.null(metadata$likely_title)) {
    comments <- c(comments, paste("# Likely Title:", metadata$likely_title))
  }
  
  if (!is.null(metadata$title_lines) && length(metadata$title_lines) > 0) {
    comments <- c(comments, "# Title Lines:")
    for (i in seq_along(metadata$title_lines)) {
      comments <- c(comments, paste("#  ", i, ":", metadata$title_lines[i]))
    }
  }
  
  if (!is.null(metadata$footnote_lines) && length(metadata$footnote_lines) > 0) {
    comments <- c(comments, "# Footnote Lines:")
    for (i in seq_along(head(metadata$footnote_lines, 5))) {  # Limit to first 5
      comments <- c(comments, paste("#  ", i, ":", metadata$footnote_lines[i]))
    }
  }
  
  comments <- c(comments, "#")
  
  # Combine comments with CSV content
  final_content <- c(comments, csv_content)
  
  # Write back to file
  writeLines(final_content, file_path)
}

#' Get column information for metadata
#'
#' @param data Structured ARD data frame
#' @keywords internal
get_column_info <- function(data) {
  
  col_info <- list()
  
  # Variable hierarchy info
  col_info$variable_hierarchy <- list(
    level_1_count = sum(!is.na(data$variable_level_1) & data$variable_level_1 != ""),
    level_2_count = sum(!is.na(data$variable_level_2) & data$variable_level_2 != ""),
    level_3_count = sum(!is.na(data$variable_level_3) & data$variable_level_3 != ""),
    max_indent_level = max(data$indent_level, na.rm = TRUE)
  )
  
  # Group structure info
  col_info$group_structure <- list(
    group1_levels = unique(data$group1_level[!is.na(data$group1_level)]),
    group2_levels = unique(data$group2_level[!is.na(data$group2_level)]),
    group3_levels = unique(data$group3_level[!is.na(data$group3_level)])
  )
  
  # Header types detected
  col_info$header_types <- list(
    col1_headers = unique(data$col1_header[!is.na(data$col1_header)]),
    col2_headers = unique(data$col2_header[!is.na(data$col2_header)]),
    col3_headers = unique(data$col3_header[!is.na(data$col3_header)])
  )
  
  # Data quality indicators
  col_info$quality_indicators <- list(
    total_rows = nrow(data),
    non_empty_stats = sum(!is.na(data$raw_stat) & data$raw_stat != ""),
    unique_variables = length(unique(data$variable_level_1)),
    hierarchy_depth = max(
      ifelse(any(!is.na(data$variable_level_3)), 3, 
      ifelse(any(!is.na(data$variable_level_2)), 2, 1))
    )
  )
  
  return(col_info)
}

#' Load structured ARD from exported JSON
#'
#' Utility function to load previously exported structured ARD data.
#'
#' @param json_file Path to JSON file created by export_structured_ard_json()
#'
#' @return Structured ARD object
#' @export
load_structured_ard_json <- function(json_file) {
  
  if (!file.exists(json_file)) {
    stop("JSON file not found: ", json_file)
  }
  
  json_data <- jsonlite::fromJSON(json_file)
  
  # Validate it's a structured ARD file
  if (is.null(json_data$stage) || json_data$stage != "reliable_structured_ard") {
    warning("File may not be a structured ARD export")
  }
  
  # Return in same format as parse_rtf_to_structured_ard()
  structured_ard <- list(
    data = as.data.frame(json_data$data),
    metadata = json_data$metadata,
    confidence_score = json_data$quality$confidence_score %||% 0.9,
    method = json_data$quality$method %||% "loaded_from_json"
  )
  
  message("Loaded structured ARD from JSON: ", basename(json_file))
  message("  Rows: ", nrow(structured_ard$data))
  message("  Export date: ", json_data$quality$export_timestamp %||% "Unknown")
  
  return(structured_ard)
}