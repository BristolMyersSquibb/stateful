#' Main Entry Points for Stateful RTF Parsing
#'
#' Unified API for RTF table parsing with clear, simple entry points.
#'
#' @name main-api
NULL

#' Convert RTF to ARD data frame
#'
#' Main function to convert RTF tables to Analysis Results Data (ARD) format.
#' Uses state-based RTF parsing for robust extraction.
#'
#' @param rtf_file Path to RTF file
#' @param config Optional configuration list
#' @param parse_stats Whether to parse statistics (TRUE) or return raw (FALSE)
#'
#' @return Data frame in ARD format
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert RTF to ARD with statistics parsed
#' ard_data <- rtf_to_ard("table.rtf")
#'
#' # Convert with custom configuration
#' config <- list(group2_name = "TIMEPOINT")
#' ard_data <- rtf_to_ard("table.rtf", config = config)
#'
#' # Get raw statistics without parsing
#' ard_raw <- rtf_to_ard("table.rtf", parse_stats = FALSE)
#' }
rtf_to_ard <- function(rtf_file, config = list(), parse_stats = TRUE, encoding = NULL) {
  # Parse RTF using state-based approach
  table_sections <- parse_rtf_table_states(rtf_file, encoding = encoding)

  # Convert to ARD
  ard_data <- state_table_to_ard(table_sections, config)

  if (!parse_stats) {
    # Return with unparsed statistics
    ard_data$stat_name <- "unparsed"
    ard_data$stat_label <- "Unparsed"
  }

  return(ard_data)
}

#' Convert RTF to ARD JSON
#'
#' Main function to convert RTF tables to ARD JSON format with metadata.
#' Uses state-based RTF parsing for robust extraction.
#'
#' @param rtf_file Path to RTF file
#' @param output_file Optional output file path (if NULL, auto-generated)
#' @param config Optional configuration list
#' @param parse_stats Whether to parse statistics (TRUE) or return raw (FALSE)
#'
#' @return Invisibly returns the JSON output list
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert RTF to JSON
#' rtf_to_ard_json("table.rtf", "output.json")
#'
#' # Auto-generate output filename
#' rtf_to_ard_json("table.rtf")
#'
#' # Custom configuration
#' config <- list(group2_name = "VISIT")
#' rtf_to_ard_json("table.rtf", config = config)
#'
#' # Export raw statistics without parsing
#' rtf_to_ard_json("table.rtf", parse_stats = FALSE)
#' }
rtf_to_ard_json <- function(rtf_file, output_file = NULL, config = list(), 
                           parse_stats = TRUE, encoding = NULL) {
  if (is.null(output_file)) {
    output_file <- stringr::str_replace(rtf_file, "\\.rtf$", "_ard.json")
  }

  # Get ARD data
  ard_data <- rtf_to_ard(rtf_file, config, parse_stats, encoding = encoding)

  # Parse RTF for metadata
  table_sections <- parse_rtf_table_states(rtf_file, encoding = encoding)
  metadata <- extract_state_metadata(table_sections, rtf_file)

  # Create JSON structure
  json_output <- list(
    metadata = metadata,
    ard_data = ard_data,
    summary = list(
      total_rows = nrow(ard_data),
      unique_groups = length(unique(ard_data$group1_level)),
      unique_variables = length(unique(
        if ("variable_level" %in% names(ard_data)) {
          ard_data$variable_level
        } else {
          ard_data$variable
        }
      )),
      stat_types = unique(ard_data$stat_name),
      hierarchical_variables = sum(grepl(
        " - ",
        if ("variable_level" %in% names(ard_data)) {
          ard_data$variable_level
        } else {
          ard_data$variable
        }
      )),
      group_levels = sum(!is.na(ard_data[grepl("^group[2-9]", names(ard_data))]))
    )
  )

  # Write JSON
  jsonlite::write_json(json_output, output_file, pretty = TRUE, auto_unbox = TRUE)

  message("ARD JSON file created: ", basename(output_file))
  message("  - Rows: ", json_output$summary$total_rows)
  message("  - Groups: ", json_output$summary$unique_groups)

  return(invisible(json_output))
}

#' Parse RTF with Stage 1 output (structural only)
#'
#' Extract raw structural data from RTF without pattern interpretation.
#' Useful for manual review and double programming.
#'
#' @param rtf_file Path to RTF file
#'
#' @return List with structured ARD data and metadata
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract structural data only
#' stage1_data <- rtf_to_ard_stage1("table.rtf")
#' }
rtf_to_ard_stage1 <- function(rtf_file) {
  state_parser_stage1(rtf_file)
}