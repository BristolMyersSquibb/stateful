#' Export Functions for ARD Data
#'
#' Unified export functions for ARD data in various formats.
#'
#' @name export-functions
NULL

#' Export ARD data to CSV format
#'
#' Exports ARD data to CSV format optimized for external analysis.
#'
#' @param ard_data ARD data frame from rtf_to_ard()
#' @param file_path Output CSV file path
#' @param include_metadata Whether to include processing metadata (default: TRUE)
#'
#' @return Invisibly returns the file path
#' @export
#'
#' @examples
#' \dontrun{
#' ard_data <- rtf_to_ard("table.rtf")
#' export_ard_csv(ard_data, "output.csv")
#' }
export_ard_csv <- function(ard_data, file_path, include_metadata = TRUE) {
  if (nrow(ard_data) == 0) {
    stop("No ARD data to export")
  }

  # Prepare data for CSV export
  export_data <- ard_data |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), "", .)))

  # Write CSV
  utils::write.csv(export_data, file_path, row.names = FALSE, na = "")

  # Add metadata as comments if requested
  if (include_metadata) {
    add_csv_metadata_header(file_path, list(
      export_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      rows = nrow(export_data),
      columns = ncol(export_data),
      exported_by = "stateful package"
    ))
  }

  message("ARD data exported to CSV: ", basename(file_path))
  message("  Rows: ", nrow(export_data))
  message("  Columns: ", ncol(export_data))

  return(invisible(file_path))
}

#' Export ARD data to JSON format
#'
#' Exports ARD data to JSON format with metadata.
#'
#' @param ard_data ARD data frame from rtf_to_ard()
#' @param file_path Output JSON file path
#' @param metadata Optional metadata list to include
#'
#' @return Invisibly returns the file path
#' @export
#'
#' @examples
#' \dontrun{
#' ard_data <- rtf_to_ard("table.rtf")
#' export_ard_json(ard_data, "output.json")
#' }
export_ard_json <- function(ard_data, file_path, metadata = NULL) {
  if (nrow(ard_data) == 0) {
    stop("No ARD data to export")
  }

  # Create JSON structure
  json_output <- list(
    metadata = metadata %||% list(
      export_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      exported_by = "stateful package"
    ),
    data = ard_data,
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
      stat_types = unique(ard_data$stat_name)
    )
  )

  # Write JSON
  jsonlite::write_json(json_output, file_path, pretty = TRUE, auto_unbox = TRUE)

  message("ARD data exported to JSON: ", basename(file_path))
  message("  Rows: ", json_output$summary$total_rows)

  return(invisible(file_path))
}

#' Batch convert RTF files to ARD format
#'
#' Convert multiple RTF files to ARD JSON format in batch.
#'
#' @param input_dir Directory containing RTF files
#' @param output_dir Directory for output JSON files (default: same as input)
#' @param config Optional configuration list applied to all files
#' @param parse_stats Whether to parse statistics (default: TRUE)
#'
#' @return Invisibly returns vector of output file paths
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert all RTF files in a directory
#' batch_convert_rtf("input_dir", "output_dir")
#' }
batch_convert_rtf <- function(input_dir, output_dir = input_dir, 
                             config = list(), parse_stats = TRUE) {
  # Find all RTF files
  rtf_files <- list.files(input_dir, pattern = "\\.rtf$", full.names = TRUE)

  if (length(rtf_files) == 0) {
    stop("No RTF files found in directory: ", input_dir)
  }

  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  message("Converting ", length(rtf_files), " RTF files...")
  output_files <- character(length(rtf_files))

  for (i in seq_along(rtf_files)) {
    rtf_file <- rtf_files[i]
    base_name <- tools::file_path_sans_ext(basename(rtf_file))
    output_file <- file.path(output_dir, paste0(base_name, "_ard.json"))

    message("  Processing: ", basename(rtf_file))

    tryCatch({
      rtf_to_ard_json(rtf_file, output_file, config, parse_stats)
      output_files[i] <- output_file
    }, error = function(e) {
      warning("Failed to process ", basename(rtf_file), ": ", e$message)
      output_files[i] <- NA_character_
    })
  }

  successful <- sum(!is.na(output_files))
  message("Batch conversion complete: ", successful, "/", length(rtf_files), 
          " files processed successfully")

  return(invisible(output_files[!is.na(output_files)]))
}

#' Helper function to add metadata header to CSV file
#' @keywords internal
add_csv_metadata_header <- function(file_path, metadata) {
  # Read existing file
  lines <- readLines(file_path)

  # Create metadata header
  header_lines <- c(
    "# ARD Data Export",
    paste("#", names(metadata), ":", metadata),
    "#",
    lines
  )

  # Write back with header
  writeLines(header_lines, file_path)
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}