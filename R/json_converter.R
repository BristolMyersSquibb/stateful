
#' Convert RTF to comprehensive ARD JSON format
#'
#' This function converts an RTF table to a complete ARD JSON structure including
#' metadata and properly expanded statistics. Uses direct RTF parsing pipeline.
#'
#' @param rtf_file Path to the RTF file
#' @param output_file Path for the output JSON file
#' @param config Optional table configuration
#'
#' @return Invisible. Writes JSON file and returns metadata.
#'
#' @export
rtf_to_ard_json <- function(rtf_file, output_file = NULL, config = list()) {
  
  # Generate output filename if not provided
  if (is.null(output_file)) {
    base_name <- tools::file_path_sans_ext(basename(rtf_file))
    output_file <- file.path(dirname(rtf_file), paste0(base_name, "_ard.json"))
  }
  
  # Use the new direct RTF pipeline (no HTML conversion)
  json_output <- rtf_to_ard_pipeline(rtf_file, output_file, config)
  
  # Return metadata for backwards compatibility
  invisible(json_output$metadata)
}

#' Convert all RTF files in extdata to ARD JSON format
#'
#' This function processes all RTF files in the extdata directory,
#' converts them to ARD format, and saves each as a JSON file.
#'
#' @param input_dir A string, path to directory containing RTF files.
#' @param output_dir A string, path to directory for JSON output files.
#'
#' @return Invisible. Creates JSON files for each RTF file.
#'
#' @export
convert_all_rtf_to_json <- function(input_dir = "extdata", output_dir = "extdata") {
  # Find all RTF files
  rtf_files <- list.files(input_dir, pattern = "\\.rtf$", full.names = TRUE)
  
  if (length(rtf_files) == 0) {
    stop("No RTF files found in directory: ", input_dir)
  }
  
  message("Found ", length(rtf_files), " RTF files to process:")
  for (file in rtf_files) {
    message("  - ", basename(file))
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Process each RTF file
  for (rtf_file in rtf_files) {
    message("\nProcessing: ", basename(rtf_file))
    
    tryCatch({
      # Convert using the comprehensive converter
      base_name <- tools::file_path_sans_ext(basename(rtf_file))
      json_file <- file.path(output_dir, paste0(base_name, "_ard.json"))
      
      rtf_to_ard_json(rtf_file, json_file)
      
    }, error = function(e) {
      message("Error processing ", basename(rtf_file), ": ", e$message)
    })
  }
  
  message("\nConversion complete!")
  invisible()
}