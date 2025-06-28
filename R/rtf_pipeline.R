#' Clean RTF to ARD JSON Pipeline
#'
#' Direct RTF parsing pipeline: RTF -> DataFrame -> ARD (unparsed) -> ARD (parsed) -> JSON
#' No HTML conversion, no pandoc dependency, pure RTF parsing.
#'
#' @name rtf-pipeline
NULL

#' Convert RTF table directly to ARD JSON
#'
#' Clean pipeline that processes RTF tables through clearly defined stages:
#' 1. RTF -> DataFrame: Extract table structure from RTF
#' 2. DataFrame -> ARD (unparsed): Convert to ARD format with unparsed cells
#' 3. ARD (unparsed) -> ARD (parsed): Parse combined statistics like "n (p%)"
#' 4. ARD (parsed) -> JSON: Generate final JSON output
#'
#' @param rtf_file Path to RTF file
#' @param output_file Path for JSON output (optional)
#' @param config Configuration list (optional)
#' @return Invisibly returns the JSON output
#' @export
rtf_to_ard_pipeline <- function(rtf_file, output_file = NULL, config = list()) {

  if (is.null(output_file)) {
    output_file <- stringr::str_replace(rtf_file, "\\.rtf$", "_ard.json")
  }

  message("Starting RTF->ARD Pipeline for: ", basename(rtf_file))

  # STAGE 1: RTF -> DataFrame
  message("  Stage 1: RTF -> DataFrame")
  table_structure <- parse_rtf_to_dataframe(rtf_file)
  message("    Extracted ", nrow(table_structure$data), " rows, ", ncol(table_structure$data), " columns")

  # STAGE 2: DataFrame -> ARD (unparsed)
  message("  Stage 2: DataFrame -> ARD (unparsed)")
  ard_unparsed <- dataframe_to_ard_unparsed(table_structure, config)
  message("    Generated ", nrow(ard_unparsed), " ARD rows with unparsed cells")

  # STAGE 3: ARD (unparsed) -> ARD (parsed)
  message("  Stage 3: ARD (unparsed) -> ARD (parsed)")
  ard_parsed <- parse_ard_statistics(ard_unparsed)
  message("    Parsed statistics, final ", nrow(ard_parsed), " ARD rows")

  # STAGE 4: ARD (parsed) -> JSON
  message("  Stage 4: ARD (parsed) -> JSON")
  json_output <- ard_to_json(ard_parsed, table_structure$metadata, rtf_file)
  jsonlite::write_json(json_output, output_file, pretty = TRUE, auto_unbox = TRUE)
  message("    JSON written to: ", basename(output_file))

  message("Pipeline complete!")
  message("  - Total rows: ", json_output$summary$total_rows)
  message("  - Groups: ", json_output$summary$unique_groups)
  message("  - Variables: ", json_output$summary$unique_variables)

  return(invisible(json_output))
}

#' Stage 1: Parse RTF directly to DataFrame
#'
#' Extract table structure from RTF without HTML conversion
#'
#' @param rtf_file Path to RTF file
#' @return List with 'data' (DataFrame) and 'metadata' (list)
#' @keywords internal
parse_rtf_to_dataframe <- function(rtf_file) {

  # Parse RTF structure
  table_sections <- parse_rtf_table_states(rtf_file)

  # Extract clean DataFrame from table section
  table_data <- table_sections$table$data

  # Extract metadata from sections
  metadata <- list(
    source_file = basename(rtf_file),
    title = extract_title_from_sections(table_sections),
    population = extract_population_from_sections(table_sections),
    footnotes = extract_footnotes_from_sections(table_sections)
  )

  return(list(
    data = table_data,
    metadata = metadata
  ))
}

#' Stage 2: Convert DataFrame to ARD format (unparsed)
#'
#' Transform DataFrame to ARD structure but keep cells unparsed (e.g., "n (p%)")
#'
#' @param table_structure Output from parse_rtf_to_dataframe
#' @param config Configuration list
#' @return DataFrame in ARD format with unparsed stat cells
#' @keywords internal
dataframe_to_ard_unparsed <- function(table_structure, config) {

  data <- table_structure$data

  if (nrow(data) == 0) {
    return(data.frame())
  }

  # Convert to long format (ARD structure)
  ard_unparsed <- data |>
    dplyr::mutate(across(everything(), as.character)) |>
    dplyr::rename(variable = 1) |>
    tidyr::pivot_longer(
      cols = -variable,
      names_to = "column_name",
      values_to = "stat_unparsed"
    ) |>
    dplyr::filter(!is.na(stat_unparsed) & trimws(stat_unparsed) != "") |>
    dplyr::mutate(
      # Extract group information from column names
      group1 = "TRT",
      group1_level = extract_group_from_column(column_name),
      variable = clean_variable_name(variable),
      # Keep original unparsed value for now
      stat = stat_unparsed,
      stat_name = "unparsed",
      stat_label = "Unparsed"
    ) |>
    dplyr::select(group1, group1_level, variable, stat, stat_name, stat_label)

  # Handle Big N extraction
  ard_with_bign <- extract_bign_from_ard(ard_unparsed)

  return(ard_with_bign)
}

#' Stage 3: Parse ARD statistics
#'
#' Parse combined statistics like "137 (39.0%)" into separate ARD rows
#'
#' @param ard_unparsed ARD data with unparsed stat cells
#' @return ARD data with parsed statistics (separate rows)
#' @keywords internal
parse_ard_statistics <- function(ard_unparsed) {

  if (nrow(ard_unparsed) == 0) {
    return(ard_unparsed)
  }

  # Apply statistical pattern parsing to each row
  parsed_rows <- list()

  for (i in seq_len(nrow(ard_unparsed))) {
    row <- ard_unparsed[i, ]

    # Skip already parsed rows (like BIGN)
    if (row$stat_name != "unparsed") {
      parsed_rows[[length(parsed_rows) + 1]] <- row
      next
    }

    # Try to parse the statistic
    parsed_stats <- parse_single_statistic(row$stat)

    if (nrow(parsed_stats) > 0) {
      # Create separate ARD rows for each parsed statistic
      for (j in seq_len(nrow(parsed_stats))) {
        new_row <- row
        new_row$stat <- parsed_stats$stat[j]
        new_row$stat_name <- parsed_stats$stat_name[j]
        new_row$stat_label <- parsed_stats$stat_label[j]
        parsed_rows[[length(parsed_rows) + 1]] <- new_row
      }
    } else {
      # Couldn't parse, keep as-is but classify as "other"
      row$stat_name <- "other"
      row$stat_label <- "Other"
      parsed_rows[[length(parsed_rows) + 1]] <- row
    }
  }

  # Combine all parsed rows
  ard_parsed <- do.call(rbind, parsed_rows)

  # Filter out blank/separator rows
  ard_parsed <- ard_parsed |>
    dplyr::filter(
      !grepl("^\\\\+$", stat) &
      !(stat_name == "other" & grepl("^\\\\+$", stat))
    )

  return(ard_parsed)
}

#' Stage 4: Convert ARD to JSON
#'
#' Generate final JSON output with metadata and summary
#'
#' @param ard_parsed Parsed ARD data
#' @param metadata Metadata from RTF parsing
#' @param rtf_file Original RTF file path
#' @return List ready for JSON serialization
#' @keywords internal
ard_to_json <- function(ard_parsed, metadata, rtf_file) {

  # Enhanced metadata
  json_metadata <- metadata
  json_metadata$conversion_date <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  json_metadata$conversion_method <- "rtf_direct_pipeline"

  # Summary statistics
  summary_stats <- list(
    total_rows = nrow(ard_parsed),
    unique_groups = length(unique(ard_parsed$group1_level)),
    unique_variables = length(unique(ard_parsed$variable)),
    stat_types = unique(ard_parsed$stat_name),
    hierarchical_variables = sum(grepl(" - ", ard_parsed$variable)),
    group_levels = sum(!is.na(ard_parsed[grepl("^group[2-9]", names(ard_parsed))]))
  )

  return(list(
    metadata = json_metadata,
    ard_data = ard_parsed,
    summary = summary_stats
  ))
}

# Helper functions

#' Extract group information from column name
#' @keywords internal
extract_group_from_column <- function(column_name) {
  # This would need to be implemented based on how columns are named
  # For now, return the column name cleaned up
  stringr::str_replace_all(column_name, "^V", "Group_")
}

#' Clean variable names
#' @keywords internal
clean_variable_name <- function(variable) {
  stringr::str_trim(variable)
}

#' Extract Big N from ARD data
#' @keywords internal
extract_bign_from_ard <- function(ard_data) {
  # Apply Big N extraction logic
  bign_patterns <- get_bign_patterns()

  # Find rows that contain Big N patterns
  bign_rows <- ard_data |>
    dplyr::filter(
      stringr::str_detect(stat, paste(bign_patterns, collapse = "|"))
    ) |>
    dplyr::mutate(
      stat = stringr::str_extract(stat, "\\d+"),
      stat_name = "n",
      stat_label = "n",
      variable = "BIGN"
    )

  # Remove Big N from original data and add back as clean rows
  ard_clean <- ard_data |>
    dplyr::filter(
      !stringr::str_detect(stat, paste(bign_patterns, collapse = "|"))
    )

  if (nrow(bign_rows) > 0) {
    ard_clean <- rbind(bign_rows, ard_clean)
  }

  return(ard_clean)
}

#' Parse a single statistic value
#' @keywords internal
parse_single_statistic <- function(stat_value) {
  # Use the pseudo-pattern parser
  result <- parse_stat_value_pseudo(stat_value)

  # If it returned "other", return empty dataframe
  # to maintain backward compatibility
  if (nrow(result) == 1 && result$stat_name[1] == "other") {
    return(data.frame())
  }

  return(result)
}

#' Extract title from table sections
#' @keywords internal
extract_title_from_sections <- function(table_sections) {
  if (is.null(table_sections$pre_header) || length(table_sections$pre_header$text) == 0) {
    return(NULL)
  }

  pre_header_text <- table_sections$pre_header$text
  title_patterns <- get_title_patterns()

  for (pattern in title_patterns) {
    matches <- pre_header_text[grepl(pattern, pre_header_text, ignore.case = TRUE)]
    if (length(matches) > 0) {
      return(matches[1])
    }
  }

  return(NULL)
}

#' Extract population from table sections
#' @keywords internal
extract_population_from_sections <- function(table_sections) {
  if (is.null(table_sections$pre_header) || length(table_sections$pre_header$text) == 0) {
    return(NULL)
  }

  pre_header_text <- table_sections$pre_header$text
  population_patterns <- get_population_patterns()

  for (pattern in population_patterns) {
    matches <- pre_header_text[grepl(pattern, pre_header_text, ignore.case = TRUE)]
    if (length(matches) > 0) {
      match_result <- regmatches(matches[1], regexpr(pattern, matches[1], ignore.case = TRUE))
      if (length(match_result) > 0) {
        return(match_result)
      }
    }
  }

  return(NULL)
}

#' Extract footnotes from table sections
#' @keywords internal
extract_footnotes_from_sections <- function(table_sections) {
  if (is.null(table_sections$footnotes) || length(table_sections$footnotes$text) == 0) {
    return(NULL)
  }

  return(table_sections$footnotes$text)
}