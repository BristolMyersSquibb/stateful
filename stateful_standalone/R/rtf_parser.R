#' RTF State-Based Table Parser
#'
#' Parse RTF tables by identifying distinct states: pre-header, header, table, footnotes
#' Uses RTF tags and structure, not content pattern matching.
#'
#' @name rtf-parser
NULL

#' Initialize global pattern libraries
#' @keywords internal
.init_global_patterns <- function() {
  .stateful_patterns$BIGN_PATTERNS <- c(
    "\\(N\\s*=\\s*\\d+\\)",           # (N = 123)
    "N\\s*=\\s*\\d+",                # N = 123
    "N\\s*:\\s*\\d+",                # N: 123
    "N\\s+\\d+",                     # N 123
    "\\(n\\s*=\\s*\\d+\\)",          # (n = 123)
    "n\\s*=\\s*\\d+",                # n = 123
    "\\(N\\d+\\)",                   # (N123)
    "N\\d+"                          # N123
  )

  # Flexible metadata extraction patterns
  .stateful_patterns$TITLE_PATTERNS <- c(
    "Table\\s+\\d+",                 # Table 14.3.2.1.1.1
    "Table\\s+[A-Z]\\d+",           # Table A1
    "^\\d+\\.\\d+",                 # 14.3.2.1.1.1
    "Appendix\\s+\\w+"              # Appendix 16.2.7.4.1
  )

  .stateful_patterns$POPULATION_PATTERNS <- c(
    "All Treated Subjects?",
    "Safety Population",
    "ITT Population",
    "Intent[- ]to[- ]Treat",
    "Per[- ]Protocol",
    "Modified ITT",
    "Full Analysis Set",
    "Randomized Set"
  )

  .stateful_patterns$FOOTNOTE_INDICATORS <- c(
    "MedDRA Version.*CTC Version",
    "Includes events reported between",
    "Excludes data collected on or after",
    "Program Source:",
    "^\\s*\\([0-9]+\\)",             # (1), (2), etc.
    "^\\s*\\{[a-z]\\}",             # {a}, {b}, etc.
    "^\\s*[a-z]\\)",                # a), b), etc.
    "\\d{2}[A-Z]{3}\\d{4}\\s+\\d{2}:\\d{2}" # Date/time stamps
  )

  .stateful_patterns$TABLE_HEADER_INDICATORS <- c(
    "SELECT AES BY CATEGORY.*\\| \\\\\\\\\\\\$",
    "IMAES.*WITHIN.*BY CATEGORY.*\\| \\\\\\\\\\\\$",
    "OESIS.*WITHIN.*BY CATEGORY.*\\| \\\\\\\\\\\\$",
    "ENDOCRINE.*WITHIN.*BY.*CATEGORY.*\\| \\\\\\\\\\\\$",
    "Safety Parameters.*Grade.*Grade",
    "Any Grade.*Grade 3-4.*Any Grade.*Grade 3-4",
    "Within \\d+ days.*\\| \\\\\\\\\\\\$",
    "Last Dose.*\\| \\\\\\\\\\\\$",
    "; \\| \\\\\\\\\\\\$",
    "Category; \\| \\\\\\\\\\\\$"
  )

  # Pattern indicators for post-processing
  .stateful_patterns$PERCENTAGE_INDICATORS <- c(
    "\\(%\\)",                          # (%) in variable names
    "\\bpercent\\b",                   # "percent" text
    "\\b%\\b"                          # % symbol
  )

  .stateful_patterns$VARIABLE_NAMES <- list(
    bign = "BIGN",
    missing = "MISSING"
  )

  .stateful_patterns$STAT_NAMES <- list(
    count = "n",
    percentage = "p",
    total = "N",
    missing = "missing"
  )

  .stateful_patterns$STAT_PATTERNS <- list(
    # Common clinical trial formats - with trailing semicolon support
    "n_pct_semicolon" = list(
      template = "{n} ({pct});",
      regex = "^(\\d+)\\s*\\(\\s*(\\d+\\.?\\d*)\\s*\\);?$",
      stats = c("n", "pct"),
      labels = c("n", "pct")
    ),
    "n_pct" = list(
      template = "{n} ({pct}%)",
      regex = "^(\\d+)\\s*\\(\\s*(\\d+\\.?\\d*)%?\\s*\\)$",
      stats = c("n", "pct"),
      labels = c("n", "pct")
    ),
    "n_pct_space" = list(
      template = "{n}{ }( {pct})",
      regex = "^(\\d+)\\{\\s*\\}\\(\\s*(\\d+\\.?\\d*)\\s*\\)$",
      stats = c("n", "pct"),
      labels = c("n", "pct")
    ),
    "mean_sd" = list(
      template = "{mean} ({sd})",
      regex = "^(\\d+\\.?\\d*)\\s*\\(\\s*(\\d+\\.?\\d*)\\s*\\);?$",
      stats = c("mean", "sd"),
      labels = c("Mean", "SD")
    ),
    "median_iqr" = list(
      template = "{median} ({q1}, {q3})",
      regex = "^(\\d+\\.?\\d*)\\s*\\(\\s*(\\d+\\.?\\d*)\\s*,\\s*(\\d+\\.?\\d*)\\s*\\);?$",
      stats = c("median", "q1", "q3"),
      labels = c("Median", "Q1", "Q3")
    ),
    "min_max" = list(
      template = "{min} - {max}",
      regex = "^(\\d+\\.?\\d*)\\s*-\\s*(\\d+\\.?\\d*);?$",
      stats = c("min", "max"),
      labels = c("Min", "Max")
    ),
    # Single value patterns with semicolon support
    "count_semicolon" = list(
      template = "{n};",
      regex = "^(\\d+);$",
      stats = c("n"),
      labels = c("n")
    ),
    "count" = list(
      template = "{n}",
      regex = "^(\\d+)$",
      stats = c("n"),
      labels = c("n")
    ),
    "decimal_semicolon" = list(
      template = "{value};",
      regex = "^(\\d+\\.\\d+);$",
      stats = c("value"),
      labels = c("Value")
    ),
    "decimal" = list(
      template = "{value}",
      regex = "^(\\d+\\.\\d+)$",
      stats = c("value"),
      labels = c("Value")
    )
  )
}

#' Get BIGN patterns (legacy - now using pseudo-patterns)
#'
#' Returns regex patterns converted from pseudo-patterns for backward compatibility.
#'
#' @return Vector of regex patterns
#' @export
get_bign_patterns <- function() {
  # Use pseudo-pattern system
  pseudo_patterns <- get_bign_pseudo_patterns()

  # Extract just the regex patterns for backward compatibility
  regex_patterns <- sapply(pseudo_patterns, function(p) p$regex)
  names(regex_patterns) <- NULL

  return(regex_patterns)
}

#' Add title pattern
#' @param pattern Regex pattern for title detection
#' @export
add_title_pattern <- function(pattern) {
  if (is.null(.stateful_patterns$TITLE_PATTERNS)) {
    .init_global_patterns()
  }
  .stateful_patterns$TITLE_PATTERNS <- c(pattern, .stateful_patterns$TITLE_PATTERNS)
}

#' Get title patterns
#' @export
get_title_patterns <- function() {
  if (is.null(.stateful_patterns$TITLE_PATTERNS)) {
    .init_global_patterns()
  }
  return(.stateful_patterns$TITLE_PATTERNS)
}

#' Add population pattern
#' @param pattern Regex pattern for population detection
#' @export
add_population_pattern <- function(pattern) {
  if (is.null(.stateful_patterns$POPULATION_PATTERNS)) {
    .init_global_patterns()
  }
  .stateful_patterns$POPULATION_PATTERNS <- c(pattern, .stateful_patterns$POPULATION_PATTERNS)
}

#' Get population patterns
#' @export
get_population_patterns <- function() {
  if (is.null(.stateful_patterns$POPULATION_PATTERNS)) {
    .init_global_patterns()
  }
  return(.stateful_patterns$POPULATION_PATTERNS)
}

#' Add footnote indicator pattern
#' @param pattern Regex pattern for footnote detection
#' @export
add_footnote_pattern <- function(pattern) {
  if (is.null(.stateful_patterns$FOOTNOTE_INDICATORS)) {
    .init_global_patterns()
  }
  .stateful_patterns$FOOTNOTE_INDICATORS <- c(pattern, .stateful_patterns$FOOTNOTE_INDICATORS)
}

#' Get footnote patterns
#' @export
get_footnote_patterns <- function() {
  if (is.null(.stateful_patterns$FOOTNOTE_INDICATORS)) {
    .init_global_patterns()
  }
  return(.stateful_patterns$FOOTNOTE_INDICATORS)
}

#' Add table header indicator pattern
#' @param pattern Regex pattern for table header detection
#' @export
add_table_header_pattern <- function(pattern) {
  if (is.null(.stateful_patterns$TABLE_HEADER_INDICATORS)) {
    .init_global_patterns()
  }
  .stateful_patterns$TABLE_HEADER_INDICATORS <- c(pattern, .stateful_patterns$TABLE_HEADER_INDICATORS)
}

#' Get table header patterns
#' @export
get_table_header_patterns <- function() {
  if (is.null(.stateful_patterns$TABLE_HEADER_INDICATORS)) {
    .init_global_patterns()
  }
  return(.stateful_patterns$TABLE_HEADER_INDICATORS)
}

#' Configure patterns for a new table type
#'
#' Easily add patterns for new table types without hardcoding
#'
#' @param table_type Name of the table type (e.g., "safety", "efficacy", "demographics")
#' @param title_patterns Vector of regex patterns for title detection
#' @param population_patterns Vector of regex patterns for population detection
#' @param footnote_patterns Vector of regex patterns for footnote detection
#' @param header_patterns Vector of regex patterns for table header detection
#' @export
configure_table_patterns <- function(table_type,
                                   title_patterns = NULL,
                                   population_patterns = NULL,
                                   footnote_patterns = NULL,
                                   header_patterns = NULL) {

  message("Configuring patterns for table type: ", table_type)

  if (!is.null(title_patterns)) {
    for (pattern in title_patterns) {
      add_title_pattern(pattern)
    }
    message("  Added ", length(title_patterns), " title patterns")
  }

  if (!is.null(population_patterns)) {
    for (pattern in population_patterns) {
      add_population_pattern(pattern)
    }
    message("  Added ", length(population_patterns), " population patterns")
  }

  if (!is.null(footnote_patterns)) {
    for (pattern in footnote_patterns) {
      add_footnote_pattern(pattern)
    }
    message("  Added ", length(footnote_patterns), " footnote patterns")
  }

  if (!is.null(header_patterns)) {
    for (pattern in header_patterns) {
      add_table_header_pattern(pattern)
    }
    message("  Added ", length(header_patterns), " table header patterns")
  }

  message("Table patterns configured for: ", table_type)
}

#' Show current pattern configuration
#'
#' Display all currently configured patterns for debugging
#'
#' @export
show_pattern_config <- function() {
  cat("=== Current Pattern Configuration ===\n\n")

  cat("Title Patterns:\n")
  for (i in seq_along(get_title_patterns())) {
    cat("  ", i, ": ", get_title_patterns()[i], "\n")
  }

  cat("\nPopulation Patterns:\n")
  for (i in seq_along(get_population_patterns())) {
    cat("  ", i, ": ", get_population_patterns()[i], "\n")
  }

  cat("\nFootnote Patterns:\n")
  for (i in seq_along(get_footnote_patterns())) {
    cat("  ", i, ": ", get_footnote_patterns()[i], "\n")
  }

  cat("\nTable Header Patterns:\n")
  for (i in seq_along(get_table_header_patterns())) {
    cat("  ", i, ": ", get_table_header_patterns()[i], "\n")
  }

  cat("\nBIGN Patterns:\n")
  for (i in seq_along(get_bign_patterns())) {
    cat("  ", i, ": ", get_bign_patterns()[i], "\n")
  }

  cat("\nPercentage Indicator Patterns:\n")
  for (i in seq_along(get_percentage_patterns())) {
    cat("  ", i, ": ", get_percentage_patterns()[i], "\n")
  }

  cat("\nStandard Variable Names:\n")
  var_names <- get_variable_names()
  for (name in names(var_names)) {
    cat("  ", name, ": ", var_names[[name]], "\n")
  }

  cat("\nStandard Stat Names:\n")
  stat_names <- get_stat_names()
  for (name in names(stat_names)) {
    cat("  ", name, ": ", stat_names[[name]], "\n")
  }
}

#' Get percentage indicator patterns
#' @export
get_percentage_patterns <- function() {
  if (is.null(.stateful_patterns$PERCENTAGE_INDICATORS)) {
    .init_global_patterns()
  }
  return(.stateful_patterns$PERCENTAGE_INDICATORS)
}

#' Get standard variable names
#' @export
get_variable_names <- function() {
  if (is.null(.stateful_patterns$VARIABLE_NAMES)) {
    .init_global_patterns()
  }
  return(.stateful_patterns$VARIABLE_NAMES)
}

#' Get standard stat names
#' @export
get_stat_names <- function() {
  if (is.null(.stateful_patterns$STAT_NAMES)) {
    .init_global_patterns()
  }
  return(.stateful_patterns$STAT_NAMES)
}

#' Set BIGN patterns
#'
#' Updates the global BIGN patterns used for extraction.
#'
#' @param patterns Vector of regex patterns (from most to least restrictive)
#' @export
set_bign_patterns <- function(patterns) {
  .stateful_patterns$BIGN_PATTERNS <- patterns
  invisible(.stateful_patterns$BIGN_PATTERNS)
}

#' Add BIGN pattern (enhanced with pseudo-pattern support)
#'
#' Adds a new BIGN pattern. Accepts either regex patterns (legacy) or pseudo-patterns (recommended).
#'
#' @param pattern Pattern to add - can be regex or pseudo-pattern template
#' @param position Position to insert (1 = highest priority, NULL = append to end)
#' @param is_pseudo_pattern Whether the pattern is a pseudo-pattern template (default: auto-detect)
#' @export
add_bign_pattern <- function(pattern, position = NULL, is_pseudo_pattern = NULL) {
  # Auto-detect if pattern is pseudo-pattern (contains {placeholders})
  if (is.null(is_pseudo_pattern)) {
    is_pseudo_pattern <- grepl("\\{[^}]+\\}", pattern)
  }

  if (is_pseudo_pattern) {
    # Use pseudo-pattern system
    pattern_name <- paste0("custom_", length(get_bign_pseudo_patterns()) + 1)
    add_bign_pseudo_pattern(pattern_name, pattern)
    message("Added pseudo-pattern: ", pattern)
  } else {
    # Legacy regex pattern - convert to pseudo-pattern for consistency
    warning("Adding raw regex patterns is deprecated. Consider using pseudo-patterns like '{N} = {value}'")

    # Try to create a pseudo-pattern equivalent or add as custom
    pattern_name <- paste0("legacy_", length(get_bign_pseudo_patterns()) + 1)
    current_patterns <- get_bign_pseudo_patterns()

    # Add as a direct regex pattern
    current_patterns[[pattern_name]] <- list(
      template = paste0("legacy: ", pattern),
      regex = pattern,
      stats = c("value"),  # Generic stat name
      labels = c("N")      # Generic label
    )

    .stateful_patterns$BIGN_PSEUDO_PATTERNS <- current_patterns
  }

  invisible(get_bign_patterns())
}

#' Get statistical patterns
#'
#' Returns the current global statistical patterns.
#'
#' @return Named list of statistical pattern definitions
#' @export
get_stat_patterns <- function() {
  if (is.null(.stateful_patterns$STAT_PATTERNS)) {
    .init_global_patterns()
  }
  return(.stateful_patterns$STAT_PATTERNS)
}

#' Set statistical patterns
#'
#' Updates the global statistical patterns used for parsing.
#'
#' @param patterns Named list of pattern definitions
#' @export
set_stat_patterns <- function(patterns) {
  .stateful_patterns$STAT_PATTERNS <- patterns
  invisible(.stateful_patterns$STAT_PATTERNS)
}

#' Add statistical pattern
#'
#' Adds a new pattern to the global statistical patterns registry.
#'
#' @param name Pattern name/ID
#' @param template Template string with placeholders like "mean (sd)"
#' @param regex Regex to match the pattern with capture groups
#' @param stats Vector of stat_name values for each capture group
#' @param labels Vector of stat_label values for each capture group
#' @param priority Insert at beginning (TRUE) or end (FALSE) of pattern list
#' @export
add_stat_pattern <- function(name, template, regex, stats, labels, priority = TRUE) {
  current_patterns <- get_stat_patterns()
  new_pattern <- list(
    template = template,
    regex = regex,
    stats = stats,
    labels = labels
  )

  if (priority) {
    # Add at beginning for high priority
    .stateful_patterns$STAT_PATTERNS <- c(setNames(list(new_pattern), name), current_patterns)
  } else {
    # Add at end for low priority
    current_patterns[[name]] <- new_pattern
    .stateful_patterns$STAT_PATTERNS <- current_patterns
  }
  invisible(.stateful_patterns$STAT_PATTERNS)
}

#' Parse RTF table with state identification
#'
#' @param rtf_file Path to RTF file
#' @return List with sections: pre_header, header, table, footnotes
#' @export
parse_rtf_table_states <- function(rtf_file) {
  rtf_content <- readLines(rtf_file, warn = FALSE) |>
    paste(collapse = "\n")

  # Extract all rows with RTF structure info
  rows <- extract_rtf_rows_with_state(rtf_content)

  # Identify table states
  table_states <- identify_table_states(rows)

  # Extract each section
  sections <- extract_table_sections(rows, table_states)

  return(sections)
}

#' Extract RTF rows with state information
#'
#' @keywords internal
extract_rtf_rows_with_state <- function(rtf_content) {
  lines <- str_split(rtf_content, "\n")[[1]]

  # Find row markers
  trowd_lines <- which(grepl("\\\\trowd", lines))
  row_end_lines <- which(grepl("\\{\\\\row\\}", lines))

  rows <- list()

  for (i in seq_along(trowd_lines)) {
    start_line <- trowd_lines[i]
    end_line <- row_end_lines[row_end_lines > start_line][1]

    if (!is.na(end_line)) {
      row_content <- paste(lines[start_line:end_line], collapse = "\n")

      # Extract comprehensive row properties for state detection
      row_info <- list(
        index = length(rows) + 1,
        line_start = start_line,
        line_end = end_line,
        content = row_content,

        # Header indicators
        has_trhdr = grepl("\\\\trhdr", row_content),

        # Border information (indicates table structure)
        borders = extract_border_info(row_content),

        # Cell information
        cells = extract_cells_from_row(row_content),
        cell_positions = extract_cell_positions(row_content),

        # Content characteristics for state detection
        characteristics = analyze_row_characteristics(row_content)
      )

      rows[[length(rows) + 1]] <- row_info
    }
  }

  return(rows)
}

#' Analyze row characteristics for state detection
#'
#' @keywords internal
analyze_row_characteristics <- function(row_content) {
  cells <- extract_cells_from_row(row_content)
  cell_texts <- sapply(cells, function(c) trimws(c$text))
  non_empty_cells <- cell_texts[cell_texts != ""]

  characteristics <- list(
    total_cells = length(cells),
    non_empty_cells = length(non_empty_cells),
    has_borders = grepl("\\\\clbrdr", row_content),
    has_shading = grepl("\\\\clcbpat", row_content),

    # Content patterns (structural, not semantic)
    has_mostly_text = FALSE,
    has_mixed_content = FALSE,
    has_mostly_numbers = FALSE,
    is_mostly_empty = length(non_empty_cells) <= 1,

    # Table structure indicators
    spans_multiple_columns = length(non_empty_cells) > 1,
    has_single_spanning_cell = length(non_empty_cells) == 1 && length(cells) > 1
  )

  if (length(non_empty_cells) > 0) {
    # Analyze content types without semantic interpretation
    numeric_cells <- sum(grepl("^[0-9.,()% +-]+$", non_empty_cells))
    mixed_cells <- sum(grepl("[0-9].*[A-Za-z]|[A-Za-z].*[0-9]", non_empty_cells))
    text_cells <- length(non_empty_cells) - numeric_cells - mixed_cells

    total_content_cells <- length(non_empty_cells)
    characteristics$has_mostly_numbers <- numeric_cells / total_content_cells > 0.7
    characteristics$has_mixed_content <- mixed_cells / total_content_cells > 0.3
    characteristics$has_mostly_text <- text_cells / total_content_cells > 0.7
  }

  return(characteristics)
}

#' Identify table states based on RTF structure and border transitions
#'
#' @keywords internal
identify_table_states <- function(rows) {
  if (length(rows) == 0) {
    return(list())
  }

  # Find all rows with trhdr tags
  header_rows <- which(sapply(rows, function(r) {
    if (is.null(r$has_trhdr)) return(FALSE)
    r$has_trhdr
  }))

  if (length(header_rows) == 0) {
    return(identify_sections_without_headers(rows))
  }

  # Group consecutive header rows to identify header blocks
  header_blocks <- identify_header_blocks(rows, header_rows)

  # Use the FIRST header block only for state detection
  first_block <- header_blocks[[1]]

  sections <- list()
  sections$pre_header_end <- if (first_block$start > 1) first_block$start - 1 else 0
  sections$header_start <- first_block$start
  sections$header_end <- first_block$end

  # Find table section after first header block
  sections$table_start <- determine_table_start_with_borders(rows, first_block$end)
  sections$table_end <- determine_table_end_with_borders(rows, sections$table_start, header_blocks)

  # Footnotes start after table
  sections$footnotes_start <- if (sections$table_end < length(rows)) sections$table_end + 1 else length(rows) + 1

  return(sections)
}

#' Identify header blocks using border transitions
#'
#' @keywords internal
identify_header_blocks <- function(rows, header_rows) {
  if (length(header_rows) == 0) {
    return(list())
  }

  blocks <- list()
  current_block_start <- header_rows[1]

  # Handle single header row case
  if (length(header_rows) == 1) {
    blocks[[1]] <- list(
      start = header_rows[1],
      end = header_rows[1],
      header_rows = header_rows
    )
    return(blocks)
  }

  for (i in 1:(length(header_rows) - 1)) {
    current_row <- header_rows[i]
    next_row <- header_rows[i + 1]

    # Validate indices
    if (is.na(current_row) || is.na(next_row) ||
        current_row < 1 || current_row > length(rows) ||
        next_row < 1 || next_row > length(rows)) {
      next
    }

    # Check for gap between header rows (indicates new table section)
    gap <- next_row - current_row

    # Also check for border patterns that indicate section breaks
    current_borders <- rows[[current_row]]$borders
    next_borders <- rows[[next_row]]$borders

    # If gap > 2, or we see a border pattern indicating new section
    has_border_break <- FALSE
    if (!is.null(current_borders) && !is.null(next_borders)) {
      if (!is.null(current_borders$bottom) && !is.na(current_borders$bottom) &&
          !is.null(next_borders$top) && !is.na(next_borders$top)) {
        has_border_break <- current_borders$bottom > 0 && next_borders$top > 10
      }
    }

    # Ensure logical values
    gap_break <- !is.na(gap) && gap > 2
    section_break <- gap_break || isTRUE(has_border_break)

    if (section_break) {
      # End current block
      blocks[[length(blocks) + 1]] <- list(
        start = current_block_start,
        end = current_row,
        header_rows = header_rows[header_rows >= current_block_start & header_rows <= current_row]
      )

      # Start new block
      current_block_start <- next_row
    }
  }

  # Add final block
  blocks[[length(blocks) + 1]] <- list(
    start = current_block_start,
    end = header_rows[length(header_rows)],
    header_rows = header_rows[header_rows >= current_block_start & header_rows <= header_rows[length(header_rows)]]
  )

  return(blocks)
}

#' Determine table start using border information
#'
#' @keywords internal
determine_table_start_with_borders <- function(rows, header_end) {
  if (header_end >= length(rows)) {
    return(length(rows) + 1)
  }

  # Look for first row after header that has data content
  for (i in (header_end + 1):length(rows)) {
    row <- rows[[i]]

    # Skip empty rows immediately after header
    if (row$characteristics$is_mostly_empty) {
      next
    }

    # Look for row with typical data pattern
    if (row$characteristics$spans_multiple_columns &&
        !row$characteristics$is_mostly_empty) {
      return(i)
    }
  }

  return(length(rows) + 1)
}

#' Determine table end using border transitions and header blocks
#'
#' @keywords internal
determine_table_end_with_borders <- function(rows, table_start, header_blocks) {
  if (table_start > length(rows)) {
    return(length(rows))
  }

  # RULE: If we encounter a page break, we've reached footnotes - table ends before page break
  for (i in table_start:length(rows)) {
    row <- rows[[i]]
    if (grepl("\\\\page", row$content)) {
      return(max(i - 1, table_start))
    }
  }

  # If there are multiple header blocks, table ends before next header block
  if (length(header_blocks) > 1) {
    next_header_start <- header_blocks[[2]]$start

    # Find last data row before next header
    for (i in (next_header_start - 1):table_start) {
      if (i < table_start) break

      row <- rows[[i]]
      if (!row$characteristics$is_mostly_empty &&
          row$characteristics$spans_multiple_columns) {
        return(i)
      }
    }
  }

  # Single table - find natural end
  last_data_row <- table_start

  # Get reference structure from first few rows
  if (table_start + 2 <= length(rows)) {
    ref_rows <- rows[table_start:(table_start + 2)]
    expected_cols <- median(sapply(ref_rows, function(r) r$characteristics$total_cells))

    # Find where table structure breaks
    for (i in table_start:length(rows)) {
      row <- rows[[i]]

      # Check if this row fits table pattern
      if (row$characteristics$spans_multiple_columns &&
          abs(row$characteristics$total_cells - expected_cols) <= 1) {
        last_data_row <- i
      } else if (!row$characteristics$is_mostly_empty) {
        # Non-empty row that doesn't fit might be footnote
        break
      }
    }
  } else {
    last_data_row <- length(rows)
  }

  return(last_data_row)
}

#' Identify table boundaries based on structure
#'
#' @keywords internal
identify_table_boundaries <- function(rows, header_rows) {
  # Look for consistent table structure after headers
  boundaries <- list()

  if (length(header_rows) > 0) {
    header_end <- max(header_rows)

    # Find where consistent data rows start
    for (i in (header_end + 1):length(rows)) {
      row <- rows[[i]]

      # Skip empty separator rows
      if (row$characteristics$is_mostly_empty) {
        next
      }

      # Look for rows with multiple data cells (table data pattern)
      if (row$characteristics$spans_multiple_columns &&
          !row$characteristics$is_mostly_empty) {
        boundaries$table_start <- i
        break
      }
    }

    # Find where table structure breaks down
    if (!is.null(boundaries$table_start)) {
      table_start <- boundaries$table_start

      # Get the expected table structure from first few data rows
      expected_structure <- analyze_table_structure(rows[table_start:(table_start + 2)])

      # Find where structure changes significantly
      for (i in (table_start + 3):length(rows)) {
        if (i > length(rows)) break

        current_structure <- analyze_table_structure(rows[i])

        # If structure changes significantly, might be end of table
        if (is_structure_break(expected_structure, current_structure)) {
          boundaries$table_end <- i - 1
          break
        }
      }

      # If no clear break found, assume table goes to end
      if (is.null(boundaries$table_end)) {
        boundaries$table_end <- length(rows)
      }
    }
  }

  return(boundaries)
}

#' Determine where table data starts after headers
#'
#' @keywords internal
determine_table_start <- function(rows, header_end) {
  if (header_end >= length(rows)) {
    return(length(rows) + 1)
  }

  # Look for first row with multi-column data after headers
  for (i in (header_end + 1):length(rows)) {
    row <- rows[[i]]

    # Skip empty rows
    if (row$characteristics$is_mostly_empty) {
      next
    }

    # Look for data pattern: first column = text, others = data
    if (row$characteristics$spans_multiple_columns) {
      return(i)
    }
  }

  return(length(rows) + 1)
}

#' Determine where table data ends
#'
#' @keywords internal
determine_table_end <- function(rows, table_start) {
  if (table_start > length(rows)) {
    return(length(rows))
  }

  # Analyze first few table rows to understand structure
  reference_rows <- rows[table_start:min(table_start + 2, length(rows))]
  expected_cols <- median(sapply(reference_rows, function(r) r$characteristics$total_cells))

  # Find where structure breaks
  last_table_row <- table_start

  for (i in table_start:length(rows)) {
    row <- rows[[i]]

    # Check if this row fits table pattern
    if (row$characteristics$spans_multiple_columns &&
        abs(row$characteristics$total_cells - expected_cols) <= 1) {
      last_table_row <- i
    } else if (!row$characteristics$is_mostly_empty) {
      # Non-empty row that doesn't fit table pattern might be footnote
      break
    }
  }

  return(last_table_row)
}

#' Analyze table structure for consistency checking
#'
#' @keywords internal
analyze_table_structure <- function(rows) {
  if (!is.list(rows)) {
    rows <- list(rows)
  }

  structures <- lapply(rows, function(row) {
    list(
      cell_count = row$characteristics$total_cells,
      non_empty_count = row$characteristics$non_empty_cells,
      has_borders = row$characteristics$has_borders,
      content_type = determine_content_type(row$characteristics)
    )
  })

  # Return median/common structure
  cell_counts <- sapply(structures, function(s) s$cell_count)

  list(
    typical_cell_count = median(cell_counts),
    typical_content_type = names(sort(table(sapply(structures, function(s) s$content_type)), decreasing = TRUE))[1]
  )
}

#' Determine content type for structure analysis
#'
#' @keywords internal
determine_content_type <- function(characteristics) {
  if (characteristics$has_mostly_numbers) {
    return("data")
  } else if (characteristics$has_mixed_content) {
    return("mixed")
  } else if (characteristics$has_mostly_text) {
    return("text")
  } else {
    return("sparse")
  }
}

#' Check if there's a significant structure break
#'
#' @keywords internal
is_structure_break <- function(expected, current) {
  if (is.null(expected) || is.null(current)) {
    return(FALSE)
  }

  # Significant change in cell count
  cell_diff <- abs(expected$typical_cell_count - current$typical_cell_count)

  # Content type change from data to text (might indicate footnotes)
  content_change <- expected$typical_content_type == "data" && current$typical_content_type == "text"

  return(cell_diff > 2 || content_change)
}

#' Identify sections when no clear headers are found
#'
#' @keywords internal
identify_sections_without_headers <- function(rows) {
  # Use border and content analysis
  sections <- list()

  # Find first row with multiple columns (likely start of table)
  for (i in seq_along(rows)) {
    if (rows[[i]]$characteristics$spans_multiple_columns) {
      sections$table_start <- i
      break
    }
  }

  if (is.null(sections$table_start)) {
    # No clear table found
    sections$pre_header_end <- length(rows)
    sections$header_start <- length(rows) + 1
    sections$header_end <- length(rows)
    sections$table_start <- length(rows) + 1
    sections$table_end <- length(rows)
    sections$footnotes_start <- length(rows) + 1
  } else {
    sections$pre_header_end <- sections$table_start - 1
    sections$header_start <- sections$table_start
    sections$header_end <- sections$table_start
    sections$table_end <- determine_table_end(rows, sections$table_start)
    sections$footnotes_start <- sections$table_end + 1
  }

  return(sections)
}

#' Extract sections based on identified states
#'
#' @keywords internal
extract_table_sections <- function(rows, states) {
  sections <- list()

  # Pre-header section (title, population, etc.)
  if (states$pre_header_end > 0) {
    sections$pre_header <- extract_section_content(rows[1:states$pre_header_end])
  } else {
    sections$pre_header <- list()
  }

  # Header section
  if (states$header_start <= states$header_end && states$header_start <= length(rows)) {
    sections$header <- list(
      rows = rows[states$header_start:states$header_end],
      analysis = analyze_header_structure_state(rows[states$header_start:states$header_end])
    )
  } else {
    sections$header <- list(rows = list(), analysis = list(type = "none"))
  }

  # Table section
  if (states$table_start <= states$table_end && states$table_start <= length(rows)) {
    sections$table <- list(
      rows = rows[states$table_start:states$table_end],
      data = extract_table_data(rows[states$table_start:states$table_end])
    )
  } else {
    sections$table <- list(rows = list(), data = data.frame())
  }

  # Footnotes section
  if (states$footnotes_start <= length(rows)) {
    sections$footnotes <- extract_section_content(rows[states$footnotes_start:length(rows)])
  } else {
    sections$footnotes <- list()
  }

  # Add state boundaries for reference
  sections$states <- states

  return(sections)
}

#' Extract content from a section (pre-header or footnotes)
#'
#' @keywords internal
extract_section_content <- function(section_rows) {
  if (length(section_rows) == 0) {
    return(list(text = character(), rows = list()))
  }

  # Extract text content from each row
  content <- lapply(section_rows, function(row) {
    cells <- row$cells
    cell_texts <- sapply(cells, function(c) trimws(c$text))
    non_empty <- cell_texts[cell_texts != ""]

    if (length(non_empty) == 0) {
      return("")
    } else if (length(non_empty) == 1) {
      return(non_empty[1])
    } else {
      # Multiple cells - might be structured content
      return(paste(non_empty, collapse = " | "))
    }
  })

  text_content <- unlist(content)
  text_content <- text_content[text_content != ""]

  # Filter out table content that got misclassified as footnotes
  # Look for patterns that indicate table headers/data rather than footnotes
  filtered_content <- character()

  for (text in text_content) {
    # Use flexible pattern matching instead of hardcoded patterns
    is_table_content <- FALSE

    # Check against configurable table header patterns
    table_header_patterns <- get_table_header_patterns()
    for (pattern in table_header_patterns) {
      if (grepl(pattern, text, ignore.case = TRUE)) {
        is_table_content <- TRUE
        break
      }
    }

    # Treatment group headers with N= (keep this as it's structural)
    if (!is_table_content && grepl("\\{Arm [A-C]:", text) && grepl("N =", text)) {
      is_table_content <- TRUE
    }

    # Table data rows (structural pattern)
    if (!is_table_content && grepl("^\\\\\\\\", text) && grepl("\\d+", text) && grepl("[;|]", text)) {
      is_table_content <- TRUE
    }

    if (!is_table_content) {
      filtered_content <- c(filtered_content, text)
    }
  }

  # Remove duplicates (keep only unique footnotes)
  unique_content <- unique(filtered_content)

  return(list(
    text = unique_content,
    rows = section_rows
  ))
}

#' Analyze header structure for state-based approach
#'
#' @keywords internal
analyze_header_structure_state <- function(header_rows) {
  if (length(header_rows) == 0) {
    return(list(type = "none"))
  }

  # Extract top-left label
  top_left_label <- ""
  if (length(header_rows) > 0 && length(header_rows[[1]]$cells) > 0) {
    top_left_label <- trimws(header_rows[[1]]$cells[[1]]$text)
  }

  # Build column hierarchy based on cell positions
  hierarchy <- build_column_hierarchy_state(header_rows)

  # Determine structure type
  if (length(header_rows) >= 3) {
    structure_type <- "three_level"
  } else if (length(header_rows) == 2) {
    structure_type <- "two_level"
  } else {
    structure_type <- "single_level"
  }

  return(list(
    type = structure_type,
    levels = length(header_rows),
    top_left_label = top_left_label,
    column_hierarchy = hierarchy,
    header_rows = header_rows
  ))
}

#' Build column hierarchy using cell positions
#'
#' @keywords internal
build_column_hierarchy_state <- function(header_rows) {
  if (length(header_rows) == 0) {
    return(data.frame())
  }

  # Get the deepest level (last header row) as base
  base_row <- header_rows[[length(header_rows)]]

  if (is.null(base_row$cell_positions) || nrow(base_row$cell_positions) == 0) {
    return(data.frame())
  }

  # Start with data columns (skip first column which is usually row labels)
  data_cols <- base_row$cell_positions
  if (nrow(data_cols) > 1) {
    data_cols <- data_cols[2:nrow(data_cols), ]
  } else {
    return(data.frame())
  }

  # Initialize hierarchy
  hierarchy <- data.frame(
    column_index = data_cols$cell_num,
    right_pos = data_cols$right_pos,
    left_pos = data_cols$left_pos,
    stringsAsFactors = FALSE
  )

  # Add each header level
  for (level in seq_along(header_rows)) {
    level_row <- header_rows[[level]]
    level_cells <- sapply(level_row$cells, function(c) trimws(c$text))
    level_positions <- level_row$cell_positions

    col_name <- paste0("level_", level)
    hierarchy[[col_name]] <- ""

    # Map each data column to appropriate header cell based on position
    for (i in seq_len(nrow(hierarchy))) {
      data_pos <- hierarchy[i, ]

      # Find which header cell this data column falls under
      if (!is.null(level_positions) && nrow(level_positions) > 0) {
        for (j in seq_len(nrow(level_positions))) {
          header_pos <- level_positions[j, ]

          # Check if data column falls within header cell bounds
          if (data_pos$left_pos >= header_pos$left_pos &&
              data_pos$right_pos <= header_pos$right_pos) {
            if (j <= length(level_cells)) {
              hierarchy[i, col_name] <- level_cells[j]
            }
            break
          }
        }
      }
    }
  }

  return(hierarchy)
}

#' Extract table data as clean data frame
#'
#' @keywords internal
extract_table_data <- function(table_rows) {
  if (length(table_rows) == 0) {
    return(data.frame())
  }

  # Filter out footnote-like rows before processing
  filtered_rows <- Filter(function(row) {
    cell_texts <- sapply(row$cells, function(cell) trimws(cell$text))
    combined_text <- paste(cell_texts, collapse = " ")

    # Use flexible footnote pattern detection
    is_footnote <- FALSE

    # Check against configurable footnote patterns
    footnote_patterns <- get_footnote_patterns()
    for (pattern in footnote_patterns) {
      if (grepl(pattern, combined_text, ignore.case = TRUE)) {
        is_footnote <- TRUE
        break
      }
    }

    return(!is_footnote)
  }, table_rows)

  # Extract text from filtered rows
  row_data <- lapply(filtered_rows, function(row) {
    sapply(row$cells, function(cell) trimws(cell$text))
  })

  # Find maximum number of columns
  max_cols <- max(sapply(row_data, length))

  # Pad shorter rows
  row_data <- lapply(row_data, function(row) {
    if (length(row) < max_cols) {
      c(row, rep("", max_cols - length(row)))
    } else {
      row
    }
  })

  # Convert to data frame
  df <- as.data.frame(do.call(rbind, row_data), stringsAsFactors = FALSE)
  names(df) <- paste0("V", 1:ncol(df))

  return(df)
}

#' Convert state-based parsed table to ARD format
#'
#' @param table_sections Result from parse_rtf_table_states
#' @param config Configuration for parsing patterns
#' @param bign_patterns Vector of regex patterns for BIGN extraction (from most to least restrictive)
#' @return ARD formatted data
#' @export
state_table_to_ard <- function(table_sections, config = list(), bign_patterns = NULL) {
  if (is.null(table_sections$table) || nrow(table_sections$table$data) == 0) {
    return(data.frame())
  }

  # Extract table data and header structure
  table_data <- table_sections$table$data
  header_analysis <- table_sections$header$analysis
  table_rows <- table_sections$table$rows

  # Analyze indentation patterns in table rows
  row_analysis <- detect_table_indentation_patterns(table_rows)

  # Build hierarchical structure based on indentation
  hierarchical_structure <- build_hierarchical_structure(row_analysis)

  # Convert to ARD using hierarchy
  ard_data <- convert_to_ard_with_state_hierarchy(table_data, header_analysis, config)

  # Apply hierarchical structure to ARD data
  ard_data <- apply_hierarchical_structure_to_ard(ard_data, hierarchical_structure, row_analysis)

  # Apply statistical pattern parsing
  ard_data <- expand_state_statistics(ard_data, config, bign_patterns)

  # Filter out blank rows (those with stat values that are just backslashes)
  # These are typically separator rows from the RTF table structure
  if (nrow(ard_data) > 0) {
    ard_data <- ard_data |>
      dplyr::filter(
        !grepl("^\\\\+$", stat) & # Remove rows where stat is only backslashes
        !(stat_name == "other" & grepl("^\\\\+$", stat)) # Double check for "other" entries
      )
  }

  return(ard_data)
}

#' Convert data to ARD format using state-based header analysis
#'
#' @keywords internal
convert_to_ard_with_state_hierarchy <- function(data_rows, header_analysis, config) {
  if (nrow(data_rows) == 0) {
    return(data.frame())
  }

  # Start with basic pivot
  ard_base <- data_rows |>
    mutate(across(everything(), as.character)) |>
    rename(variable = 1) |>
    tidyr::pivot_longer(
      cols = -variable,
      names_to = "column_name",
      values_to = "stat"
    ) |>
    filter(!is.na(stat) & trimws(stat) != "") |>
    mutate(
      column_index = as.integer(str_extract(column_name, "\\d+"))
    )

  # Add hierarchy based on header structure
  if (header_analysis$type != "none" && !is.null(header_analysis$column_hierarchy)) {
    hierarchy <- header_analysis$column_hierarchy

    # Map column indices to hierarchy levels
    ard_data <- ard_base |>
      left_join(hierarchy, by = c("column_index" = "column_index"))

    # Detect and handle spanners vs actual group levels
    ard_data <- fix_spanner_hierarchy_state(ard_data, hierarchy, config)

    # Create ARD group structure based on corrected levels
    if ("group1_level" %in% names(ard_data)) {
      ard_data <- ard_data |>
        mutate(group1 = "TRT")
    } else if ("level_1" %in% names(hierarchy)) {
      ard_data <- ard_data |>
        mutate(
          group1 = "TRT",
          group1_level = clean_treatment_name(level_1)
        )
    }

    if ("group2_level" %in% names(ard_data)) {
      ard_data <- ard_data |>
        mutate(group2 = "GROUP2")
    } else if ("level_2" %in% names(ard_data) || "level_2" %in% names(hierarchy)) {
      if ("level_2" %in% names(ard_data)) {
        ard_data <- ard_data |>
          mutate(
            group2 = determine_group2_type_state(level_2, config),
            group2_level = clean_treatment_name(level_2)
          )
      }
    }

    if ("level_3" %in% names(hierarchy)) {
      ard_data <- ard_data |>
        mutate(
          group3 = determine_group3_type_state(level_3, config),
          group3_level = clean_treatment_name(level_3)
        )
    }

    # Add variable label from top-left
    if (nchar(header_analysis$top_left_label) > 0) {
      ard_data <- ard_data |>
        mutate(variable_label1 = header_analysis$top_left_label)
    }

    # Clean up columns and ensure stat_name and stat_label exist
    ard_data <- ard_data |>
      select(-column_name, -column_index, -starts_with("level_"))
    
    if (!"stat_name" %in% names(ard_data)) {
      ard_data$stat_name <- NA_character_
    }
    if (!"stat_label" %in% names(ard_data)) {
      ard_data$stat_label <- NA_character_
    }

  } else {
    # Simple structure without hierarchy
    ard_data <- ard_base |>
      mutate(
        group1 = "COLUMN",
        group1_level = column_name,
        stat_name = NA_character_,
        stat_label = NA_character_
      ) |>
      select(-column_name, -column_index)
  }

  return(ard_data)
}

#' Determine group2 type for state-based parsing
#'
#' @keywords internal
determine_group2_type_state <- function(level2_values, config) {
  if (!is.null(config$group2_name)) {
    return(config$group2_name)
  }

  # Pure position-based: if we have a level 2, it's simply GROUP2
  # No content analysis - structure only
  return("GROUP2")
}

#' Determine group3 type for state-based parsing
#'
#' @keywords internal
determine_group3_type_state <- function(level3_values, config) {
  if (!is.null(config$group3_name)) {
    return(config$group3_name)
  }

  # Pure position-based: if we have a level 3, it's simply GROUP3
  # No content analysis - structure only
  return("GROUP3")
}

#' Fix spanner hierarchy to correctly identify group levels vs spanners
#'
#' @keywords internal
fix_spanner_hierarchy_state <- function(ard_data, hierarchy, config) {
  # Identify common spanner patterns
  spanner_patterns <- c(
    "Number.*%.*of.*Subjects",
    "Any Grade.*Grade.*",
    "Overall.*Summary",
    "Safety.*Parameter"
  )

  # Check if level_1 contains spanner text
  if ("level_1" %in% names(hierarchy)) {
    level_1_values <- unique(hierarchy$level_1[hierarchy$level_1 != ""])
    is_level_1_spanner <- any(sapply(spanner_patterns, function(p) any(grepl(p, level_1_values, ignore.case = TRUE))))

    if (is_level_1_spanner) {
      # level_1 is a spanner, use level_2 as group1_level
      if ("level_2" %in% names(hierarchy)) {
        ard_data <- ard_data |>
          mutate(
            group1_level = clean_treatment_name(level_2),
            .keep = "all"
          ) |>
          select(-level_1, -level_2)
      }
    } else {
      # level_1 is actual group, use as group1_level
      ard_data <- ard_data |>
        mutate(
          group1_level = clean_treatment_name(level_1),
          .keep = "all"
        ) |>
        select(-level_1)

      # If there's level_2, it becomes group2_level
      if ("level_2" %in% names(hierarchy)) {
        ard_data <- ard_data |>
          mutate(
            group2_level = clean_treatment_name(level_2),
            .keep = "all"
          ) |>
          select(-level_2)
      }
    }
  }

  return(ard_data)
}

#' Clean treatment names by extracting just the treatment part
#'
#' @keywords internal
clean_treatment_name <- function(treatment_text) {
  if (is.null(treatment_text) || length(treatment_text) == 0) {
    return(treatment_text)
  }

  # Handle vectorized input
  result <- sapply(treatment_text, function(x) {
    if (is.na(x) || x == "") {
      return(x)
    }

    # Extract treatment name before (N = xxx) part
    cleaned <- str_replace(x, "\\s*\\(N\\s*=\\s*\\d+\\).*$", "")
    cleaned <- str_replace_all(cleaned, "\\{|\\}", "")
    cleaned <- str_trim(cleaned)

    return(cleaned)
  }, USE.NAMES = FALSE)

  return(result)
}

#' Expand statistics using state-based approach
#'
#' @param ard_data Data frame with ARD structure
#' @param config Configuration list
#' @param bign_patterns Vector of regex patterns for BIGN extraction
#' @keywords internal
expand_state_statistics <- function(ard_data, config, bign_patterns = NULL) {
  if (!"stat" %in% names(ard_data)) {
    return(ard_data)
  }

  # Process Big N headers first
  ard_data <- extract_state_bign_rows(ard_data, bign_patterns)

  # Process each row and expand statistics
  expanded_rows <- purrr::map_dfr(seq_len(nrow(ard_data)), function(i) {
    row <- ard_data[i, ]
    stat_value <- row$stat

    # Skip if already processed (e.g., Big N rows)
    if ("stat_name" %in% names(row) && !is.na(row$stat_name) && !is.null(row$stat_name) && row$stat_name != "") {
      return(row)
    }

    # Parse the statistic with variable context for pattern disambiguation
    variable_context <- if ("variable_level" %in% names(row)) row$variable_level else row$variable
    parsed_stats <- parse_state_stat_value_with_context(stat_value, variable_context, config)

    # If parsing resulted in multiple rows, replicate the other columns
    if (nrow(parsed_stats) > 1) {
      other_cols <- row[, !names(row) %in% c("stat", "stat_name", "stat_label")]
      expanded <- purrr::map_dfr(seq_len(nrow(parsed_stats)), function(j) {
        cbind(other_cols, parsed_stats[j, ], stringsAsFactors = FALSE)
      })
      return(expanded)
    } else {
      # Single statistic, update the existing row
      row$stat_name <- parsed_stats$stat_name[1]
      row$stat_label <- parsed_stats$stat_label[1]
      row$stat <- parsed_stats$stat[1]
      return(row)
    }
  })

  return(expanded_rows)
}

#' Extract Big N rows from group headers
#'
#' @keywords internal
extract_state_bign_rows <- function(ard_data, bign_patterns = NULL) {
  # Find rows with Big N patterns in group1_level
  if (!"group1_level" %in% names(ard_data)) {
    return(ard_data)
  }

  # Use global patterns if none provided
  if (is.null(bign_patterns)) {
    bign_patterns <- get_bign_patterns()
  }

  # Try patterns in order until we find matches
  big_n_rows <- NULL
  pattern_used <- NULL

  for (pattern in bign_patterns) {
    matches <- ard_data |>
      filter(grepl(pattern, group1_level, ignore.case = TRUE))

    if (nrow(matches) > 0) {
      pattern_used <- pattern

      # Extract N values using the matching pattern
      big_n_rows <- matches |>
        distinct(group1_level) |>
        mutate(
          n_value = str_extract(group1_level, pattern) |>
            str_extract("\\d+"),
          clean_group_level = str_extract(group1_level, paste0(".*?(?=", pattern, ")")) |>
            str_trim()
        ) |>
        filter(!is.na(n_value)) |>
        transmute(
          group1 = "TRT",
          group1_level = str_replace_all(clean_group_level, "\\{|\\}", "") |> str_trim(),
          variable = "BIGN",
          stat_name = "N",
          stat_label = "N",
          stat = n_value
        )
      break
    }
  }

  # If no pattern matched, return original data
  if (is.null(big_n_rows)) {
    return(ard_data)
  }

  # Clean group1_level in main data using the pattern that worked
  ard_data_clean <- ard_data |>
    mutate(
      group1_level = {
        extracted <- str_extract(group1_level, paste0(".*?(?=", pattern_used, ")")) |>
          str_trim()
        ifelse(extracted == "" | is.na(extracted), group1_level, extracted)
      } |>
        # Clean up RTF formatting characters
        str_replace_all("\\{|\\}", "") |>
        str_trim()
    )

  # Combine with Big N rows
  if (nrow(big_n_rows) > 0) {
    # Ensure compatible column structure
    common_cols <- intersect(names(big_n_rows), names(ard_data_clean))
    big_n_rows <- big_n_rows[common_cols]
    ard_data_clean <- ard_data_clean[common_cols]

    result <- bind_rows(big_n_rows, ard_data_clean)
  } else {
    result <- ard_data_clean
  }

  return(result)
}

#' Parse individual statistic values
#'
#' @keywords internal
parse_state_stat_value <- function(stat_value, config = list()) {
  # Use the pseudo-pattern parser
  patterns <- if (!is.null(config$stat_patterns)) config$stat_patterns else NULL
  return(parse_stat_value_pseudo(stat_value, patterns))
}

#' Parse statistic values with variable context for pattern disambiguation
#'
#' @keywords internal
parse_state_stat_value_with_context <- function(stat_value, variable_context, config = list()) {
  # Use context-aware pseudo-pattern parser
  patterns <- if (!is.null(config$stat_patterns)) config$stat_patterns else NULL
  return(parse_stat_value_pseudo_with_context(stat_value, variable_context, patterns))
}

#' Convert RTF to ARD JSON using state-based parser
#'
#' @param rtf_file Path to RTF file
#' @param output_file Optional output file path
#' @param config Optional configuration
#' @param bign_patterns Vector of regex patterns for BIGN extraction (from most to least restrictive)
#' @export
rtf_to_ard_json <- function(rtf_file, output_file = NULL, config = list(), bign_patterns = NULL) {
  if (is.null(output_file)) {
    output_file <- str_replace(rtf_file, "\\.rtf$", "_ard.json")
  }

  # Parse RTF using state-based approach
  table_sections <- parse_rtf_table_states(rtf_file)

  # Convert to ARD
  ard_data <- state_table_to_ard(table_sections, config, bign_patterns)

  # Extract metadata from pre-header section
  metadata <- extract_state_metadata(table_sections, rtf_file)

  # Create JSON structure
  json_output <- list(
    metadata = metadata,
    ard_data = ard_data,
    summary = list(
      total_rows = nrow(ard_data),
      unique_groups = length(unique(ard_data$group1_level)),
      unique_variables = length(unique(if ("variable_level" %in% names(ard_data)) ard_data$variable_level else ard_data$variable)),
      stat_types = unique(ard_data$stat_name),
      hierarchical_variables = sum(grepl(" - ", if ("variable_level" %in% names(ard_data)) ard_data$variable_level else ard_data$variable)),
      group_levels = sum(!is.na(ard_data[grepl("^group[2-9]", names(ard_data))]))
    )
  )

  # Write JSON
  jsonlite::write_json(json_output, output_file, pretty = TRUE, auto_unbox = TRUE)

  message("State-based JSON ARD file created: ", basename(output_file))
  message("  - Rows: ", json_output$summary$total_rows)
  message("  - Groups: ", json_output$summary$unique_groups)

  return(invisible(json_output))
}

#' Extract metadata from state-based sections
#'
#' @keywords internal
extract_state_metadata <- function(table_sections, rtf_file) {
  metadata <- list(
    source_file = basename(rtf_file),
    conversion_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    conversion_method = "rtf_state_parser"
  )

  # Extract title and population from pre-header using flexible patterns
  if (!is.null(table_sections$pre_header) && length(table_sections$pre_header$text) > 0) {
    pre_header_text <- table_sections$pre_header$text

    # Extract title using flexible patterns
    title_patterns <- get_title_patterns()
    for (pattern in title_patterns) {
      title_matches <- pre_header_text[grepl(pattern, pre_header_text, ignore.case = TRUE)]
      if (length(title_matches) > 0) {
        metadata$title <- title_matches[1]
        break
      }
    }

    # Extract population using flexible patterns
    population_patterns <- get_population_patterns()
    for (pattern in population_patterns) {
      pop_matches <- pre_header_text[grepl(pattern, pre_header_text, ignore.case = TRUE)]
      if (length(pop_matches) > 0) {
        # Extract the actual matched text, not just the pattern
        match_result <- regmatches(pop_matches[1], regexpr(pattern, pop_matches[1], ignore.case = TRUE))
        if (length(match_result) > 0) {
          metadata$population <- match_result
        }
        break
      }
    }
  }

  # Extract footnotes if available
  if (!is.null(table_sections$footnotes) && length(table_sections$footnotes$text) > 0) {
    metadata$footnotes <- table_sections$footnotes$text
  }

  return(metadata)
}

# Include helper functions from previous file
source_if_exists <- function(file) {
  if (file.exists(file)) {
    source(file)
  }
}

# Include the cell extraction functions
extract_cells_from_row <- function(row_content) {
  cell_parts <- str_split(row_content, "\\\\pard")[[1]]

  cells <- list()

  for (part in cell_parts) {
    if (grepl("\\{.*?\\\\cell", part)) {
      text_match <- str_extract(part, "\\{([^\\\\]*(?:\\\\(?!cell)[^\\\\]*)*)\\\\cell")

      if (!is.na(text_match)) {
        text <- str_replace(text_match, "^\\{", "")
        text <- str_replace(text, "\\\\cell.*$", "")
        text <- str_replace_all(text, "\\\\line", " ")
        text <- str_replace_all(text, "\\\\[a-z]+[0-9]*\\s*", "")
        text <- str_replace_all(text, "~", "")
        text <- str_trim(text)

        cells[[length(cells) + 1]] <- list(
          index = length(cells) + 1,
          text = text,
          raw = part
        )
      }
    }
  }

  return(cells)
}

extract_cell_positions <- function(row_content) {
  cellx_pattern <- "\\\\cellx(-?\\d+)"
  cellx_matches <- str_extract_all(row_content, cellx_pattern)[[1]]

  if (length(cellx_matches) == 0) {
    return(NULL)
  }

  positions <- as.integer(str_replace(cellx_matches, "\\\\cellx", ""))

  cell_info <- data.frame(
    cell_num = seq_along(positions),
    right_pos = positions,
    left_pos = c(0, positions[-length(positions)]),
    width = c(positions[1], diff(positions))
  )

  return(cell_info)
}

extract_border_info <- function(row_content) {
  borders <- list()

  if (grepl("\\\\clbrdrt", row_content)) {
    top_width <- str_extract(row_content, "\\\\clbrdrt\\\\brdrs\\\\brdrw(\\d+)")
    if (!is.na(top_width)) {
      borders$top <- as.integer(str_extract(top_width, "\\d+$"))
    }
  }

  if (grepl("\\\\clbrdrb", row_content)) {
    bottom_width <- str_extract(row_content, "\\\\clbrdrb\\\\brdrs\\\\brdrw(\\d+)")
    if (!is.na(bottom_width)) {
      borders$bottom <- as.integer(str_extract(bottom_width, "\\d+$"))
    }
  }

  return(borders)
}

#' Extract RTF indentation information from row content
#'
#' This function detects indentation level based on RTF structural markers
#' rather than text-based spacing, making it robust across different RTF sources.
#'
#' @param row_content Raw RTF content for a single row
#' @return List with indentation information
#' @keywords internal
extract_rtf_indentation <- function(row_content) {
  indent_info <- list(
    level = 0,
    left_indent = 0,
    first_line_indent = 0,
    cell_left_margin = 0,
    method = "none"
  )

  # Method 1: Visual indentation markers (check first cell text)
  cells <- extract_cells_from_row(row_content)
  if (length(cells) > 0) {
    first_cell_text <- trimws(cells[[1]]$text)

    # Count leading backslashes as indentation levels
    if (grepl("^\\\\\\\\", first_cell_text)) {
      # Count the number of leading backslash pairs
      leading_backslashes <- str_extract(first_cell_text, "^\\\\+")
      if (!is.na(leading_backslashes)) {
        # Each pair of backslashes represents one indentation level
        indent_level <- nchar(leading_backslashes) / 2
        indent_info$level <- floor(indent_level)
        indent_info$method <- "visual_backslash"
        return(indent_info)
      }
    }

    # Check for other visual indentation patterns (spaces, tabs, other markers)
    if (grepl("^\\s{4,}", first_cell_text)) {
      # 4+ spaces = 1 level, 8+ spaces = 2 levels, etc.
      leading_spaces <- str_extract(first_cell_text, "^\\s+")
      if (!is.na(leading_spaces)) {
        indent_info$level <- floor(nchar(leading_spaces) / 4)
        indent_info$method <- "visual_spaces"
        return(indent_info)
      }
    }
  }

  # Method 2: Paragraph left indent (\li)
  li_match <- str_extract(row_content, "\\\\li(-?\\d+)")
  if (!is.na(li_match)) {
    indent_info$left_indent <- as.integer(str_extract(li_match, "-?\\d+"))
    indent_info$method <- "paragraph_indent"
  }

  # Method 3: First line indent (\fi)
  fi_match <- str_extract(row_content, "\\\\fi(-?\\d+)")
  if (!is.na(fi_match)) {
    indent_info$first_line_indent <- as.integer(str_extract(fi_match, "-?\\d+"))
    if (indent_info$method == "none") indent_info$method <- "first_line_indent"
  }

  # Method 4: Cell left padding (\clpadl)
  clpadl_match <- str_extract(row_content, "\\\\clpadl(-?\\d+)")
  if (!is.na(clpadl_match)) {
    indent_info$cell_left_margin <- as.integer(str_extract(clpadl_match, "-?\\d+"))
    if (indent_info$method == "none") indent_info$method <- "cell_padding"
  }

  # Method 5: Cell position analysis (compare first cell position to reference)
  cell_positions <- extract_cell_positions(row_content)
  if (!is.null(cell_positions) && nrow(cell_positions) > 0) {
    first_cell_left <- cell_positions$left_pos[1]

    # If first cell doesn't start at 0, it indicates indentation
    if (first_cell_left > 0 && indent_info$method == "none") {
      indent_info$cell_left_margin <- first_cell_left
      indent_info$method <- "cell_position"
    }
  }

  # Calculate logical indentation level for non-visual methods
  if (indent_info$method != "visual_backslash" && indent_info$method != "visual_spaces") {
    indent_info$level <- calculate_indent_level(indent_info)
  }

  return(indent_info)
}

#' Calculate logical indentation level from RTF markers
#'
#' @param indent_info List from extract_rtf_indentation
#' @return Integer indentation level (0 = no indent, 1+ = nested levels)
#' @keywords internal
calculate_indent_level <- function(indent_info) {
  # Base indentation thresholds (in twips - RTF units)
  # 1 inch = 1440 twips, so these are rough guidelines
  base_indent_threshold <- 360  # ~0.25 inch
  indent_increment <- 360       # ~0.25 inch per level

  total_indent <- 0

  # Combine different indentation sources
  if (indent_info$left_indent > 0) {
    total_indent <- max(total_indent, indent_info$left_indent)
  }

  if (indent_info$first_line_indent > 0) {
    total_indent <- max(total_indent, indent_info$first_line_indent)
  }

  if (indent_info$cell_left_margin > 0) {
    total_indent <- max(total_indent, indent_info$cell_left_margin)
  }

  # Convert to logical levels
  if (total_indent < base_indent_threshold) {
    return(0)  # No indentation
  } else {
    return(floor((total_indent - base_indent_threshold) / indent_increment) + 1)
  }
}

#' Detect indentation patterns in table rows
#'
#' Analyzes all table rows to identify hierarchical indentation patterns
#' using RTF structural information.
#'
#' @param table_rows List of table row objects from state parser
#' @return Data frame with row indices and indentation levels
#' @keywords internal
detect_table_indentation_patterns <- function(table_rows) {
  if (length(table_rows) == 0) {
    return(data.frame(row_index = integer(), indent_level = integer(),
                     has_values = logical(), first_cell_text = character()))
  }

  # Extract indentation and content info for each row
  row_analysis <- purrr::map_dfr(seq_along(table_rows), function(i) {
    row <- table_rows[[i]]

    # Get indentation from RTF structure
    indent_info <- extract_rtf_indentation(row$content)

    # Check if row has statistical values (non-empty cells beyond first)
    cells <- row$cells
    if (length(cells) <= 1) {
      has_values <- FALSE
      first_cell_text <- ""
    } else {
      # Check if there are non-empty values in columns 2+
      value_cells <- sapply(cells[2:length(cells)], function(c) trimws(c$text))
      has_values <- any(value_cells != "" & !is.na(value_cells))
      first_cell_text <- if (length(cells) > 0) trimws(cells[[1]]$text) else ""
    }

    data.frame(
      row_index = i,
      indent_level = indent_info$level,
      has_values = has_values,
      first_cell_text = first_cell_text,
      indent_method = indent_info$method,
      left_indent = indent_info$left_indent,
      stringsAsFactors = FALSE
    )
  })

  return(row_analysis)
}

#' Build hierarchical variable labels and group structures
#'
#' Applies the two hierarchy rules based on indentation patterns:
#' Rule 1: Empty parent + indented children = concatenated variable labels
#' Rule 2: Value parent + indented children = nested group structure
#'
#' @param row_analysis Data frame from detect_table_indentation_patterns
#' @return List with variable_hierarchy and group_hierarchy information
#' @keywords internal
build_hierarchical_structure <- function(row_analysis) {
  if (nrow(row_analysis) == 0) {
    return(list(variable_hierarchy = data.frame(), group_hierarchy = data.frame()))
  }

  # Initialize tracking structures
  variable_hierarchy <- data.frame(
    row_index = integer(),
    variable_path = character(),
    hierarchy_type = character(),
    parent_rows = character(),
    stringsAsFactors = FALSE
  )

  group_hierarchy <- data.frame(
    row_index = integer(),
    group_level = integer(),
    group_name = character(),
    group_value = character(),
    parent_groups = character(),
    stringsAsFactors = FALSE
  )

  # Track the current hierarchy stack
  label_stack <- character()  # For variable label concatenation
  group_stack <- character()  # For group nesting

  for (i in seq_len(nrow(row_analysis))) {
    current_row <- row_analysis[i, ]
    current_level <- current_row$indent_level
    current_text <- current_row$first_cell_text
    has_values <- current_row$has_values

    # Skip empty rows
    if (current_text == "" || is.na(current_text)) {
      next
    }

    # Adjust stacks to current indentation level
    label_stack <- adjust_stack_to_level(label_stack, current_level)
    group_stack <- adjust_stack_to_level(group_stack, current_level)

    if (has_values) {
      # This row has statistical values

      # Check if this should create a group hierarchy entry
      if (current_level > 0) {
        # This is an indented row with values - add to group hierarchy
        group_entry <- data.frame(
          row_index = i,
          group_level = current_level,
          group_name = if (length(group_stack) > 0) group_stack[length(group_stack)] else "GROUP",
          group_value = current_text,
          parent_groups = paste(group_stack, collapse = " > "),
          stringsAsFactors = FALSE
        )
        group_hierarchy <- rbind(group_hierarchy, group_entry)
      }

      # Create variable label (concatenated if needed)
      if (length(label_stack) > 0) {
        # Rule 1: Concatenate with parent labels
        # Clean the current text by removing visual indentation markers
        clean_current_text <- str_replace(current_text, "^\\\\+", "") |> str_trim()
        variable_path <- paste(c(label_stack, clean_current_text), collapse = " - ")
        hierarchy_type <- "concatenated"
      } else {
        # No parent labels to concatenate
        # Clean the current text by removing visual indentation markers
        clean_current_text <- str_replace(current_text, "^\\\\+", "") |> str_trim()
        variable_path <- clean_current_text
        hierarchy_type <- "direct"
      }

      variable_entry <- data.frame(
        row_index = i,
        variable_path = variable_path,
        hierarchy_type = hierarchy_type,
        parent_rows = paste(label_stack, collapse = " > "),
        stringsAsFactors = FALSE
      )
      variable_hierarchy <- rbind(variable_hierarchy, variable_entry)

      # If this row has values and will have children, add to group stack
      if (i < nrow(row_analysis)) {
        next_levels <- row_analysis$indent_level[(i+1):nrow(row_analysis)]
        next_levels <- next_levels[!is.na(next_levels)]
        if (length(next_levels) > 0 && any(next_levels > current_level)) {
          # This row will have indented children with values
          group_stack <- c(group_stack, current_text)
        }
      }

    } else {
      # This row has NO statistical values - it's a label-only row
      # Add to label stack for Rule 1 (variable concatenation)
      label_stack <- c(label_stack, current_text)

      # Also could be a group header for Rule 2
      # We'll determine this when we see children
    }
  }

  return(list(
    variable_hierarchy = variable_hierarchy,
    group_hierarchy = group_hierarchy
  ))
}

#' Adjust hierarchy stack to match current indentation level
#'
#' @param stack Character vector representing current hierarchy
#' @param target_level Target indentation level
#' @return Adjusted stack
#' @keywords internal
adjust_stack_to_level <- function(stack, target_level) {
  if (target_level <= 0) {
    return(character())
  }

  if (length(stack) > target_level) {
    # We've moved to a shallower level - trim the stack
    return(stack[1:target_level])
  } else if (length(stack) == target_level) {
    # Same level - replace the last item (will be done by caller)
    return(stack[1:(target_level-1)])
  } else {
    # Deeper level - stack is already correct size or will be extended
    return(stack)
  }
}

#' Apply hierarchical structure to ARD data
#'
#' Updates ARD data frame with hierarchical variable labels and group structure
#' based on the detected indentation patterns.
#'
#' @param ard_data Base ARD data frame
#' @param hierarchical_structure Output from build_hierarchical_structure
#' @param row_analysis Output from detect_table_indentation_patterns
#' @return Updated ARD data frame with hierarchical information
#' @keywords internal
apply_hierarchical_structure_to_ard <- function(ard_data, hierarchical_structure, row_analysis) {
  if (nrow(ard_data) == 0) {
    return(ard_data)
  }

  variable_hierarchy <- hierarchical_structure$variable_hierarchy
  group_hierarchy <- hierarchical_structure$group_hierarchy

  # Create mapping from original variable names to hierarchical variable paths
  if (nrow(variable_hierarchy) > 0) {
    # Map original variables to hierarchical paths
    for (i in seq_len(nrow(variable_hierarchy))) {
      var_info <- variable_hierarchy[i, ]
      row_idx <- var_info$row_index
      original_variable <- row_analysis$first_cell_text[row_idx]
      hierarchical_path <- var_info$variable_path

      # Update all ARD rows with this variable in variable_level
      if ("variable_level" %in% names(ard_data)) {
        ard_data$variable_level[ard_data$variable_level == original_variable] <- hierarchical_path
      } else {
        # Create variable_level column if it doesn't exist
        ard_data$variable_level <- ard_data$variable
        ard_data$variable_level[ard_data$variable_level == original_variable] <- hierarchical_path
      }
    }
  }

  # Apply group hierarchy
  if (nrow(group_hierarchy) > 0) {
    # Add additional group columns for nested structure
    max_group_level <- max(group_hierarchy$group_level, na.rm = TRUE)

    for (level in 1:max_group_level) {
      group_col_name <- paste0("group", level + 1)  # group1 is already TRT
      group_level_col_name <- paste0("group", level + 1, "_level")

      if (!group_col_name %in% names(ard_data)) {
        ard_data[[group_col_name]] <- NA_character_
        ard_data[[group_level_col_name]] <- NA_character_
      }

      # Fill in group information for relevant rows
      level_groups <- group_hierarchy[group_hierarchy$group_level == level, ]
      for (j in seq_len(nrow(level_groups))) {
        group_info <- level_groups[j, ]
        row_idx <- group_info$row_index
        original_variable <- row_analysis$first_cell_text[row_idx]

        # Find ARD rows that match this variable in variable_level
        variable_col <- if ("variable_level" %in% names(ard_data)) "variable_level" else "variable"
        matching_rows <- which(ard_data[[variable_col]] == group_info$group_value |
                              grepl(paste0("\\Q", group_info$group_value, "\\E"), ard_data[[variable_col]]))

        if (length(matching_rows) > 0) {
          ard_data[matching_rows, group_col_name] <- group_info$group_name
          ard_data[matching_rows, group_level_col_name] <- group_info$group_value
        }
      }
    }
  }

  return(ard_data)
}