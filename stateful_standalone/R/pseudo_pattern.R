#' Simplified Pseudo-Pattern System V3 - Using Simple Regex
#'
#' Converts user-friendly pseudo-patterns like "n in (pct%)" into regex patterns
#' using simple non-whitespace capture groups.
#'
#' @name pseudo-pattern-v3
NULL

#' Convert pseudo-pattern to regex pattern (ultra-simplified)
#'
#' @param template Character string with placeholders, e.g., "n (pct%)"
#' @return List with regex pattern and metadata
#' @export
pseudo_to_regex <- function(template) {
  # Validate input
  if (!is.character(template) || length(template) != 1) {
    stop("Template must be a single character string")
  }

  # Extract placeholders
  placeholder_pattern <- "\\{([^}]+)\\}"
  matches <- gregexpr(placeholder_pattern, template)

  if (matches[[1]][1] == -1) {
    stop("No placeholders found in template. Use {name} format for placeholders.")
  }

  # Get placeholder names
  placeholder_matches <- regmatches(template, matches)[[1]]
  stat_names <- gsub("\\{|\\}", "", placeholder_matches)

  # Build regex by replacing placeholders
  regex_pattern <- template

  # Escape special regex characters EXCEPT our placeholders
  # First, temporarily replace placeholders
  for (i in seq_along(placeholder_matches)) {
    regex_pattern <- gsub(
      placeholder_matches[i],
      paste0("<<<PLACEHOLDER", i, ">>>"),
      regex_pattern,
      fixed = TRUE
    )
  }

  # Escape regex special characters
  special_chars <- c("(", ")", "[", "]", ".", "+", "*", "?", "^", "$", "|", "\\")
  for (char in special_chars) {
    regex_pattern <- gsub(char, paste0("\\", char), regex_pattern, fixed = TRUE)
  }

  # Now replace placeholders with capture groups
  # Use pattern that matches numbers/decimals (positive or negative)
  for (i in seq_along(placeholder_matches)) {
    # Create named capture group
    group_name <- stat_names[i]
    regex_pattern <- gsub(
      paste0("<<<PLACEHOLDER", i, ">>>"),
      paste0("(?<", group_name, ">[+-]?\\d+(?:\\.\\d+)?)"),
      regex_pattern,
      fixed = TRUE
    )
  }

  # Replace whitespace with \\s* for flexibility
  regex_pattern <- gsub("\\s+", "\\\\s*", regex_pattern)

  # Make % optional
  regex_pattern <- gsub("%", "%?", regex_pattern, fixed = TRUE)

  # Add anchors
  regex_pattern <- paste0("^\\s*", regex_pattern, "\\s*$")

  # Return structured result
  return(list(
    template = template,
    regex = regex_pattern,
    stats = stat_names,
    labels = create_default_labels(stat_names)
  ))
}

#' Alternative: Use numbered capture groups for compatibility
#' @param template Character string with placeholders like "n (pct%)"
#' @return List with regex pattern and metadata
#' @export
pseudo_to_regex_simple <- function(template) {
  # Extract placeholders
  placeholder_pattern <- "\\{([^}]+)\\}"
  matches <- gregexpr(placeholder_pattern, template)

  if (matches[[1]][1] == -1) {
    stop("No placeholders found in template")
  }

  # Get placeholder names
  placeholder_matches <- regmatches(template, matches)[[1]]
  stat_names <- gsub("\\{|\\}", "", placeholder_matches)

  # Start with the template
  regex_pattern <- template

  # Escape special characters (but preserve our placeholders)
  # Replace placeholders with markers first
  for (i in seq_along(placeholder_matches)) {
    regex_pattern <- sub(
      fixed = TRUE,
      pattern = placeholder_matches[i],
      replacement = paste0("<<<MARKER", i, ">>>"),
      x = regex_pattern
    )
  }

  # Escape parentheses and other special chars
  regex_pattern <- gsub("(", "\\(", regex_pattern, fixed = TRUE)
  regex_pattern <- gsub(")", "\\)", regex_pattern, fixed = TRUE)
  regex_pattern <- gsub(".", "\\.", regex_pattern, fixed = TRUE)
  regex_pattern <- gsub("[", "\\[", regex_pattern, fixed = TRUE)
  regex_pattern <- gsub("]", "\\]", regex_pattern, fixed = TRUE)
  regex_pattern <- gsub("+", "\\+", regex_pattern, fixed = TRUE)
  regex_pattern <- gsub("*", "\\*", regex_pattern, fixed = TRUE)
  regex_pattern <- gsub("?", "\\?", regex_pattern, fixed = TRUE)

  # Replace markers with capture groups
  # Use pattern that matches numbers/decimals (positive or negative)
  for (i in seq_along(placeholder_matches)) {
    regex_pattern <- sub(
      fixed = TRUE,
      pattern = paste0("<<<MARKER", i, ">>>"),
      replacement = "([+-]?\\d+(?:\\.\\d+)?)",  # Matches: -123, 123, -12.34, 12.34
      x = regex_pattern
    )
  }

  # Handle whitespace - make it flexible
  regex_pattern <- gsub("\\s+", "\\\\s*", regex_pattern)

  # Make % optional
  regex_pattern <- gsub("%", "%?", regex_pattern, fixed = TRUE)

  # Add anchors
  regex_pattern <- paste0("^\\s*", regex_pattern, "\\s*$")

  return(list(
    template = template,
    regex = regex_pattern,
    stats = stat_names,
    labels = create_default_labels(stat_names)
  ))
}

#' Create default labels from stat names
#' @keywords internal
create_default_labels <- function(stat_names) {
  label_map <- c(
    n = "n",
    pct = "pct",
    mean = "Mean",
    sd = "SD",
    se = "SE",
    median = "Median",
    q1 = "Q1",
    q3 = "Q3",
    min = "Min",
    max = "Max",
    ci_lower = "CI Lower",
    ci_upper = "CI Upper",
    hr = "Hazard Ratio",
    or = "Odds Ratio",
    rr = "Risk Ratio",
    p = "p-value",
    total = "Total",
    estimate = "Estimate"
  )

  sapply(stat_names, function(name) {
    if (name %in% names(label_map)) {
      label_map[[name]]
    } else {
      name
    }
  })
}

#' Update statistical patterns to use pseudo-patterns
#' @export
update_stat_patterns_to_pseudo <- function() {
  # Define patterns using pseudo-pattern syntax
  pseudo_patterns <- list(
    # Most common patterns first (for greedy matching)
    "n_pct" = "{n} ({pct}%)",  # Handles all variations: space/no-space, %/no-%
    "mean_sd" = "{mean} ({sd})",
    "mean_se" = "{mean} ({se})",
    "median_range" = "{median} ({min}, {max})",
    "median_iqr" = "{median} ({q1}, {q3})",
    "median_q1_q3" = "{median} ({q1} - {q3})",
    "range" = "{min} - {max}",
    "range_comma" = "{min}, {max}",
    "ci" = "{estimate} ({ci_lower}, {ci_upper})",
    "ci_dash" = "{estimate} ({ci_lower}-{ci_upper})",
    "hr_ci" = "{hr} ({ci_lower}-{ci_upper})",
    "count_only" = "{n}",
    "pct_only" = "{pct}%",
    "count_total" = "{n}/{total}",
    "mean_sd_n" = "{mean} ({sd}) [{n}]"
  )

  # Convert each pseudo-pattern to full pattern definition
  patterns <- list()
  for (name in names(pseudo_patterns)) {
    template <- pseudo_patterns[[name]]
    pattern_info <- pseudo_to_regex_simple(template)  # Use simple version

    patterns[[name]] <- list(
      template = template,
      regex = pattern_info$regex,
      stats = pattern_info$stats,
      labels = pattern_info$labels
    )
  }

  # Update global patterns
  set_stat_patterns(patterns)

  invisible(patterns)
}

#' Parse statistic value using pseudo-pattern system with context awareness
#' @param stat_value Character string containing the statistic to parse
#' @param variable_context Variable name/label for context-based pattern selection
#' @param patterns Optional list of patterns to use (if NULL, uses global patterns)
#' @return Data frame with parsed statistics
#' @keywords internal
parse_stat_value_pseudo_with_context <- function(stat_value, variable_context = NULL, patterns = NULL) {
  stat_value <- trimws(as.character(stat_value))

  # Skip if empty or NA
  if (is.na(stat_value) || stat_value == "") {
    return(data.frame(
      stat = stat_value,
      stat_name = "missing",
      stat_label = "Missing",
      stringsAsFactors = FALSE
    ))
  }

  # Get patterns
  if (is.null(patterns)) {
    patterns <- get_stat_patterns()
    # Initialize pseudo-patterns if not already done
    if (length(patterns) <= 2) {
      update_stat_patterns_to_pseudo()
      patterns <- get_stat_patterns()
    }
  }

  # Reorder patterns based on variable context for better matching
  if (!is.null(variable_context)) {
    patterns <- reorder_patterns_by_context(patterns, variable_context)
  }

  # Try each pattern (greedy - first match wins)
  for (pattern_name in names(patterns)) {
    pattern <- patterns[[pattern_name]]

    if (grepl(pattern$regex, stat_value)) {
      # Extract matches
      matches <- regmatches(stat_value, regexec(pattern$regex, stat_value))[[1]]

      if (length(matches) > 1) {
        # matches[1] is full match, rest are capture groups
        values <- matches[-1]

        # Clean up values - remove trailing % from percentages
        for (i in seq_along(values)) {
          if (pattern$stats[i] == "pct" && grepl("%$", values[i])) {
            values[i] <- sub("%$", "", values[i])
          }
        }

        # Create result with one row per statistic
        result <- data.frame(
          stat = values,
          stat_name = pattern$stats,
          stat_label = pattern$labels,
          stringsAsFactors = FALSE
        )

        return(result)
      }
    }
  }

  # No pattern matched - return as "other"
  return(data.frame(
    stat = stat_value,
    stat_name = "other",
    stat_label = "Other",
    stringsAsFactors = FALSE
  ))
}

#' Parse statistic value using pseudo-pattern system
#' @param stat_value Character string containing the statistic to parse
#' @param patterns Optional list of patterns to use (if NULL, uses global patterns)
#' @return Data frame with parsed statistics
#' @export
parse_stat_value_pseudo <- function(stat_value, patterns = NULL) {
  stat_value <- trimws(as.character(stat_value))

  # Skip if empty or NA
  if (is.na(stat_value) || stat_value == "") {
    return(data.frame(
      stat = stat_value,
      stat_name = "missing",
      stat_label = "Missing",
      stringsAsFactors = FALSE
    ))
  }

  # Get patterns
  if (is.null(patterns)) {
    patterns <- get_stat_patterns()
    # Initialize pseudo-patterns if not already done
    if (length(patterns) <= 2) {
      update_stat_patterns_to_pseudo()
      patterns <- get_stat_patterns()
    }
  }

  # Try each pattern (greedy - first match wins)
  for (pattern_name in names(patterns)) {
    pattern <- patterns[[pattern_name]]

    if (grepl(pattern$regex, stat_value)) {
      # Extract matches
      matches <- regmatches(stat_value, regexec(pattern$regex, stat_value))[[1]]

      if (length(matches) > 1) {
        # matches[1] is full match, rest are capture groups
        values <- matches[-1]

        # Clean up values - remove trailing % from percentages
        for (i in seq_along(values)) {
          if (pattern$stats[i] == "pct" && grepl("%$", values[i])) {
            values[i] <- sub("%$", "", values[i])
          }
        }

        # Create result with one row per statistic
        result <- data.frame(
          stat = values,
          stat_name = pattern$stats,
          stat_label = pattern$labels,
          stringsAsFactors = FALSE
        )

        return(result)
      }
    }
  }

  # No pattern matched - return as "other"
  return(data.frame(
    stat = stat_value,
    stat_name = "other",
    stat_label = "Other",
    stringsAsFactors = FALSE
  ))
}

#' Add statistical pattern from pseudo-pattern
#'
#' Convenience function that converts a pseudo-pattern and adds it to the registry.
#'
#' @param name Pattern name
#' @param template Pseudo-pattern template (e.g., "n (pct%)")
#' @param labels Optional custom labels for the statistics
#' @param priority Priority for pattern matching (higher = checked first)
#'
#' @export
#'
#' @examples
#' # Add a new pattern for count (percentage)
#' add_pseudo_pattern("count_pct", "{n} ({pct}%)")
#'
#' # Add pattern with custom labels
#' add_pseudo_pattern("ci_range", "{hr} ({ci_lower}-{ci_upper})",
#'                   labels = c("Hazard Ratio", "95% CI Lower", "95% CI Upper"))
add_pseudo_pattern <- function(name, template, labels = NULL, priority = NULL) {
  # Convert pseudo-pattern to regex
  pattern_info <- pseudo_to_regex_simple(template)

  # Use provided labels or defaults
  if (!is.null(labels)) {
    if (length(labels) != length(pattern_info$stats)) {
      stop("Number of labels must match number of placeholders in template")
    }
    pattern_info$labels <- labels
  }

  # Add to pattern registry
  add_stat_pattern(
    name = name,
    template = template,
    regex = pattern_info$regex,
    stats = pattern_info$stats,
    labels = pattern_info$labels,
    priority = ifelse(is.null(priority), TRUE, priority == 1)
  )
}

#' Initialize BIGN pseudo-patterns
#' @keywords internal
.init_bign_pseudo_patterns <- function() {
  # Single maximal pattern that covers most BIGN cases
  # This replaces 8+ hardcoded regex patterns with one flexible template
  bign_patterns <- list(
    "maximal_n" = list(
      template = "N={n}",
      # Maximal regex: optional parens, optional N/n/BIGN indicators, optional =/:, number, optional trailing
      regex = "(?:\\()?\\s*[NnBbIiGg]*\\s*[=:]?\\s*(\\d+)(?:\\s*\\)|\\s*[;,.]?|$)",
      stats = c("n"),
      labels = c("n")
    )
  )
  
  bign_patterns
}

#' Get BIGN patterns using pseudo-pattern system
#' @export
get_bign_pseudo_patterns <- function() {
  if (is.null(.stateful_patterns$BIGN_PSEUDO_PATTERNS)) {
    .stateful_patterns$BIGN_PSEUDO_PATTERNS <- .init_bign_pseudo_patterns()
  }
  return(.stateful_patterns$BIGN_PSEUDO_PATTERNS)
}

#' Parse BIGN value using pseudo-pattern system
#' @param bign_text Character string containing BIGN information
#' @return List with extracted N value and label, or NULL if no match
#' @keywords internal
parse_bign_value_pseudo <- function(bign_text) {
  bign_text <- trimws(as.character(bign_text))

  # Skip if empty or NA
  if (is.na(bign_text) || bign_text == "") {
    return(NULL)
  }

  # Use simple regex to extract numbers after N/n indicators
  # Flexible pattern: optional parens, N/n, optional =/:, number
  n_match <- regexec("(?:\\()?\\s*[NnBbIiGg]*\\s*[=:]?\\s*(\\d+)", bign_text, perl = TRUE)

  if (n_match[[1]][1] != -1) {
    match_groups <- regmatches(bign_text, n_match)[[1]]

    if (length(match_groups) >= 2) {
      n_value <- as.numeric(match_groups[2])  # First capture group

      # Extract label by removing the matched BIGN part
      full_match <- match_groups[1]
      label_part <- stringr::str_replace(bign_text, stringr::fixed(full_match), "")
      label_part <- trimws(label_part)

      list(
        n_value = n_value,
        label = if (nchar(label_part) > 0) label_part else NA_character_,
        pattern_used = "flexible_n",
        full_match = full_match
      )
    }
  } else {
    NULL
  }
}

#' Add BIGN pseudo-pattern
#' @param name Pattern name  
#' @param template Template like "N=\\\\{n\\\\}" or "(N=\\\\{n\\\\})"
#' @export
#' @examples
#' # Add specific BIGN pattern
#' add_bign_pseudo_pattern("my_bign", "(BIGN=\\\\{n\\\\})")
add_bign_pseudo_pattern <- function(name, template) {
  # For BIGN patterns, use standard pseudo-pattern approach
  pattern_info <- pseudo_to_regex_simple(template)

  # Get current patterns
  current_patterns <- get_bign_pseudo_patterns()

  # Add new pattern
  current_patterns[[name]] <- list(
    template = template,
    regex = pattern_info$regex,
    stats = pattern_info$stats,
    labels = pattern_info$labels
  )

  # Update global patterns
  .stateful_patterns$BIGN_PSEUDO_PATTERNS <- current_patterns

  invisible(current_patterns)
}

#' Reorder patterns based on variable context for better pattern matching
#' @param patterns List of statistical patterns
#' @param variable_context Variable name/label for context
#' @return Reordered patterns list
#' @keywords internal
reorder_patterns_by_context <- function(patterns, variable_context) {
  if (is.null(variable_context) || is.na(variable_context) || variable_context == "") {
    return(patterns)
  }
  
  variable_lower <- tolower(variable_context)
  
  # Define context indicators for different stat types
  continuous_indicators <- c("mean", "median", "std", "sd", "se", "min", "max", "q1", "q3", 
                           "average", "variance", "deviation", "quartile", "range",
                           "cfb", "change from baseline", "baseline", "score")
  
  categorical_indicators <- c("n (%)", "count", "frequency", "number", "percent", 
                            "proportion", "rate", "incidence", "events", "subjects",
                            "patients", "responders", "response")
  
  ci_indicators <- c("confidence", "ci", "interval", "estimate", "hazard", "odds", 
                    "risk", "ratio", "difference", "effect")
  
  # Check which type this variable likely represents
  is_continuous <- any(sapply(continuous_indicators, function(x) grepl(x, variable_lower)))
  is_categorical <- any(sapply(categorical_indicators, function(x) grepl(x, variable_lower)))
  is_ci <- any(sapply(ci_indicators, function(x) grepl(x, variable_lower)))
  
  # Create ordered pattern preference
  priority_patterns <- character(0)
  
  if (is_ci) {
    priority_patterns <- c("ci_extra_spaces", "ci", "ci_dash", "hr_ci")
  } else if (is_continuous) {
    priority_patterns <- c("mean_sd", "mean_se", "median_range", "median_iqr", 
                          "median_q1_q3", "range", "range_comma")
  } else if (is_categorical) {
    priority_patterns <- c("n_pct", "count_only", "pct_only", "count_total")
  }
  
  # Reorder: priority patterns first, then the rest
  remaining_patterns <- names(patterns)[!names(patterns) %in% priority_patterns]
  new_order <- c(priority_patterns[priority_patterns %in% names(patterns)], remaining_patterns)
  
  return(patterns[new_order])
}

