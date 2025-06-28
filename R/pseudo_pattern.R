#' Simplified Pseudo-Pattern System V3 - Using Simple Regex
#'
#' Converts user-friendly pseudo-patterns like "{n} ({pct})" into regex patterns
#' using simple non-whitespace capture groups.
#'
#' @name pseudo-pattern-v3
NULL

#' Convert pseudo-pattern to regex pattern (ultra-simplified)
#'
#' @param template Character string with placeholders, e.g., "{n} ({pct}%)"
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
  # Use simple pattern: one or more non-whitespace characters
  for (i in seq_along(placeholder_matches)) {
    # Create named capture group
    group_name <- stat_names[i]
    regex_pattern <- gsub(
      paste0("<<<PLACEHOLDER", i, ">>>"),
      paste0("(?<", group_name, ">[^\\s]+)"),
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
  # Use [^\\s()]+ to capture non-whitespace, non-parenthesis characters
  for (i in seq_along(placeholder_matches)) {
    regex_pattern <- sub(
      fixed = TRUE,
      pattern = paste0("<<<MARKER", i, ">>>"),
      replacement = "([^\\s()]+)",
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
    "n_pct" = "{n} ({pct}%)",
    "n_pct_tight" = "{n}({pct}%)",
    "n_pct_nopercent" = "{n} ({pct})",
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

#' Parse statistic value using pseudo-pattern system
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
#' @param template Pseudo-pattern template (e.g., "{n} ({pct}%)")
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