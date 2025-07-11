#' Pattern Management Functions
#'
#' Functions for managing and configuring patterns used in RTF parsing.
#' These provide backward compatibility with the existing pattern system.
#'
#' @name pattern-management
NULL

# Global pattern storage
.pattern_store <- new.env(parent = emptyenv())

#' Initialize pattern store with defaults
#'
#' @keywords internal
initialize_pattern_store <- function() {
  .pattern_store$bign_patterns <- list(
    "N = {n}" = "n",
    "n = {n}" = "n",
    "{n} subjects" = "n"
  )
  
  .pattern_store$stat_patterns <- list(
    "{n} ({pct}%)" = c("n", "pct"),
    "{n} ({pct})" = c("n", "pct"),
    "{mean} ({sd})" = c("mean", "sd")
  )
  
  .pattern_store$title_patterns <- c(
    "Table \\d+",
    "Figure \\d+"
  )
  
  .pattern_store$population_patterns <- c(
    "All Treated Subjects",
    "Safety Population",
    "Intent-to-Treat Population"
  )
  
  .pattern_store$footnote_patterns <- c(
    "^\\(\\d+\\)",
    "^Note:"
  )
  
  .pattern_store$table_header_patterns <- c(
    "Treatment Group",
    "Arm [A-Z]:"
  )
}

# Initialize on load
initialize_pattern_store()

#' Get BIGN patterns
#'
#' @return Character vector of BIGN patterns
#' @export
get_bign_patterns <- function() {
  patterns <- .pattern_store$bign_patterns %||% list()
  names(patterns)
}

#' Set BIGN patterns
#'
#' @param patterns List of BIGN patterns
#' @export
set_bign_patterns <- function(patterns) {
  .pattern_store$bign_patterns <- patterns
}

#' Add BIGN pattern
#'
#' @param pattern Pattern string
#' @param stat_name Statistic name (defaults to "n")
#' @export
add_bign_pattern <- function(pattern, stat_name = "n") {
  current <- .pattern_store$bign_patterns %||% list()
  current[[pattern]] <- stat_name
  .pattern_store$bign_patterns <- current
}

#' Get statistic patterns
#'
#' @return List of statistic patterns
#' @export
get_stat_patterns <- function() {
  .pattern_store$stat_patterns %||% list()
}

#' Set statistic patterns
#'
#' @param patterns List of statistic patterns
#' @export
set_stat_patterns <- function(patterns) {
  .pattern_store$stat_patterns <- patterns
}

#' Add statistic pattern
#'
#' @param pattern Pattern string
#' @param stat_names Vector of statistic names
#' @export
add_stat_pattern <- function(pattern, stat_names) {
  current <- get_stat_patterns()
  current[[pattern]] <- stat_names
  set_stat_patterns(current)
}

#' Get title patterns
#'
#' @return Vector of title patterns
#' @export
get_title_patterns <- function() {
  .pattern_store$title_patterns %||% character(0)
}

#' Add title pattern
#'
#' @param pattern Pattern string
#' @export
add_title_pattern <- function(pattern) {
  current <- get_title_patterns()
  .pattern_store$title_patterns <- c(current, pattern)
}

#' Get population patterns
#'
#' @return Vector of population patterns
#' @export
get_population_patterns <- function() {
  .pattern_store$population_patterns %||% character(0)
}

#' Add population pattern
#'
#' @param pattern Pattern string
#' @export
add_population_pattern <- function(pattern) {
  current <- get_population_patterns()
  .pattern_store$population_patterns <- c(current, pattern)
}

#' Get footnote patterns
#'
#' @return Vector of footnote patterns
#' @export
get_footnote_patterns <- function() {
  .pattern_store$footnote_patterns %||% character(0)
}

#' Add footnote pattern
#'
#' @param pattern Pattern string
#' @export
add_footnote_pattern <- function(pattern) {
  current <- get_footnote_patterns()
  .pattern_store$footnote_patterns <- c(current, pattern)
}

#' Get table header patterns
#'
#' @return Vector of table header patterns
#' @export
get_table_header_patterns <- function() {
  .pattern_store$table_header_patterns %||% character(0)
}

#' Add table header pattern
#'
#' @param pattern Pattern string
#' @export
add_table_header_pattern <- function(pattern) {
  current <- get_table_header_patterns()
  .pattern_store$table_header_patterns <- c(current, pattern)
}

#' Configure table patterns
#'
#' @param title_patterns Vector of title patterns
#' @param population_patterns Vector of population patterns
#' @param stat_patterns List of statistic patterns
#' @param bign_patterns List of BIGN patterns
#' @param footnote_patterns Vector of footnote patterns
#' @param table_header_patterns Vector of table header patterns
#' @export
configure_table_patterns <- function(title_patterns = NULL,
                                   population_patterns = NULL,
                                   stat_patterns = NULL,
                                   bign_patterns = NULL,
                                   footnote_patterns = NULL,
                                   table_header_patterns = NULL) {
  if (!is.null(title_patterns)) {
    .pattern_store$title_patterns <- title_patterns
  }
  
  if (!is.null(population_patterns)) {
    .pattern_store$population_patterns <- population_patterns
  }
  
  if (!is.null(stat_patterns)) {
    .pattern_store$stat_patterns <- stat_patterns
  }
  
  if (!is.null(bign_patterns)) {
    .pattern_store$bign_patterns <- bign_patterns
  }
  
  if (!is.null(footnote_patterns)) {
    .pattern_store$footnote_patterns <- footnote_patterns
  }
  
  if (!is.null(table_header_patterns)) {
    .pattern_store$table_header_patterns <- table_header_patterns
  }
}

#' Show pattern configuration
#'
#' @export
show_pattern_config <- function() {
  list(
    bign_patterns = get_bign_patterns(),
    stat_patterns = get_stat_patterns(),
    title_patterns = get_title_patterns(),
    population_patterns = get_population_patterns(),
    footnote_patterns = get_footnote_patterns(),
    table_header_patterns = get_table_header_patterns()
  )
}

#' Get percentage patterns (alias for stat patterns)
#'
#' @return List of percentage patterns
#' @export
get_percentage_patterns <- function() {
  patterns <- get_stat_patterns()
  # Filter to only patterns with "pct" statistic
  patterns[sapply(patterns, function(x) "pct" %in% x)]
}

#' Get variable names (placeholder)
#'
#' @return Character vector of common variable names
#' @export
get_variable_names <- function() {
  c("AGE", "SEX", "RACE", "AEBODSYS", "AEDECOD", "AESEV")
}

#' Get statistic names (placeholder)
#'
#' @return Character vector of common statistic names
#' @export
get_stat_names <- function() {
  c("n", "pct", "mean", "sd", "median", "min", "max", "q1", "q3")
}

#' Add pseudo pattern (wrapper for add_stat_pattern)
#'
#' @param name Pattern name
#' @param template Pattern template
#' @export
add_pseudo_pattern <- function(name, template) {
  # Extract statistic names from template
  stats <- regmatches(template, gregexpr("\\{([^}]+)\\}", template))[[1]]
  stats <- gsub("\\{|\\}", "", stats)
  
  add_stat_pattern(template, stats)
}

#' Get BIGN pseudo patterns
#'
#' @return List of BIGN pseudo patterns
#' @export
get_bign_pseudo_patterns <- function() {
  get_bign_patterns()
}

#' Add BIGN pseudo pattern
#'
#' @param pattern Pattern string
#' @param stat_name Statistic name
#' @export
add_bign_pseudo_pattern <- function(pattern, stat_name) {
  add_bign_pattern(pattern, stat_name)
}

#' Update stat patterns to pseudo format
#'
#' @export
update_stat_patterns_to_pseudo <- function() {
  # This is a no-op for backward compatibility
  invisible(NULL)
}