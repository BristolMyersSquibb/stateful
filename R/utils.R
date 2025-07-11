#' Global variables for R CMD check
#' @name stateful-globals
#' @keywords internal
utils::globalVariables(c(
  "variable",
  "stat",
  "column_name",
  "level_1",
  "level_2",
  "level_3",
  "column_index",
  "stat_unparsed",
  "stat_name",
  "stat_label",
  "group1_level",
  "n_value",
  "clean_group_level",
  "indent_level",
  "variable_clean",
  "variable_label1",
  "group1",
  "calc_percent",
  "total",
  "group_label",
  "."
))

#' Clean variable text from RTF formatting
#'
#' Remove RTF formatting artifacts from variable text
#'
#' @param text Character string with potential RTF formatting
#' @return Clean text string
#' @export
clean_variable_text <- function(text) {
  if (is.null(text) || length(text) == 0) {
    return("")
  }
  
  # Convert to character if not already
  if (!is.character(text)) {
    text <- as.character(text)
  }
  
  if (nchar(text) == 0) {
    return("")
  }
  
  # Remove RTF line markers
  text <- gsub("\\{\\\\line\\}", " ", text)
  
  # Remove extra braces and formatting
  text <- gsub("\\{|\\}", "", text)
  
  # Clean multiple spaces
  text <- gsub("\\s+", " ", text)
  
  # Trim whitespace
  trimws(text)
}

#' Extract variable hierarchy (legacy function)
#'
#' Legacy function for backward compatibility
#'
#' @param rows Table rows data
#' @return Processed hierarchy information
#' @export
extract_variable_hierarchy <- function(rows) {
  if (is.null(rows) || length(rows) == 0) {
    return(list())
  }
  
  # Basic hierarchy extraction - delegate to new system
  # This is a placeholder for backward compatibility
  list(
    hierarchical_variables = character(0),
    hierarchy_levels = integer(0)
  )
}

#' Process table rows (legacy function)
#'
#' Legacy function for backward compatibility
#'
#' @param rows Table rows data
#' @param header_groups Header group information
#' @return Processed table data
#' @export
process_table_rows <- function(rows, header_groups) {
  if (is.null(rows) || length(rows) == 0) {
    return(data.frame())
  }
  
  # Basic processing - delegate to new system
  # This is a placeholder for backward compatibility
  data.frame(
    variable = character(0),
    stat = character(0),
    stringsAsFactors = FALSE
  )
}