#' Convert an RTF Table into an ARD data frame
#'
#' This function converts a table in RTF format into a data frame in R,
#' following the ARD standard. Uses direct RTF parsing (no HTML conversion).
#'
#' @param file A string, the path to the input .rtf file.
#' @param config A list of configuration parameters. If NULL, uses default config.
#'
#' @return an R data frame following the ARD standard.
#'
#' @export
rtf_to_ard <- function(file, config = list()) {
  
  # STAGE 1: RTF → DataFrame (direct parsing)
  table_structure <- parse_rtf_to_dataframe(file)
  
  # STAGE 2: DataFrame → ARD (unparsed)  
  ard_unparsed <- dataframe_to_ard_unparsed(table_structure, config)
  
  # STAGE 3: ARD (unparsed) → ARD (parsed)
  ard_parsed <- parse_ard_statistics(ard_unparsed)
  
  return(ard_parsed)
}
