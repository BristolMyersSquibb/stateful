#' stateful: State-Based RTF Table Parser for ARD Conversion
#'
#' A modern state-based parser for RTF tables that converts clinical trial data
#' into Analysis Results Data (ARD) format. Features global pattern registries,
#' position-aware parsing, and template-based statistical extraction.
#'
#' @section Main API Functions:
#' - [rtf_to_ard()]: Convert RTF file to ARD data frame
#' - [rtf_to_ard_json()]: Convert RTF file to ARD JSON format
#' - [rtf_to_ard_stage1()]: Extract raw structural data only
#' - [launch_stateful_app()]: Launch interactive Shiny application
#'
#' @section Export Functions:
#' - [export_ard_csv()]: Export ARD data to CSV format
#' - [export_ard_json()]: Export ARD data to JSON format
#' - [batch_convert_rtf()]: Batch convert multiple RTF files
#'
#' @section Pattern Management:
#' - [get_bign_patterns()], [set_bign_patterns()], [add_bign_pattern()]: Manage BIGN extraction patterns
#' - [get_stat_patterns()], [set_stat_patterns()], [add_stat_pattern()]: Manage statistical parsing patterns
#' - [add_pseudo_pattern()]: Add new statistical patterns using templates
#'
#' @keywords internal
"_PACKAGE"
#' @import dplyr stringr
#' @importFrom jsonlite write_json
#' @importFrom purrr map_dfr
#' @importFrom tidyr separate pivot_longer
#' @importFrom tools file_path_sans_ext
#' @importFrom stats median setNames
#' @importFrom utils head
NULL

#' Global Pattern Registry Environment
#' @keywords internal
.stateful_patterns <- new.env(parent = emptyenv())

#' Initialize patterns when package loads
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Initialize with minimal BIGN patterns (will be replaced by pseudo patterns)
  .stateful_patterns$BIGN_PATTERNS <- NULL  # Will use pseudo-pattern system

  # Initialize with basic patterns
  .stateful_patterns$STAT_PATTERNS <- list(
    "n_pct" = list(
      template = "{n} ({pct}%)",
      regex = "^\\s*([^\\s()]+)\\s*\\(([^\\s()]+)%?\\)\\s*$",
      stats = c("n", "pct"),
      labels = c("n", "pct")
    ),
    "count" = list(
      template = "{n}",
      regex = "^\\s*([^\\s()]+)\\s*$",
      stats = c("n"),
      labels = c("n")
    )
  )
  
  # Initialize pseudo-patterns after loading
  # This will be called after pseudo_pattern.R is loaded
  setHook(packageEvent("stateful", "onLoad"), function(...) {
    if (exists("update_stat_patterns_to_pseudo", mode = "function")) {
      update_stat_patterns_to_pseudo()
    }
  })
}

#' Launch the Stateful Shiny Application
#'
#' Launches an interactive web application for RTF table parsing and ARD conversion.
#' Allows users to upload RTF files, customize patterns, and download JSON results.
#'
#' @param port Port number for the Shiny app (default: random available port)
#' @param host Host IP address (default: "127.0.0.1")
#' @param launch.browser Whether to launch browser automatically (default: TRUE)
#'
#' @export
#' @examples
#' \dontrun{
#' # Launch the Shiny app
#' launch_stateful_app()
#'
#' # Launch on specific port
#' launch_stateful_app(port = 3838)
#' }
launch_stateful_app <- function(port = NULL, host = "127.0.0.1", launch.browser = TRUE) {
  app_dir <- system.file("shiny-app", package = "stateful")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Please reinstall the package.")
  }

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is required to run the Stateful app. Please install it with: install.packages('shiny')")
  }

  shiny::runApp(
    appDir = app_dir,
    port = port,
    host = host,
    launch.browser = launch.browser
  )
}