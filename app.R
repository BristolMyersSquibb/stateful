# Stateful Shiny Application - Updated with State Parser
library(shiny)
library(shinydashboard)
library(DT)
library(jsonlite)
library(plotly)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(tools)

# Load stateful package - required for all functions
pkgload::load_all()

# Load .Renviron if it exists
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# Initialize patterns on app start
if (exists("update_stat_patterns_to_pseudo")) {
  update_stat_patterns_to_pseudo()
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Stateful RTF Parser"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("State Parser", tabName = "state_parser", icon = icon("table")),
      menuItem("Pattern Parser", tabName = "pattern_parser", icon = icon("search")),
      menuItem("Data Viewer", tabName = "viewer", icon = icon("eye")),
      menuItem("LLM Query", tabName = "llm", icon = icon("robot")),
      menuItem("Export", tabName = "export", icon = icon("download")),
      menuItem("Help & Support", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    tabItems(
      # State Parser Tab (Stage 1)
      tabItem(tabName = "state_parser",
        fluidRow(
          box(title = "State Parser (Stage 1)", status = "primary", solidHeader = TRUE, width = 12,
            p("Structural RTF parsing using state-based table extraction. Raw statistics preserved for manual review."),
            fileInput("state_parser_files", "Choose RTF files:", 
                     accept = c(".rtf"), multiple = TRUE),
            br(),
            actionButton("state_parse_btn", "Parse Files", class = "btn-primary"),
            br(), br(),
            textOutput("state_parse_status")
          )
        ),
        
        fluidRow(
          box(title = "Parsing Results", status = "info", solidHeader = TRUE, width = 6,
            tableOutput("state_files_table")
          ),
          
          box(title = "Export Options", status = "success", solidHeader = TRUE, width = 6,
            br(),
            downloadButton("download_state_csv", "Download CSV", class = "btn-success"),
            br(), br(),
            downloadButton("download_state_json", "Download JSON", class = "btn-info")
          )
        ),
        
        fluidRow(
          box(title = "State Parser Data", status = "primary", solidHeader = TRUE, width = 12,
            p("Complete data table for manual review:"),
            DTOutput("state_parser_table")
          )
        )
      ),
      
      # Pattern Parser Tab (Stage 2)
      tabItem(tabName = "pattern_parser",
        fluidRow(
          box(title = "Stage 2: Pattern Enhancement", status = "warning", solidHeader = TRUE, width = 12,
            div(class = "alert alert-warning",
              icon("cogs"),
              HTML("<strong>Optional Enhancement:</strong> Load Stage 1 data and apply pattern matching to split combined statistics.")
            ),
            fileInput("load_structured_file", "Load Structured ARD (JSON from Stage 1):", 
                     accept = c(".json")),
            br(),
            actionButton("pattern_parse_btn", "Apply Pattern Parsing", class = "btn-warning"),
            br(), br(),
            textOutput("pattern_parse_status")
          )
        ),
        
        fluidRow(
          box(title = "Pattern-Enhanced Data", status = "warning", solidHeader = TRUE, width = 12,
            DTOutput("pattern_parser_table")
          )
        )
      ),
      
      # Original Data Viewer Tab
      tabItem(tabName = "viewer",
        fluidRow(
          box(title = "RTF File Upload", status = "primary", solidHeader = TRUE, width = 6,
            fileInput("file", "Choose RTF files:",
                     accept = c(".rtf"), multiple = TRUE),
            checkboxInput("show_unparsed", "Show unparsed data", FALSE),
            checkboxInput("show_metadata", "Show metadata", TRUE),
            br(),
            textInput("group2_name", "Group 2 Name (optional):", 
                     placeholder = "e.g., TIMEPOINT"),
            br(),
            actionButton("parse_btn", "Parse Files", class = "btn-primary")
          ),
          
          box(title = "Sample Data", status = "info", solidHeader = TRUE, width = 6,
            p("Try parsing with these sample files:"),
            actionButton("load_sample", "Load Sample Files", class = "btn-info"),
            br(), br(),
            textOutput("sample_status")
          )
        ),
        
        fluidRow(
          box(title = "Parsing Results", status = "success", solidHeader = TRUE, width = 12,
            tableOutput("files_table")
          )
        ),
        
        fluidRow(
          box(title = "ARD Data", status = "primary", solidHeader = TRUE, width = 12,
            DTOutput("ard_table")
          )
        )
      ),
      
      # LLM Query Tab
      tabItem(tabName = "llm",
        fluidRow(
          box(title = "Natural Language Queries", status = "warning", solidHeader = TRUE, width = 12,
            div(class = "alert alert-info",
              icon("info-circle"),
              HTML("<strong>Note:</strong> LLM functionality requires Azure OpenAI configuration.")
            ),
            fileInput("llm_file", "Load ARD JSON file:", accept = c(".json")),
            br(),
            textAreaInput("query_text", "Enter your question:", 
                         placeholder = "e.g., What is the mean age by treatment group?",
                         height = "100px"),
            br(),
            actionButton("query_btn", "Query Data", class = "btn-warning"),
            br(), br(),
            textOutput("query_status")
          )
        ),
        
        fluidRow(
          box(title = "Query Results", status = "warning", solidHeader = TRUE, width = 12,
            verbatimTextOutput("query_results")
          )
        )
      ),
      
      # Export Tab
      tabItem(tabName = "export",
        fluidRow(
          box(title = "Export Options", status = "success", solidHeader = TRUE, width = 12,
            p("Export parsed data in various formats:"),
            br(),
            downloadButton("download_csv", "Download CSV", class = "btn-success"),
            br(), br(),
            downloadButton("download_json", "Download JSON", class = "btn-info"),
            br(), br(),
            downloadButton("download_excel", "Download Excel", class = "btn-warning")
          )
        )
      ),
      
      # Help Tab
      tabItem(tabName = "help",
        fluidRow(
          box(title = "Help & Support", status = "info", solidHeader = TRUE, width = 12,
            h3("State Parser (Stage 1)"),
            p("Upload RTF files for structural parsing. This stage extracts raw table data without pattern interpretation."),
            
            h3("Pattern Parser (Stage 2)"),
            p("Apply pattern matching to Stage 1 data to split combined statistics like '137 (39.0%)' into separate rows."),
            
            h3("Data Viewer"),
            p("Traditional RTF parsing with pattern interpretation applied automatically."),
            
            h3("LLM Query"),
            p("Natural language queries on parsed data (requires Azure OpenAI configuration)."),
            
            h3("Export"),
            p("Download parsed data in CSV, JSON, or Excel formats."),
            
            br(),
            h3("Support"),
            p("For technical support, contact the stateful package maintainer.")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values for state parser
  state_parsed_data <- reactiveVal(NULL)
  state_full_results <- reactiveVal(NULL)  # Store full results with metadata
  pattern_parsed_data <- reactiveVal(NULL)
  
  # State Parser Logic
  observeEvent(input$state_parse_btn, {
    req(input$state_parser_files)
    
    output$state_parse_status <- renderText("Processing files...")
    
    tryCatch({
      all_data <- data.frame()
      all_metadata <- list()
      results <- data.frame(File = character(), Status = character(), Rows = integer())
      
      for (i in 1:nrow(input$state_parser_files)) {
        file_path <- input$state_parser_files$datapath[i]
        file_name <- input$state_parser_files$name[i]
        
        # Use state parser stage 1
        stage1_result <- state_parser_stage1(file_path)
        
        if (!is.null(stage1_result) && nrow(stage1_result$data) > 0) {
          stage1_result$data$source_file <- file_name
          all_data <- rbind(all_data, stage1_result$data)
          all_metadata[[file_name]] <- stage1_result$metadata
          results <- rbind(results, data.frame(
            File = file_name,
            Status = "Success", 
            Rows = nrow(stage1_result$data)
          ))
        } else {
          results <- rbind(results, data.frame(
            File = file_name,
            Status = "Failed",
            Rows = 0
          ))
        }
      }
      
      # Store both data and full results
      state_parsed_data(all_data)
      state_full_results(list(
        data = all_data,
        metadata = all_metadata,
        stage = "state_parser_stage1"
      ))
      
      output$state_files_table <- renderTable({
        results
      })
      
      output$state_parser_table <- renderDT({
        datatable(all_data, 
                 options = list(pageLength = 25, scrollX = TRUE),
                 filter = "top")
      })
      
      output$state_parse_status <- renderText(
        paste("Processing complete:", nrow(all_data), "rows extracted from", 
              sum(results$Status == "Success"), "files")
      )
      
    }, error = function(e) {
      output$state_parse_status <- renderText(paste("Error:", e$message))
    })
  })
  
  # Pattern Parser Logic
  observeEvent(input$pattern_parse_btn, {
    req(input$load_structured_file)
    
    output$pattern_parse_status <- renderText("Applying pattern parsing...")
    
    tryCatch({
      # Load JSON data
      json_path <- input$load_structured_file$datapath
      structured_data <- load_structured_ard_json(json_path)
      
      # Apply pattern parsing (Stage 2)
      parsed_data <- parse_ard_statistics(structured_data)
      
      pattern_parsed_data(parsed_data)
      
      output$pattern_parser_table <- renderDT({
        datatable(parsed_data,
                 options = list(pageLength = 25, scrollX = TRUE),
                 filter = "top")
      })
      
      output$pattern_parse_status <- renderText(
        paste("Pattern parsing complete:", nrow(parsed_data), "rows")
      )
      
    }, error = function(e) {
      output$pattern_parse_status <- renderText(paste("Error:", e$message))
    })
  })
  
  # Download handlers for state parser
  output$download_state_csv <- downloadHandler(
    filename = function() {
      paste0("state_parser_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- state_parsed_data()
      if (!is.null(data)) {
        write.csv(data, file, row.names = FALSE)
      }
    }
  )
  
  output$download_state_json <- downloadHandler(
    filename = function() {
      paste0("state_parser_data_", Sys.Date(), ".json")
    },
    content = function(file) {
      full_results <- state_full_results()
      if (!is.null(full_results) && !is.null(full_results$data)) {
        # Create complete JSON structure with metadata
        json_output <- list(
          stage = full_results$stage,
          data = full_results$data,
          metadata = full_results$metadata,
          export_info = list(
            export_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            row_count = nrow(full_results$data),
            column_count = ncol(full_results$data),
            files_processed = length(full_results$metadata)
          )
        )
        write_json(json_output, file, pretty = TRUE)
      }
    }
  )
  
  # Reactive values for original functionality
  parsed_data <- reactiveVal(NULL)
  llm_data <- reactiveVal(NULL)
  
  # Original parsing logic
  observeEvent(input$parse_btn, {
    req(input$file)
    
    tryCatch({
      all_data <- data.frame()
      results <- data.frame(File = character(), Status = character(), Rows = integer())
      
      config <- list()
      if (nchar(input$group2_name) > 0) {
        config$group2_name <- input$group2_name
      }
      
      for (i in 1:nrow(input$file)) {
        file_path <- input$file$datapath[i]
        file_name <- input$file$name[i]
        
        if (input$show_unparsed) {
          # Parse to unparsed ARD
          df <- parse_rtf_to_dataframe(file_path)
          ard_data <- dataframe_to_ard_unparsed(df, config)
        } else {
          # Full pipeline
          ard_data <- rtf_to_ard(file_path, config)
        }
        
        if (!is.null(ard_data) && nrow(ard_data) > 0) {
          ard_data$source_file <- file_name
          all_data <- rbind(all_data, ard_data)
          results <- rbind(results, data.frame(
            File = file_name,
            Status = "Success", 
            Rows = nrow(ard_data)
          ))
        } else {
          results <- rbind(results, data.frame(
            File = file_name,
            Status = "Failed",
            Rows = 0
          ))
        }
      }
      
      parsed_data(all_data)
      
      output$files_table <- renderTable({
        results
      })
      
      output$ard_table <- renderDT({
        display_data <- all_data
        if (!input$show_metadata) {
          display_data <- display_data[!grepl("^(TITLE|POPULATION|FOOTNOTE|BIGN)", display_data$variable), ]
        }
        
        datatable(display_data, 
                 options = list(pageLength = 25, scrollX = TRUE),
                 filter = "top")
      })
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Sample data logic
  observeEvent(input$load_sample, {
    sample_dir <- system.file("extdata", package = "stateful")
    if (sample_dir != "") {
      sample_files <- list.files(sample_dir, pattern = "\\.rtf$", full.names = TRUE)
      if (length(sample_files) > 0) {
        output$sample_status <- renderText(paste("Found", length(sample_files), "sample RTF files"))
      } else {
        output$sample_status <- renderText("No sample RTF files found")
      }
    } else {
      output$sample_status <- renderText("Sample data not available")
    }
  })
  
  # LLM Query logic
  observeEvent(input$llm_file, {
    req(input$llm_file)
    tryCatch({
      json_path <- input$llm_file$datapath
      ard_data <- load_ard_json(json_path)
      llm_data(ard_data)
      output$query_status <- renderText("ARD data loaded successfully")
    }, error = function(e) {
      output$query_status <- renderText(paste("Error loading file:", e$message))
    })
  })
  
  observeEvent(input$query_btn, {
    req(input$query_text, llm_data())
    
    output$query_status <- renderText("Processing query...")
    
    tryCatch({
      if (exists("query_ard_with_llm")) {
        result <- query_ard_with_llm(llm_data(), input$query_text)
        output$query_results <- renderText(result)
        output$query_status <- renderText("Query completed")
      } else {
        output$query_results <- renderText("LLM functionality not available - check Azure OpenAI configuration")
        output$query_status <- renderText("LLM query failed")
      }
    }, error = function(e) {
      output$query_results <- renderText(paste("Error:", e$message))
      output$query_status <- renderText("Query failed")
    })
  })
  
  # Download handlers
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("stateful_ard_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- parsed_data()
      if (!is.null(data)) {
        write.csv(data, file, row.names = FALSE)
      }
    }
  )
  
  output$download_json <- downloadHandler(
    filename = function() {
      paste0("stateful_ard_", Sys.Date(), ".json")
    },
    content = function(file) {
      data <- parsed_data()
      if (!is.null(data)) {
        write_json(data, file, pretty = TRUE)
      }
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("stateful_ard_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      data <- parsed_data()
      if (!is.null(data)) {
        # Simple Excel export using write.csv as fallback
        write.csv(data, file, row.names = FALSE)
      }
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)