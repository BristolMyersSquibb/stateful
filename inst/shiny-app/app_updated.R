# Stateful Shiny Application - Updated with Pseudo-Pattern Support
library(shiny)
library(shinydashboard)
library(DT)
library(jsonlite)
library(plotly)
library(stateful)

# Initialize patterns on app start
if (exists("update_stat_patterns_to_pseudo")) {
  update_stat_patterns_to_pseudo()
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Stateful RTF Parser"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload & Parse", tabName = "upload", icon = icon("upload")),
      menuItem("Pattern Management", tabName = "patterns", icon = icon("cogs")),
      menuItem("Data Viewer", tabName = "viewer", icon = icon("table")),
      menuItem("LLM Query", tabName = "llm", icon = icon("robot")),
      menuItem("Export", tabName = "export", icon = icon("download"))
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
      # Upload & Parse Tab
      tabItem(tabName = "upload",
        fluidRow(
          box(title = "Upload RTF Files", status = "primary", solidHeader = TRUE, width = 12,
            fileInput("rtf_files", "Choose RTF files:", 
                     accept = c(".rtf"), multiple = TRUE),
            br(),
            actionButton("parse_btn", "Parse Files", class = "btn-success btn-lg"),
            br(), br(),
            textOutput("parse_status")
          )
        ),
        
        fluidRow(
          box(title = "Parsed Files", status = "info", solidHeader = TRUE, width = 6,
            tableOutput("parsed_files_table")
          ),
          
          box(title = "Summary Statistics", status = "success", solidHeader = TRUE, width = 6,
            tableOutput("summary_stats")
          )
        ),
        
        fluidRow(
          box(title = "Sample Data Preview", status = "warning", solidHeader = TRUE, width = 12,
            h4("First 100 rows of ARD data:"),
            DTOutput("sample_data_table")
          )
        )
      ),
      
      # Pattern Management Tab - UPDATED
      tabItem(tabName = "patterns",
        fluidRow(
          box(title = "Current Patterns", status = "info", solidHeader = TRUE, width = 6,
            h4("BIGN Patterns:"),
            verbatimTextOutput("current_bign_patterns"),
            br(),
            h4("Statistical Patterns:"),
            verbatimTextOutput("current_stat_patterns")
          ),
          
          box(title = "Pattern Management", status = "primary", solidHeader = TRUE, width = 6,
            h4("Add New BIGN Pattern:"),
            textInput("new_bign_pattern", "Pattern:", placeholder = "e.g., (N = 123)"),
            numericInput("bign_priority", "Priority (1 = highest):", value = 1, min = 1),
            actionButton("add_bign_btn", "Add BIGN Pattern", class = "btn-primary"),
            br(), br(),
            
            h4("Add New Statistical Pattern:"),
            textInput("stat_pattern_name", "Pattern Name:", placeholder = "e.g., hr_ci"),
            textInput("stat_pattern_template", "Template:", 
                     placeholder = "e.g., {hr} ({ci_lower}-{ci_upper})"),
            p("Use {name} for placeholders. Examples:", style = "color: #666;"),
            tags$ul(
              tags$li("{n} ({pct}%) for count and percentage"),
              tags$li("{mean} ({sd}) for mean and standard deviation"),
              tags$li("{min} - {max} for ranges"),
              style = "font-size: 0.9em; color: #666;"
            ),
            textInput("stat_pattern_labels", "Labels (optional, comma-separated):", 
                     placeholder = "e.g., Hazard Ratio,CI Lower,CI Upper"),
            actionButton("add_stat_btn", "Add Pattern", class = "btn-primary"),
            br(), br(),
            
            h4("Common Templates:"),
            selectInput("common_templates", "Select a template:",
                       choices = list(
                         "Count (Percentage)" = "{n} ({pct}%)",
                         "Count (Percentage) - No space" = "{n}({pct}%)",
                         "Mean (SD)" = "{mean} ({sd})",
                         "Median (Range)" = "{median} ({min}, {max})",
                         "Median (IQR)" = "{median} ({q1}, {q3})",
                         "Range" = "{min} - {max}",
                         "Confidence Interval" = "{estimate} ({ci_lower}, {ci_upper})",
                         "Hazard Ratio (CI)" = "{hr} ({ci_lower}-{ci_upper})",
                         "Count/Total" = "{n}/{total}",
                         "p-value" = "p = {p}"
                       )),
            actionButton("use_template_btn", "Use This Template", class = "btn-info")
          )
        ),
        
        fluidRow(
          box(title = "Pattern Testing", status = "success", solidHeader = TRUE, width = 12,
            h4("Test Statistical Patterns:"),
            textInput("test_value", "Test Value:", placeholder = "e.g., 137 (39.0%)"),
            actionButton("test_pattern_btn", "Test Pattern", class = "btn-success"),
            br(), br(),
            h4("Test Results:"),
            DTOutput("pattern_test_results"),
            br(),
            h4("Pattern Debug Info:"),
            verbatimTextOutput("pattern_debug")
          )
        )
      ),
      
      # Data Viewer Tab
      tabItem(tabName = "viewer",
        fluidRow(
          box(title = "File Selection", status = "primary", solidHeader = TRUE, width = 12,
            selectInput("selected_file", "Select File to View:", choices = NULL),
            br(),
            h4("File Summary:"),
            tableOutput("file_summary")
          )
        ),
        
        fluidRow(
          box(title = "ARD Data", status = "info", solidHeader = TRUE, width = 12,
            DTOutput("ard_data_table"),
            br(),
            downloadButton("download_json", "Download JSON", class = "btn-success")
          )
        ),
        
        fluidRow(
          box(title = "Data Visualization", status = "success", solidHeader = TRUE, width = 12,
            plotlyOutput("data_plot")
          )
        )
      ),
      
      # LLM Query Tab
      tabItem(tabName = "llm",
        fluidRow(
          box(title = "Multi-Table Query Interface", status = "primary", solidHeader = TRUE, width = 12,
            h4("Query Examples:"),
            tags$ul(
              tags$li("What are the total number of patients in table A vs table B?"),
              tags$li("Compare the mean age between treatment groups"),
              tags$li("What is the most common adverse event across all tables?"),
              tags$li("Calculate the percentage of patients with Grade 3+ events")
            ),
            br(),
            textAreaInput("llm_query", "Enter your query:", 
                         placeholder = "Ask questions about your parsed RTF data...",
                         rows = 3),
            actionButton("submit_query_btn", "Submit Query", class = "btn-primary"),
            br(), br(),
            h4("Available Tables:"),
            tableOutput("available_tables"),
            br(),
            h4("Query Response:"),
            verbatimTextOutput("llm_response"),
            br(),
            h4("Generated R Code:"),
            verbatimTextOutput("generated_code")
          )
        )
      ),
      
      # Export Tab
      tabItem(tabName = "export",
        fluidRow(
          box(title = "Export Options", status = "primary", solidHeader = TRUE, width = 6,
            h4("Individual Files:"),
            tableOutput("export_files_list"),
            br(),
            h4("Bulk Export:"),
            checkboxInput("include_metadata", "Include metadata files", value = TRUE),
            checkboxInput("include_summary", "Include summary report", value = TRUE),
            br(),
            downloadButton("download_zip", "Download All as ZIP", class = "btn-success")
          ),
          
          box(title = "Export Summary", status = "info", solidHeader = TRUE, width = 6,
            h4("Export Statistics:"),
            tableOutput("export_summary"),
            br(),
            h4("File Formats:"),
            tags$ul(
              tags$li("JSON: ARD-compliant data format"),
              tags$li("CSV: Tabular data export"),
              tags$li("Excel: Multi-sheet workbook"),
              tags$li("Summary: Processing report")
            )
          )
        )
      )
    )
  )
)

# Server - UPDATED
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    parsed_data = list(),
    file_info = data.frame(),
    pattern_test_result = NULL
  )
  
  # Upload and Parse Tab Logic
  observeEvent(input$parse_btn, {
    req(input$rtf_files)
    
    output$parse_status <- renderText({
      "Processing RTF files..."
    })
    
    # Process each uploaded file
    results <- list()
    file_info <- data.frame()
    
    for (i in 1:nrow(input$rtf_files)) {
      file_path <- input$rtf_files$datapath[i]
      file_name <- input$rtf_files$name[i]
      
      tryCatch({
        # Parse RTF file
        parsed_result <- rtf_to_ard_json(file_path, output_file = NULL)
        
        results[[file_name]] <- parsed_result
        
        # Collect file info
        file_info <- rbind(file_info, data.frame(
          File = file_name,
          Status = "Success",
          Rows = nrow(parsed_result$ard_data),
          Groups = length(unique(parsed_result$ard_data$group1_level)),
          StatTypes = length(unique(parsed_result$ard_data$stat_name)),
          stringsAsFactors = FALSE
        ))
        
      }, error = function(e) {
        file_info <<- rbind(file_info, data.frame(
          File = file_name,
          Status = paste("Error:", e$message),
          Rows = NA,
          Groups = NA,
          StatTypes = NA,
          stringsAsFactors = FALSE
        ))
      })
    }
    
    values$parsed_data <- results
    values$file_info <- file_info
    
    output$parse_status <- renderText({
      paste("Processed", nrow(input$rtf_files), "files.")
    })
    
    # Update file selection choices
    updateSelectInput(session, "selected_file", 
                     choices = names(results))
  })
  
  # Display parsed files table
  output$parsed_files_table <- renderTable({
    values$file_info
  })
  
  # Display summary stats
  output$summary_stats <- renderTable({
    if (nrow(values$file_info) > 0) {
      success_files <- values$file_info[values$file_info$Status == "Success", ]
      if (nrow(success_files) > 0) {
        data.frame(
          Metric = c("Total Files", "Successful", "Total Rows", "Avg Groups"),
          Value = c(
            nrow(values$file_info),
            nrow(success_files),
            sum(success_files$Rows, na.rm = TRUE),
            round(mean(success_files$Groups, na.rm = TRUE), 1)
          )
        )
      }
    }
  })
  
  # Sample data preview
  output$sample_data_table <- renderDT({
    if (length(values$parsed_data) > 0) {
      first_file <- values$parsed_data[[1]]
      if (!is.null(first_file$ard_data)) {
        datatable(head(first_file$ard_data, 100), options = list(scrollX = TRUE))
      }
    }
  })
  
  # Pattern Management Tab Logic - UPDATED
  output$current_bign_patterns <- renderText({
    patterns <- get_bign_patterns()
    paste(1:length(patterns), patterns, sep = ". ", collapse = "\n")
  })
  
  output$current_stat_patterns <- renderText({
    patterns <- get_stat_patterns()
    pattern_names <- names(patterns)
    if (length(pattern_names) > 0) {
      pattern_descriptions <- sapply(pattern_names, function(name) {
        template <- patterns[[name]]$template
        paste0(name, ": ", template)
      })
      paste(pattern_descriptions, collapse = "\n")
    } else {
      "No patterns defined"
    }
  })
  
  # Add BIGN pattern
  observeEvent(input$add_bign_btn, {
    req(input$new_bign_pattern)
    
    tryCatch({
      add_bign_pattern(input$new_bign_pattern, position = input$bign_priority)
      showNotification("BIGN pattern added successfully!", type = "success")
      updateTextInput(session, "new_bign_pattern", value = "")
    }, error = function(e) {
      showNotification(paste("Error adding pattern:", e$message), type = "error")
    })
  })
  
  # Use template button
  observeEvent(input$use_template_btn, {
    updateTextInput(session, "stat_pattern_template", value = input$common_templates)
  })
  
  # Add statistical pattern - UPDATED to use pseudo-patterns
  observeEvent(input$add_stat_btn, {
    req(input$stat_pattern_name, input$stat_pattern_template)
    
    tryCatch({
      # Parse labels if provided
      labels <- NULL
      if (nchar(input$stat_pattern_labels) > 0) {
        labels <- trimws(strsplit(input$stat_pattern_labels, ",")[[1]])
      }
      
      # Use add_pseudo_pattern function
      add_pseudo_pattern(
        name = input$stat_pattern_name,
        template = input$stat_pattern_template,
        labels = labels
      )
      
      showNotification("Statistical pattern added successfully!", type = "success")
      
      # Clear inputs
      updateTextInput(session, "stat_pattern_name", value = "")
      updateTextInput(session, "stat_pattern_template", value = "")
      updateTextInput(session, "stat_pattern_labels", value = "")
      
    }, error = function(e) {
      showNotification(paste("Error adding pattern:", e$message), type = "error")
    })
  })
  
  # Pattern testing - UPDATED
  observeEvent(input$test_pattern_btn, {
    req(input$test_value)
    
    tryCatch({
      result <- parse_stat_value_pseudo(input$test_value)
      values$pattern_test_result <- result
      
      # Show debug info
      patterns <- get_stat_patterns()
      debug_info <- paste0(
        "Testing value: '", input$test_value, "'\n",
        "Number of patterns: ", length(patterns), "\n",
        "First 3 patterns:\n"
      )
      
      for (i in 1:min(3, length(patterns))) {
        name <- names(patterns)[i]
        pat <- patterns[[name]]
        debug_info <- paste0(debug_info,
          "  ", name, ": ", pat$template, "\n",
          "    Regex: ", substr(pat$regex, 1, 50), "...\n"
        )
      }
      
      output$pattern_debug <- renderText(debug_info)
      
    }, error = function(e) {
      showNotification(paste("Error testing pattern:", e$message), type = "error")
      values$pattern_test_result <- data.frame(
        stat = "Error",
        stat_name = "Error",
        stat_label = e$message
      )
    })
  })
  
  output$pattern_test_results <- renderDT({
    if (!is.null(values$pattern_test_result)) {
      datatable(values$pattern_test_result, options = list(pageLength = 5))
    }
  })
  
  # Data Viewer Tab Logic
  output$file_summary <- renderTable({
    req(input$selected_file)
    
    if (input$selected_file %in% names(values$parsed_data)) {
      file_data <- values$parsed_data[[input$selected_file]]
      data.frame(
        Metric = c("Title", "Total Rows", "Unique Groups", "Stat Types"),
        Value = c(
          file_data$metadata$title %||% "N/A",
          nrow(file_data$ard_data),
          length(unique(file_data$ard_data$group1_level)),
          paste(unique(file_data$ard_data$stat_name), collapse = ", ")
        )
      )
    }
  })
  
  output$ard_data_table <- renderDT({
    req(input$selected_file)
    
    if (input$selected_file %in% names(values$parsed_data)) {
      file_data <- values$parsed_data[[input$selected_file]]
      datatable(file_data$ard_data, 
               options = list(scrollX = TRUE, pageLength = 25))
    }
  })
  
  # Download handler
  output$download_json <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$selected_file), "_ard.json")
    },
    content = function(file) {
      if (input$selected_file %in% names(values$parsed_data)) {
        file_data <- values$parsed_data[[input$selected_file]]
        jsonlite::write_json(file_data, file, pretty = TRUE, auto_unbox = TRUE)
      }
    }
  )
  
  # Data visualization
  output$data_plot <- renderPlotly({
    req(input$selected_file)
    
    if (input$selected_file %in% names(values$parsed_data)) {
      file_data <- values$parsed_data[[input$selected_file]]
      ard_data <- file_data$ard_data
      
      # Create a simple bar plot of stat counts by group
      stat_summary <- ard_data %>%
        filter(stat_name == "n") %>%
        group_by(group1_level) %>%
        summarise(count = n())
      
      plot_ly(stat_summary, x = ~group1_level, y = ~count, type = 'bar') %>%
        layout(title = "Count Statistics by Group",
               xaxis = list(title = "Group"),
               yaxis = list(title = "Count"))
    }
  })
  
  # LLM Query Tab Logic (unchanged)
  output$available_tables <- renderTable({
    if (length(values$parsed_data) > 0) {
      data.frame(
        Table = names(values$parsed_data),
        Title = sapply(values$parsed_data, function(x) x$metadata$title %||% "N/A"),
        Rows = sapply(values$parsed_data, function(x) nrow(x$ard_data))
      )
    }
  })
  
  observeEvent(input$submit_query_btn, {
    req(input$llm_query)
    
    # Simple query processing (could be enhanced with actual LLM integration)
    query <- tolower(input$llm_query)
    
    response <- "Query processing feature coming soon! "
    
    if (grepl("total.*patient", query)) {
      # Calculate total patients across tables
      if (length(values$parsed_data) > 0) {
        totals <- sapply(values$parsed_data, function(file_data) {
          bign_rows <- file_data$ard_data[file_data$ard_data$variable == "BIGN", ]
          if (nrow(bign_rows) > 0) {
            sum(as.numeric(bign_rows$stat), na.rm = TRUE)
          } else {
            0
          }
        })
        
        response <- paste0("Patient counts by table:\n",
                          paste(names(totals), totals, sep = ": ", collapse = "\n"),
                          "\n\nTotal across all tables: ", sum(totals))
      }
    }
    
    output$llm_response <- renderText({
      response
    })
    
    # Generate sample R code
    output$generated_code <- renderText({
      paste0(
        "# Generated R code for your query:\n",
        "library(dplyr)\n\n",
        "# Extract BIGN data from all tables\n",
        "bign_data <- bind_rows(\n",
        "  lapply(names(parsed_data), function(table_name) {\n",
        "    file_data <- parsed_data[[table_name]]\n",
        "    bign_rows <- file_data$ard_data[file_data$ard_data$variable == 'BIGN', ]\n",
        "    bign_rows$table_name <- table_name\n",
        "    return(bign_rows)\n",
        "  })\n",
        ")\n\n",
        "# Summarize patient counts\n",
        "summary(bign_data)"
      )
    })
  })
  
  # Export Tab Logic (unchanged)
  output$export_files_list <- renderTable({
    if (length(values$parsed_data) > 0) {
      data.frame(
        File = names(values$parsed_data),
        Format = "JSON",
        Size = sapply(values$parsed_data, function(x) {
          paste(nrow(x$ard_data), "rows")
        })
      )
    }
  })
  
  output$export_summary <- renderTable({
    if (length(values$parsed_data) > 0) {
      data.frame(
        Metric = c("Total Files", "Total ARD Rows", "Unique Variables"),
        Value = c(
          length(values$parsed_data),
          sum(sapply(values$parsed_data, function(x) nrow(x$ard_data))),
          length(unique(unlist(lapply(values$parsed_data, function(x) unique(x$ard_data$variable)))))
        )
      )
    }
  })
  
  # Download all as ZIP
  output$download_zip <- downloadHandler(
    filename = function() {
      paste0("stateful_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      # Create temp directory
      temp_dir <- tempdir()
      export_dir <- file.path(temp_dir, "stateful_export")
      dir.create(export_dir, showWarnings = FALSE)
      
      # Export each file
      for (name in names(values$parsed_data)) {
        json_file <- file.path(export_dir, paste0(tools::file_path_sans_ext(name), "_ard.json"))
        jsonlite::write_json(values$parsed_data[[name]], json_file, pretty = TRUE, auto_unbox = TRUE)
      }
      
      # Create summary if requested
      if (input$include_summary) {
        summary_data <- data.frame(
          file = names(values$parsed_data),
          rows = sapply(values$parsed_data, function(x) nrow(x$ard_data)),
          groups = sapply(values$parsed_data, function(x) length(unique(x$ard_data$group1_level)))
        )
        write.csv(summary_data, file.path(export_dir, "summary.csv"), row.names = FALSE)
      }
      
      # Create ZIP
      old_wd <- getwd()
      setwd(export_dir)
      zip(file, list.files())
      setwd(old_wd)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)