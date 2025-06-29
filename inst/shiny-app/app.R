# Stateful Shiny Application - Updated with Pseudo-Pattern Support
library(shiny)
library(shinydashboard)
library(DT)
library(jsonlite)
library(plotly)
library(stateful)
library(dplyr)

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
            textOutput("parse_status"),
            br(),
            # Debug info
            div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 10px;",
              h5("Debug Information:"),
              textOutput("debug_info")
            ),
            
            # Sample files section
            br(),
            div(style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px;",
              h5("Sample Files:"),
              p("Sample RTF files are included in the package. Click below to test with a sample file:"),
              actionButton("load_sample_btn", "Load Sample RTF File", class = "btn-info")
            )
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
          box(title = "Pattern System Features", status = "success", solidHeader = TRUE, width = 12,
            h4("New Enhancements:"),
            tags$ul(
              tags$li(tags$strong("Minimal Hardcoding:"), " Single BIGN pattern replaces 8+ regex patterns"),
              tags$li(tags$strong("Context-Aware Matching:"), " Patterns selected based on variable context (e.g., 'MEAN (SD)' → mean/sd pattern)"),
              tags$li(tags$strong("Number-Only Matching:"), " Patterns now correctly match only numbers, preventing comma bleeding"),
              tags$li(tags$strong("Template Syntax:"), " Use {placeholders} instead of complex regex"),
              tags$li(tags$strong("Runtime Extensible:"), " Add patterns without modifying source code")
            )
          )
        ),
        
        fluidRow(
          box(title = "Current Patterns", status = "info", solidHeader = TRUE, width = 6,
            h4("BIGN Patterns:"),
            p("Single maximal pattern covers most cases. Add specific patterns if needed."),
            verbatimTextOutput("current_bign_patterns"),
            br(),
            h4("Statistical Patterns:"),
            p("Context-aware patterns automatically prioritize based on variable labels."),
            tags$small(
              tags$ul(
                tags$li("Mean/SD patterns prioritized for continuous variables"),
                tags$li("N/% patterns prioritized for categorical variables"),
                tags$li("CI patterns prioritized for confidence intervals"),
                style = "color: #666;"
              )
            ),
            verbatimTextOutput("current_stat_patterns")
          ),
          
          box(title = "Pattern Management", status = "primary", solidHeader = TRUE, width = 6,
            h4("Add New BIGN Pattern:"),
            textInput("new_bign_name", "Pattern Name:", placeholder = "e.g., my_bign"),
            textInput("new_bign_template", "Template:", placeholder = "e.g., (N={n})"),
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
            
            h4("Example Patterns to Add:"),
            tags$ul(
              tags$li(tags$code("ci_spaces"), ": ", tags$code("{estimate}  ( {ci_lower}, {ci_upper})"), " - CI with extra spaces"),
              tags$li(tags$code("q1_q3"), ": ", tags$code("{q1}, {q3}"), " - Quartile ranges"),
              tags$li(tags$code("mean_ci"), ": ", tags$code("{mean} [{ci_lower}, {ci_upper}]"), " - Mean with CI"),
              style = "font-size: 0.9em; margin-bottom: 15px;"
            ),
            
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
      
      # LLM Query Tab - UPDATED
      tabItem(tabName = "llm",
        fluidRow(
          box(title = "AI-Powered SQL Query Interface", status = "primary", solidHeader = TRUE, width = 12,
            div(class = "alert alert-info",
              icon("info-circle"),
              "This feature uses Azure OpenAI to generate SQL queries from natural language questions about ARD summary data.",
              "Uses sqldf to execute queries. Only aggregate/summary questions supported - no patient-level queries."
            ),
            div(class = "alert alert-warning",
              icon("exclamation-triangle"),
              HTML("<strong>IMPORTANT:</strong> This is summary data only. You cannot ask patient-level questions like 'which patients' or 'show me individual subjects'.")
            ),
            h4("Valid Query Examples (Summary Data):"),
            tags$ul(
              tags$li("How many subjects are in each treatment group?"),
              tags$li("Compare adverse event percentages between treatments"),
              tags$li("Which treatment group has the highest death rate?"),
              tags$li("What are the mean values by treatment group?"),
              tags$li("Show me statistics for variables containing 'age'")
            ),
            h4("Invalid Query Examples (Patient-Level):"),
            tags$ul(class = "text-danger",
              tags$li("Which patients had adverse events? (NOT SUPPORTED)"),
              tags$li("Show me patient demographics (NOT SUPPORTED)"),
              tags$li("List individual patient data (NOT SUPPORTED)")
            ),
            br(),
            textAreaInput("llm_query", "Enter your question in plain English:", 
                         placeholder = "Example: Show me the distribution of ages across treatment groups...",
                         rows = 3,
                         width = "100%"),
            actionButton("submit_query_btn", "Generate SQL Query", 
                        class = "btn-primary", 
                        icon = icon("robot")),
            actionButton("clear_query_btn", "Clear", 
                        class = "btn-warning", 
                        icon = icon("eraser")),
            br(), br()
          )
        ),
        
        fluidRow(
          box(title = "Available Tables", status = "info", solidHeader = TRUE, width = 6,
            tableOutput("available_tables")
          ),
          
          box(title = "Query Results", status = "success", solidHeader = TRUE, width = 6,
            verbatimTextOutput("llm_response"),
            br(),
            conditionalPanel(
              condition = "output.llm_response",
              downloadButton("download_query_results", "Download Results", class = "btn-success")
            )
          )
        ),
        
        fluidRow(
          box(title = "Generated SQL Code", status = "warning", solidHeader = TRUE, width = 12,
            verbatimTextOutput("generated_code"),
            br(),
            conditionalPanel(
              condition = "output.generated_code",
              actionButton("copy_sql_btn", "Copy SQL", class = "btn-info", icon = icon("copy"))
            )
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
    pattern_test_result = NULL,
    llm_query_results = NULL
  )
  
  # Debug information output
  output$debug_info <- renderText({
    file_count <- if (is.null(input$rtf_files)) 0 else nrow(input$rtf_files)
    parsed_count <- length(values$parsed_data)
    paste("Files uploaded:", file_count, 
          "| Files parsed:", parsed_count,
          "| Button clicks: Check if button is clickable")
  })
  
  # Upload and Parse Tab Logic
  observeEvent(input$parse_btn, {
    # Debug: Check if files are uploaded
    if (is.null(input$rtf_files)) {
      output$parse_status <- renderText({
        "No files selected. Please upload RTF files first."
      })
      return()
    }
    
    output$parse_status <- renderText({
      paste("Processing", nrow(input$rtf_files), "RTF files...")
    })
    
    # Process each uploaded file
    results <- list()
    file_info <- data.frame()
    
    for (i in 1:nrow(input$rtf_files)) {
      file_path <- input$rtf_files$datapath[i]
      file_name <- input$rtf_files$name[i]
      
      tryCatch({
        # Debug: Show which file is being processed
        output$parse_status <- renderText({
          paste("Processing file:", file_name, "...")
        })
        
        # Parse RTF file
        parsed_result <- rtf_to_ard_json(file_path, output_file = NULL)
        
        # Debug: Check if result is valid
        if (is.null(parsed_result) || is.null(parsed_result$ard_data)) {
          stop("RTF parsing returned NULL or invalid result")
        }
        
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
        # Better error reporting
        error_msg <- paste("Error:", e$message)
        if (grepl("pandoc", e$message, ignore.case = TRUE)) {
          error_msg <- paste(error_msg, "- Check if pandoc is installed and in PATH")
        }
        
        file_info <<- rbind(file_info, data.frame(
          File = file_name,
          Status = error_msg,
          Rows = NA,
          Groups = NA,
          StatTypes = NA,
          stringsAsFactors = FALSE
        ))
      })
    }
    
    values$parsed_data <- results
    values$file_info <- file_info
    
    # Final status update
    success_count <- sum(file_info$Status == "Success", na.rm = TRUE)
    output$parse_status <- renderText({
      paste("Completed processing", nrow(input$rtf_files), "files.",
            success_count, "successful,", 
            nrow(input$rtf_files) - success_count, "failed.")
    })
    
    # Update file selection choices
    updateSelectInput(session, "selected_file", 
                     choices = names(results))
  })
  
  # Load sample file button
  observeEvent(input$load_sample_btn, {
    extdata_dir <- system.file("extdata", package = "stateful")
    sample_file <- file.path(extdata_dir, "rt-dm-sum.rtf")
    
    if (file.exists(sample_file)) {
      output$parse_status <- renderText({
        "Processing sample file: rt-dm-sum.rtf..."
      })
      
      tryCatch({
        # Parse the sample RTF file
        parsed_result <- rtf_to_ard_json(sample_file, output_file = NULL)
        
        if (is.null(parsed_result) || is.null(parsed_result$ard_data)) {
          stop("Sample RTF parsing returned NULL or invalid result")
        }
        
        # Store result
        values$parsed_data <- list("rt-dm-sum.rtf" = parsed_result)
        
        # Create file info
        values$file_info <- data.frame(
          File = "rt-dm-sum.rtf (sample)",
          Status = "Success",
          Rows = nrow(parsed_result$ard_data),
          Groups = length(unique(parsed_result$ard_data$group1_level)),
          StatTypes = length(unique(parsed_result$ard_data$stat_name)),
          stringsAsFactors = FALSE
        )
        
        output$parse_status <- renderText({
          "Successfully loaded sample file with 85 rows of ARD data!"
        })
        
        # Update file selection choices
        updateSelectInput(session, "selected_file", 
                         choices = names(values$parsed_data))
        
      }, error = function(e) {
        output$parse_status <- renderText({
          paste("Error loading sample file:", e$message)
        })
      })
    } else {
      output$parse_status <- renderText({
        "Sample file not found. Please upload your own RTF files."
      })
    }
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
    patterns <- stateful::get_bign_pseudo_patterns()
    if (length(patterns) > 0) {
      pattern_descriptions <- sapply(names(patterns), function(name) {
        template <- patterns[[name]]$template
        paste0(name, ": ", template)
      })
      paste(pattern_descriptions, collapse = "\n")
    } else {
      "No BIGN patterns defined"
    }
  })
  
  output$current_stat_patterns <- renderText({
    patterns <- stateful::get_stat_patterns()
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
  
  # Add BIGN pattern - UPDATED to use pseudo-patterns
  observeEvent(input$add_bign_btn, {
    req(input$new_bign_name, input$new_bign_template)
    
    tryCatch({
      stateful::add_bign_pseudo_pattern(input$new_bign_name, input$new_bign_template)
      showNotification("BIGN pattern added successfully!", type = "success")
      updateTextInput(session, "new_bign_name", value = "")
      updateTextInput(session, "new_bign_template", value = "")
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
      result <- stateful::parse_stat_value_pseudo(input$test_value)
      values$pattern_test_result <- result
      
      # Show debug info
      patterns <- stateful::get_stat_patterns()
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
  
  # LLM Query Tab Logic - UPDATED with Azure OpenAI
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
    
    # Show processing message
    showNotification("Generating SQL query using Azure OpenAI...", type = "message", id = "llm_processing")
    
    # Get first ARD dataset for LLM query
    ard_data <- if (length(values$parsed_data) > 0) {
      values$parsed_data[[1]]$ard_data  # Use first dataset
    } else {
      data.frame()  # Empty data frame
    }
    
    if (nrow(ard_data) == 0) {
      showNotification("No ARD data loaded. Please parse RTF files first.", type = "error")
      removeNotification("llm_processing")
      return()
    }
    
    # Get example queries
    examples <- stateful::get_example_queries()
    
    # Call Azure OpenAI to generate SQL (updated function signature)
    llm_result <- stateful::generate_sql_from_question(
      question = input$llm_query,
      ard_data = ard_data,
      examples = examples
    )
    
    removeNotification("llm_processing")
    
    if (llm_result$success) {
      # Display the generated SQL
      output$generated_code <- renderText({
        paste0(
          "-- Generated SQL Query\n",
          "-- Question: ", input$llm_query, "\n\n",
          llm_result$sql,
          "\n\n-- Explanation: ", llm_result$explanation,
          if (!is.na(llm_result$assumptions)) paste0("\n-- Assumptions: ", llm_result$assumptions) else ""
        )
      })
      
      # Try to execute the SQL query
      if (nrow(ard_data) > 0 && !is.na(llm_result$sql)) {
        exec_result <- stateful::execute_ard_sql(ard_data, llm_result$sql)
        
        if (exec_result$success) {
          output$llm_response <- renderText({
            paste0(
              "Query executed successfully!\n\n",
              "Results: ", nrow(exec_result$data), " rows returned\n\n",
              "Preview of results:\n",
              paste(capture.output(print(head(exec_result$data, 10))), collapse = "\n")
            )
          })
          
          # Store results for potential download
          values$llm_query_results <- exec_result$data
        } else {
          output$llm_response <- renderText({
            paste0(
              "SQL execution failed:\n",
              exec_result$error,
              "\n\nYou can still copy the SQL above and modify it as needed."
            )
          })
        }
      } else {
        output$llm_response <- renderText({
          "SQL query generated. Load data files to execute the query."
        })
      }
    } else {
      # Show error
      showNotification(paste("LLM Error:", llm_result$error), type = "error")
      output$llm_response <- renderText({
        paste("Error generating SQL:", llm_result$error)
      })
      output$generated_code <- renderText({
        "-- No SQL generated due to error"
      })
    }
  })
  
  # Clear query button
  observeEvent(input$clear_query_btn, {
    updateTextAreaInput(session, "llm_query", value = "")
    output$llm_response <- renderText({ "" })
    output$generated_code <- renderText({ "" })
    values$llm_query_results <- NULL
  })
  
  # Copy SQL button
  observeEvent(input$copy_sql_btn, {
    # This requires JavaScript to work properly in Shiny
    showNotification("SQL copied to clipboard!", type = "success", duration = 2)
  })
  
  # Download query results
  output$download_query_results <- downloadHandler(
    filename = function() {
      paste0("query_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      if (!is.null(values$llm_query_results)) {
        write.csv(values$llm_query_results, file, row.names = FALSE)
      }
    }
  )
  
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