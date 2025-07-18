# Stateful Shiny Application - RTF to JSON Converter
library(shiny)
library(shinydashboard)
library(DT)
library(jsonlite)
library(dplyr)
library(tools)

# Source batch processing functions which includes all dependencies
source('R/batch_processing_functions.R')

# Load .Renviron if it exists
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Stateful RTF to JSON Converter"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("RTF Converter", tabName = "converter", icon = icon("exchange-alt")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
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
      # RTF Converter Tab
      tabItem(tabName = "converter",
        fluidRow(
          box(title = "RTF to JSON Converter", status = "primary", solidHeader = TRUE, width = 12,
            p("Convert RTF clinical trial tables to standardized JSON format."),
            fileInput("rtf_file", "Choose RTF file:", 
                     accept = c(".rtf"), multiple = FALSE),
            br(),
            actionButton("convert_btn", "Convert to JSON", class = "btn-primary"),
            br(), br(),
            textOutput("convert_status")
          )
        ),
        
        fluidRow(
          box(title = "Preview", status = "info", solidHeader = TRUE, width = 12,
            h4("Data Preview:"),
            DTOutput("preview_table"),
            br(),
            h4("Metadata:"),
            verbatimTextOutput("metadata_preview")
          )
        ),
        
        fluidRow(
          box(title = "Download", status = "success", solidHeader = TRUE, width = 12,
            downloadButton("download_json", "Download JSON", class = "btn-success"),
            br(), br(),
            p("JSON file will be named: [filename]_state_parser.json")
          )
        )
      ),
      
      # Help Tab
      tabItem(tabName = "help",
        fluidRow(
          box(title = "Help & Documentation", status = "info", solidHeader = TRUE, width = 12,
            h3("RTF to JSON Converter"),
            p("This application converts RTF clinical trial tables to standardized JSON format."),
            
            h4("How to use:"),
            tags$ol(
              tags$li("Click 'Browse' to select an RTF file"),
              tags$li("Click 'Convert to JSON' to process the file"),
              tags$li("Review the data preview and metadata"),
              tags$li("Click 'Download JSON' to save the output")
            ),
            
            h4("Output Format:"),
            p("The JSON output contains:"),
            tags$ul(
              tags$li("data: Array of parsed table records in ARD format"),
              tags$li("metadata: Document information including title and footnotes")
            ),
            
            h4("File Naming:"),
            p("Output files are named: [original_filename]_state_parser.json"),
            
            br(),
            h4("Support:"),
            p("For technical support, contact the stateful package maintainer.")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  json_result <- reactiveVal(NULL)
  
  # Convert button logic
  observeEvent(input$convert_btn, {
    req(input$rtf_file)
    
    output$convert_status <- renderText("Processing RTF file...")
    
    tryCatch({
      # Get file paths
      rtf_path <- input$rtf_file$datapath
      
      # Create temporary JSON path
      temp_json_path <- tempfile(fileext = ".json")
      
      # Use the batch processing function
      process_rtf_file(rtf_path, temp_json_path)
      
      # Read the generated JSON
      json_data <- jsonlite::read_json(temp_json_path)
      json_result(json_data)
      
      # Display preview
      if (!is.null(json_data$data) && length(json_data$data) > 0) {
        # Convert data to data frame for display
        df_data <- jsonlite::fromJSON(jsonlite::toJSON(json_data$data))
        
        output$preview_table <- renderDT({
          datatable(df_data,
                    options = list(pageLength = 10, scrollX = TRUE),
                    filter = "top")
        })
        
        # Display metadata
        output$metadata_preview <- renderPrint({
          list(
            title = json_data$metadata$title,
            footnotes = json_data$metadata$footnotes,
            parser = json_data$metadata$parser,
            timestamp = json_data$metadata$timestamp
          )
        })
        
        output$convert_status <- renderText(
          paste("Conversion complete:", nrow(df_data), "records extracted")
        )
      } else {
        output$convert_status <- renderText("No data extracted from RTF file")
      }
      
    }, error = function(e) {
      output$convert_status <- renderText(paste("Error:", e$message))
      showNotification(paste("Conversion failed:", e$message), type = "error")
    })
  })
  
  # Download handler for JSON
  output$download_json <- downloadHandler(
    filename = function() {
      if (!is.null(input$rtf_file)) {
        base_name <- tools::file_path_sans_ext(input$rtf_file$name)
        paste0(base_name, "_state_parser.json")
      } else {
        paste0("converted_state_parser.json")
      }
    },
    content = function(file) {
      json_data <- json_result()
      if (!is.null(json_data)) {
        jsonlite::write_json(json_data, file, pretty = TRUE, auto_unbox = TRUE)
      }
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)