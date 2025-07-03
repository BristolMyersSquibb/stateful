test_that("state parser handles edge cases and errors", {
  
  # Test clean_variable_text function with various inputs
  expect_equal(clean_variable_text("Arm C: Chemo{ }N = 115"), "Arm C: Chemo N = 115")
  expect_equal(clean_variable_text("Test{  }Value"), "Test Value")
  expect_equal(clean_variable_text("Multiple  spaces"), "Multiple spaces")
  expect_equal(clean_variable_text(""), "")
  expect_equal(clean_variable_text(NULL), "")
  
  # Test with non-character inputs that should cause the error
  expect_error(clean_variable_text(123), NA)  # Should not error, should handle gracefully
  expect_error(clean_variable_text(list(a = 1)), NA)  # Should not error
  expect_error(clean_variable_text(factor("test")), NA)  # Should not error
})

test_that("state parser stage1 handles file upload scenarios", {
  
  # Test with sample RTF files
  sample_files <- list.files(system.file("extdata", package = "stateful"), 
                            pattern = "\\.rtf$", full.names = TRUE)
  
  if (length(sample_files) > 0) {
    # Test normal parsing
    result <- state_parser_stage1(sample_files[1])
    
    expect_true(is.list(result))
    expect_true("data" %in% names(result))
    expect_true("metadata" %in% names(result))
    expect_true(is.data.frame(result$data))
    
    # Check required columns
    if (nrow(result$data) > 0) {
      expect_true("variable" %in% names(result$data))
      expect_true("variable_level" %in% names(result$data))
      expect_true("group1" %in% names(result$data))
      expect_true("raw_stat" %in% names(result$data))
      expect_true("source_row" %in% names(result$data))
      expect_true("source_col" %in% names(result$data))
      
      # Check that all text fields are character
      expect_true(is.character(result$data$variable))
      expect_true(is.character(result$data$variable_level))
      expect_true(is.character(result$data$group1))
      expect_true(is.character(result$data$raw_stat))
      
      # Check for curly braces - should be cleaned
      expect_false(any(grepl("\\{.*\\}", result$data$variable)))
      expect_false(any(grepl("\\{.*\\}", result$data$variable_level)))
      expect_false(any(grepl("\\{.*\\}", result$data$group1)))
    }
  } else {
    skip("No sample RTF files available for testing")
  }
})

test_that("state parser handles invalid file inputs", {
  
  # Test with non-existent file
  expect_error(state_parser_stage1("nonexistent.rtf"))
  
  # Test with empty file path
  expect_error(state_parser_stage1(""))
  
  # Test with NULL input
  expect_error(state_parser_stage1(NULL))
})

test_that("extract_variable_hierarchy handles edge cases", {
  
  # Mock table rows for testing
  mock_rows <- list(
    list(
      index = 1,
      cells = list(
        list(text = "Parent Category", rtf_content = "\\li0 Parent Category"),
        list(text = "100", rtf_content = "100")
      )
    ),
    list(
      index = 2, 
      cells = list(
        list(text = "  Subcategory{ }A", rtf_content = "\\li360   Subcategory{ }A"),
        list(text = "50", rtf_content = "50")
      )
    ),
    list(
      index = 3,
      cells = list(
        list(text = NULL, rtf_content = ""),  # NULL text should not break
        list(text = "25", rtf_content = "25")
      )
    )
  )
  
  # This should not error even with edge cases
  expect_error(extract_variable_hierarchy(mock_rows), NA)
  
  # Test with empty list
  expect_error(extract_variable_hierarchy(list()), NA)
  
  # Test with NULL
  expect_error(extract_variable_hierarchy(NULL), NA)
})

test_that("process_table_rows handles data type consistency", {
  
  # Mock header groups
  mock_header_groups <- data.frame(
    column_index = c(2, 3, 4),
    group_name = c("Treatment A", "Treatment B", "Control"),
    stringsAsFactors = FALSE
  )
  
  # Mock table rows with various data types
  mock_rows <- list(
    list(
      index = 1,
      cells = list(
        list(text = "Variable 1"),
        list(text = "100"),
        list(text = "200"),
        list(text = "300")
      )
    ),
    list(
      index = 2,
      cells = list(
        list(text = factor("Variable 2")),  # Factor input
        list(text = 150),  # Numeric input
        list(text = "250"),
        list(text = NULL)  # NULL input
      )
    )
  )
  
  # This should handle mixed data types gracefully
  expect_error(process_table_rows(mock_rows, mock_header_groups), NA)
  
  result <- process_table_rows(mock_rows, mock_header_groups)
  
  # All columns should be character type in the result
  expect_true(is.character(result$variable))
  expect_true(is.character(result$variable_level))
  expect_true(is.character(result$group1))
  expect_true(is.character(result$raw_stat))
})

test_that("state parser handles Shiny file upload format", {
  
  # Simulate Shiny fileInput structure
  sample_files <- list.files(system.file("extdata", package = "stateful"), 
                            pattern = "\\.rtf$", full.names = TRUE)
  
  if (length(sample_files) > 0) {
    # Mock Shiny input$file structure
    mock_file_input <- data.frame(
      name = c("test1.rtf", "test2.rtf"),
      size = c(1000, 2000),
      type = c("text/rtf", "text/rtf"),
      datapath = c(sample_files[1], sample_files[min(2, length(sample_files))]),
      stringsAsFactors = FALSE
    )
    
    # Test processing multiple files like in the Shiny app
    all_data <- data.frame()
    results <- data.frame(File = character(), Status = character(), Rows = integer())
    
    for (i in 1:nrow(mock_file_input)) {
      file_path <- mock_file_input$datapath[i]
      file_name <- mock_file_input$name[i]
      
      # This should not error
      expect_error({
        stage1_result <- state_parser_stage1(file_path)
        if (!is.null(stage1_result) && nrow(stage1_result$data) > 0) {
          stage1_result$data$source_file <- file_name
          all_data <- rbind(all_data, stage1_result$data)
        }
      }, NA)
    }
    
    # Check final result
    if (nrow(all_data) > 0) {
      expect_true("source_file" %in% names(all_data))
      expect_true(is.character(all_data$source_file))
    }
  } else {
    skip("No sample RTF files available for Shiny upload simulation")
  }
})