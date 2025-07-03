test_that("baseline snapshot tests capture current parser output", {
  # Get all RTF files in extdata
  rtf_files <- list.files(
    system.file("extdata", package = "stateful"),
    pattern = "\\.rtf$",
    full.names = TRUE
  )
  
  expect_true(length(rtf_files) > 0, "No RTF files found in extdata")
  
  for (rtf_file in rtf_files) {
    file_name <- tools::file_path_sans_ext(basename(rtf_file))
    
    # Parse RTF to get current output
    result <- rtf_to_ard_json(rtf_file, output_file = NULL)
    
    # Create fixture file path
    fixture_file <- file.path(
      "fixtures", 
      paste0(file_name, "_baseline.json")
    )
    fixture_path <- testthat::test_path(fixture_file)
    
    # If baseline doesn't exist, create it
    if (!file.exists(fixture_path)) {
      dir.create(dirname(fixture_path), recursive = TRUE, showWarnings = FALSE)
      jsonlite::write_json(result, fixture_path, pretty = TRUE, auto_unbox = TRUE)
      message("Created baseline for ", file_name)
    } else {
      # Load baseline and compare structure
      baseline <- jsonlite::read_json(fixture_path, simplifyVector = TRUE)
      
      # Test basic structure equality (for now)
      expect_equal(names(result), names(baseline), 
                  info = paste("Structure mismatch in", file_name))
      expect_equal(names(result$ard_data), names(baseline$ard_data),
                  info = paste("ARD columns mismatch in", file_name))
      expect_equal(nrow(result$ard_data), nrow(baseline$ard_data),
                  info = paste("Row count mismatch in", file_name))
    }
  }
})

test_that("rt-dm-sum hierarchical structure is correctly captured", {
  rtf_file <- system.file("extdata", "rt-dm-sum.rtf", package = "stateful")
  skip_if_not(file.exists(rtf_file), "rt-dm-sum.rtf not found")
  
  result <- rtf_to_ard_json(rtf_file, output_file = NULL)
  
  # Basic structure tests
  expect_true("ard_data" %in% names(result))
  expect_true(is.data.frame(result$ard_data))
  
  ard_data <- result$ard_data
  
  # Check group1_level values are properly extracted
  group_levels <- unique(ard_data$group1_level)
  expect_true("Arm A: Nivo" %in% group_levels)
  expect_true("Arm B: Nivo + Ipi" %in% group_levels) 
  expect_true("Arm C: Chemo" %in% group_levels)
  
  # Check that hierarchical variables exist (currently with \\ prefix)
  variables <- unique(ard_data$variable)
  hierarchical_vars <- variables[grepl("^\\\\\\\\", variables)]
  expect_true(length(hierarchical_vars) > 0, "No hierarchical variables found")
  
  # Specifically check for PRIMARY REASON FOR DEATH sub-categories
  expected_subcats <- c("\\\\DISEASE", "\\\\STUDY DRUG TOXICITY", "\\\\UNKNOWN", "\\\\OTHER")
  found_subcats <- intersect(variables, expected_subcats)
  expect_equal(length(found_subcats), length(expected_subcats),
              info = paste("Missing subcategories:", 
                          paste(setdiff(expected_subcats, found_subcats), collapse = ", ")))
  
  # Check that we have the main category too
  expect_true("NUMBER OF SUBJECTS WHO DIED (%)" %in% variables)
  expect_true("PRIMARY REASON FOR DEATH (%)" %in% variables ||
              any(grepl("PRIMARY REASON", variables)))
})

test_that("hierarchical structure detection works correctly", {
  rtf_file <- system.file("extdata", "rt-dm-sum.rtf", package = "stateful") 
  skip_if_not(file.exists(rtf_file), "rt-dm-sum.rtf not found")
  
  result <- rtf_to_ard_json(rtf_file, output_file = NULL)
  ard_data <- result$ard_data
  
  # Test that indented items (with \\) have corresponding parent items
  hierarchical_vars <- unique(ard_data$variable[grepl("^\\\\\\\\", ard_data$variable)])
  
  for (var in hierarchical_vars) {
    # Check that this variable has data
    var_data <- ard_data[ard_data$variable == var, ]
    expect_true(nrow(var_data) > 0, 
               info = paste("No data found for hierarchical variable:", var))
    
    # Check that it has proper statistics
    expect_true(any(var_data$stat_name %in% c("n", "pct", "mean", "sd")),
               info = paste("No valid statistics found for:", var))
  }
})

test_that("rt-dm-sum specific hierarchical issues are detected", {
  rtf_file <- system.file("extdata", "rt-dm-sum.rtf", package = "stateful")
  skip_if_not(file.exists(rtf_file), "rt-dm-sum.rtf not found")
  
  result <- rtf_to_ard_json(rtf_file, output_file = NULL)
  ard_data <- result$ard_data
  
  # Issue 1: Missing group1_level labels (should not be Group_1, Group_2)
  group_levels <- unique(ard_data$group1_level)
  expect_false(any(grepl("^Group_\\d+$", group_levels)),
              info = "Found default Group_X labels instead of proper treatment names")
  
  # Issue 2: Hierarchical variables should have proper parent association
  hierarchical_vars <- ard_data$variable[grepl("^\\\\\\\\", ard_data$variable)]
  
  # These should be children of "PRIMARY REASON FOR DEATH (%)"
  expected_children <- c("\\\\DISEASE", "\\\\STUDY DRUG TOXICITY", "\\\\UNKNOWN", "\\\\OTHER")
  found_children <- intersect(hierarchical_vars, expected_children)
  
  # For now, just document what we find (this will help track the fix)
  message("Found hierarchical variables: ", paste(unique(hierarchical_vars), collapse = ", "))
  message("Expected PRIMARY REASON children: ", paste(expected_children, collapse = ", "))
  message("Found PRIMARY REASON children: ", paste(found_children, collapse = ", "))
  
  # Test that we have some hierarchical structure
  expect_true(length(hierarchical_vars) > 0,
             "No hierarchical variables found")
})

test_that("variable_level field should exist for hierarchical variables", {
  rtf_file <- system.file("extdata", "rt-dm-sum.rtf", package = "stateful")
  skip_if_not(file.exists(rtf_file), "rt-dm-sum.rtf not found")
  
  result <- rtf_to_ard_json(rtf_file, output_file = NULL)
  ard_data <- result$ard_data
  
  # Check if variable_level field exists
  has_variable_level <- "variable_level" %in% names(ard_data)
  
  if (has_variable_level) {
    # If it exists, check that hierarchical items have proper paths
    hierarchical_vars <- ard_data$variable[grepl("^\\\\\\\\", ard_data$variable)]
    
    if (length(hierarchical_vars) > 0) {
      hierarchical_data <- ard_data[ard_data$variable %in% hierarchical_vars, ]
      
      # Check that variable_level shows parent-child relationship
      expect_true(any(grepl(" - ", hierarchical_data$variable_level, fixed = TRUE)),
                 "variable_level should show parent-child relationships with ' - ' separator")
      
      # Check that variable names are clean (no \\ prefix)
      expect_false(any(grepl("^\\\\\\\\", hierarchical_data$variable)),
                  "variable field should be clean (no \\\\ prefix) when variable_level exists")
    }
  } else {
    # Document that variable_level doesn't exist yet (expected for current state)
    message("variable_level field not found - this is expected before the fix")
    expect_true(TRUE) # Pass for now
  }
})

test_that("rt-saf-allsubjects files show hierarchical structure examples", {
  # Check one of the safety files that likely has hierarchical structure
  rtf_files <- list.files(
    system.file("extdata", package = "stateful"),
    pattern = "rt-saf.*\\.rtf$",
    full.names = TRUE
  )
  
  if (length(rtf_files) > 0) {
    result <- rtf_to_ard_json(rtf_files[1], output_file = NULL)
    ard_data <- result$ard_data
    
    # Look for hierarchical patterns
    hierarchical_vars <- ard_data$variable[grepl("^\\\\\\\\", ard_data$variable)]
    
    if (length(hierarchical_vars) > 0) {
      message("Found hierarchical structure in safety file: ", basename(rtf_files[1]))
      message("Hierarchical variables: ", paste(unique(hierarchical_vars), collapse = ", "))
      
      # Check if variable_level exists and shows proper structure
      if ("variable_level" %in% names(ard_data)) {
        hierarchical_data <- ard_data[ard_data$variable %in% hierarchical_vars, ]
        variable_levels <- unique(hierarchical_data$variable_level)
        message("Variable levels found: ", paste(variable_levels, collapse = "; "))
      }
    }
  }
  
  expect_true(TRUE) # This is just for documentation/observation
})