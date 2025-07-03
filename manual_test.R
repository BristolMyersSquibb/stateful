# Manual testing script for state parser
# Run each line individually to inspect output

setwd("/home/tarapk/stateful")
pkgload::load_all()

# Option to enable/disable hierarchical labeling
USE_HIERARCHY <- TRUE  # Set to FALSE to see flat structure

# Helper function to run state parser with hierarchy option
test_file <- function(file_path, use_hierarchy = USE_HIERARCHY) {
  # Temporarily modify the hierarchy detection if needed
  if (!use_hierarchy) {
    # Could add a parameter to state_parser_stage1 to disable hierarchy
    cat("Running with hierarchy disabled\n")
  }
  
  result <- state_parser_stage1(file_path)
  
  cat("File:", basename(file_path), "\n")
  cat("Rows:", nrow(result$data), "\n")
  cat("Columns:", paste(names(result$data), collapse = ", "), "\n")
  
  # Show metadata
  if (!is.null(result$metadata)) {
    cat("\nMetadata:\n")
    for (name in names(result$metadata)) {
      if (name != "footnotes") {
        cat("  ", name, ": ", as.character(result$metadata[[name]]), "\n", sep = "")
      }
    }
    if (!is.null(result$metadata$footnotes) && length(result$metadata$footnotes) > 0) {
      cat("  footnotes: ", length(result$metadata$footnotes), " footnote(s)\n", sep = "")
    }
  }
  
  if (nrow(result$data) > 0) {
    cat("\nFirst 5 rows:\n")
    print(head(result$data, 5))
    
    cat("\nUnique variables:\n")
    print(unique(result$data$variable))
    
    if ("variable_level" %in% names(result$data)) {
      hierarchical <- result$data[result$data$variable != result$data$variable_level, ]
      if (nrow(hierarchical) > 0) {
        cat("\nHierarchical examples:\n")
        examples <- unique(hierarchical[c("variable", "variable_level")])
        print(head(examples, 5))
      }
    }
  }
  
  return(result)
}

# ============================================================================
# MANUAL TEST LINES - Run each one individually
# ============================================================================

# Test 1: rt-dm-sum.rtf (the hierarchical one)
result1 <- test_file("inst/extdata/rtf_inputs/rt-dm-sum.rtf")

# Test 2: rt-dm-demo.rtf 
result2 <- test_file("inst/extdata/rtf_inputs/rt-dm-demo.rtf")

# Test 3: rt-ae-ae1.rtf
result3 <- test_file("inst/extdata/rtf_inputs/rt-ae-ae1.rtf")

# Test 4: rt-saf-allsubjects_1.rtf
result4 <- test_file("inst/extdata/rtf_inputs/rt-saf-allsubjects_1.rtf")

# Test 5: rt-ef-acr20.rtf
result5 <- test_file("inst/extdata/rtf_inputs/rt-ef-acr20.rtf")

# Test 6: rt-ae-aesoc1.rtf
result6 <- test_file("inst/extdata/rtf_inputs/rt-ae-aesoc1.rtf")

# Test 7: rt-ds-pretrt.rtf
result7 <- test_file("inst/extdata/rtf_inputs/rt-ds-pretrt.rtf")

# Test 8: rt-ef-cfbdas.rtf
result8 <- test_file("inst/extdata/rtf_inputs/rt-ef-cfbdas.rtf")

# ============================================================================
# Quick inspection commands
# ============================================================================

# View specific result data
# View(result1$data)  # Uncomment to view in RStudio

# Check for curly braces
# any(grepl("\\{", result1$data$variable))
# any(grepl("\\{", result1$data$group1))

# Check variable levels
# table(result1$data$variable == result1$data$variable_level)

# Show hierarchical relationships
# hierarchical <- result1$data[result1$data$variable != result1$data$variable_level, ]
# View(hierarchical[c("variable", "variable_level", "group1", "raw_stat")])

# Show specific examples
# result1$data[result1$data$variable == "DISEASE", c("variable", "variable_level", "raw_stat")]
# result1$data[grepl("PRIMARY REASON", result1$data$variable), c("variable", "variable_level", "raw_stat")]

# Export to CSV for inspection
# write.csv(result1$data, "rt-dm-sum_state_parser.csv", row.names = FALSE)

# ============================================================================
# Toggle hierarchy on/off
# ============================================================================

# Test with hierarchy disabled
# USE_HIERARCHY <- FALSE
# result1_flat <- test_file("inst/extdata/rt-dm-sum.rtf")

# Test with hierarchy enabled  
# USE_HIERARCHY <- TRUE
# result1_hier <- test_file("inst/extdata/rt-dm-sum.rtf")