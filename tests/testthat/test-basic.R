test_that("pattern functions work", {
  # Test BIGN patterns
  patterns <- get_bign_patterns()
  expect_true(length(patterns) > 0)
  expect_true(is.character(patterns))
  
  # Test statistical patterns
  stat_patterns <- get_stat_patterns()
  expect_true(length(stat_patterns) > 0)
  expect_true(is.list(stat_patterns))
  
  # Test adding patterns
  initial_count <- length(get_bign_patterns())
  add_bign_pattern("test_pattern_\\d+")
  new_count <- length(get_bign_patterns())
  expect_equal(new_count, initial_count + 1)
})

test_that("RTF parsing works with sample data", {
  # Test with sample RTF file if available
  sample_files <- list.files(system.file("extdata", package = "stateful"), 
                            pattern = "\\.rtf$", full.names = TRUE)
  
  if (length(sample_files) > 0) {
    result <- rtf_to_ard_json(sample_files[1], output_file = NULL)
    
    expect_true(is.list(result))
    expect_true("ard_data" %in% names(result))
    expect_true("metadata" %in% names(result))
    expect_true(is.data.frame(result$ard_data))
    
    # Check ARD compliance
    if (nrow(result$ard_data) > 0) {
      expect_true("stat" %in% names(result$ard_data))
      expect_true("stat_name" %in% names(result$ard_data))
      expect_true("stat_label" %in% names(result$ard_data))
    }
  } else {
    skip("No sample RTF files available")
  }
})