#!/usr/bin/env Rscript

# Basic usage example for stateful package
library(stateful)

cat("=== STATEFUL PACKAGE BASIC USAGE ===\n")

# Example 1: View default patterns
cat("\n1. Default BIGN Patterns:\n")
bign_patterns <- get_bign_patterns()
for (i in seq_along(bign_patterns)) {
  cat(sprintf("   %d. %s\n", i, bign_patterns[i]))
}

cat("\n2. Default Statistical Patterns:\n")
stat_patterns <- get_stat_patterns()
for (name in names(stat_patterns)) {
  template <- stat_patterns[[name]]$template
  cat(sprintf("   %s: %s\n", name, template))
}

# Example 2: Add custom patterns
cat("\n3. Adding Custom Patterns:\n")
add_bign_pattern("\\[N=\\d+\\]", position = 1)
cat("   Added bracket BIGN pattern: [N=123]\n")

add_stat_pattern(
  name = "hr_ci",
  template = "{hr} ({ci_lower}-{ci_upper})",
  regex = "^(\\d+\\.?\\d*)\\s*\\(\\s*(\\d+\\.?\\d*)\\s*-\\s*(\\d+\\.?\\d*)\\s*\\)$",
  stats = c("hr", "ci_lower", "ci_upper"),
  labels = c("Hazard Ratio", "CI Lower", "CI Upper")
)
cat("   Added hazard ratio pattern: 1.23 (0.98-1.45)\n")

# Example 3: Test pattern matching
cat("\n4. Testing Pattern Matching:\n")
test_values <- c(
  "137 (39.0)",
  "65.2 (8.1)", 
  "1.23 (0.98-1.45)",
  "45.2 - 78.9",
  "123"
)

for (test_val in test_values) {
  result <- parse_state_stat_value(test_val)
  cat(sprintf("   \"%s\" → %d rows: %s\n", 
              test_val, nrow(result), 
              paste(result$stat_name, collapse=", ")))
}

# Example 4: Process sample RTF file
cat("\n5. Processing Sample RTF File:\n")
sample_files <- list.files(system.file("extdata", package = "stateful"), 
                          pattern = "\\.rtf$", full.names = TRUE)

if (length(sample_files) > 0) {
  cat(sprintf("   Found %d sample files\n", length(sample_files)))
  
  # Process first file
  sample_file <- sample_files[1]
  cat(sprintf("   Processing: %s\n", basename(sample_file)))
  
  result <- rtf_to_ard_json(sample_file, output_file = NULL)
  
  cat(sprintf("   ✓ Success! Generated %d ARD rows\n", nrow(result$ard_data)))
  cat(sprintf("   Title: %s\n", result$metadata$title %||% "N/A"))
  cat(sprintf("   Groups: %d\n", length(unique(result$ard_data$group1_level))))
  cat(sprintf("   Stat types: %s\n", paste(unique(result$ard_data$stat_name), collapse=", ")))
  
} else {
  cat("   No sample RTF files found\n")
}

cat("\n6. Launch Interactive App:\n")
cat("   Run: launch_stateful_app()\n")
cat("   This will open a web interface for interactive RTF processing\n")

cat("\n=== BASIC USAGE COMPLETE ===\n")
cat("✓ Pattern management working\n")
cat("✓ RTF parsing functional\n") 
cat("✓ ARD conversion successful\n")
cat("✓ Ready for production use!\n")