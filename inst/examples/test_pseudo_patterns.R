# Test script for pseudo-pattern implementation

# Load the package
pkgload::load_all()

# Source the pseudo-pattern file
source("R/pseudo_pattern.R")

# Test 1: Basic pseudo-pattern conversion
cat("=== Test 1: Basic Pseudo-Pattern Conversion ===\n")
test_templates <- list(
  "count_pct" = "{n} ({pct}%)",
  "count_pct_tight" = "{n}({pct}%)",
  "mean_sd" = "{mean} ({sd})",
  "range" = "{min} - {max}",
  "median_iqr" = "{median} ({q1}, {q3})"
)

for (name in names(test_templates)) {
  template <- test_templates[[name]]
  result <- pseudo_to_regex(template)
  cat("\nTemplate:", template, "\n")
  cat("Regex:", result$regex, "\n")
  cat("Stats:", paste(result$stats, collapse=", "), "\n")
  cat("Labels:", paste(result$labels, collapse=", "), "\n")
}

# Test 2: Pattern matching
cat("\n\n=== Test 2: Pattern Matching ===\n")
test_values <- c(
  "137 (39.0%)",
  "5 (1.2%)",
  "42.5 (12.3)",
  "10 - 85",
  "55.0 (45.0, 65.0)"
)

# Update stat patterns to use pseudo-patterns
update_stat_patterns_to_pseudo()

# Test each value
for (val in test_values) {
  result <- parse_stat_value_pseudo(val)
  cat("\nValue: '", val, "'\n", sep="")
  if (nrow(result) > 0) {
    for (i in 1:nrow(result)) {
      cat("  - stat:", result$stat[i], 
          " | stat_name:", result$stat_name[i],
          " | stat_label:", result$stat_label[i], "\n")
    }
  }
}

# Test 3: BIGN patterns
cat("\n\n=== Test 3: BIGN Pattern Conversion ===\n")
update_bign_patterns_to_pseudo()
bign_patterns <- get_bign_patterns()
cat("Generated", length(bign_patterns), "BIGN patterns:\n")
for (i in 1:min(5, length(bign_patterns))) {
  cat(" ", i, ":", bign_patterns[i], "\n")
}

# Test BIGN matching
test_bign_values <- c(
  "Nivo (Arm A)(N = 351)",
  "Placebo (N=175)",
  "Drug A N: 100",
  "Control [N=50]"
)

cat("\nTesting BIGN extraction:\n")
for (val in test_bign_values) {
  # Try each pattern
  matched <- FALSE
  for (pattern in bign_patterns) {
    if (grepl(pattern, val)) {
      n_value <- stringr::str_extract(val, pattern) |>
        stringr::str_extract("\\d+")
      cat("Value: '", val, "' -> N =", n_value, "\n")
      matched <- TRUE
      break
    }
  }
  if (!matched) {
    cat("Value: '", val, "' -> No match\n")
  }
}

cat("\n\nTests completed successfully!\n")