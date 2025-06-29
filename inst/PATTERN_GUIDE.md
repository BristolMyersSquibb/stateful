# Stateful Pattern Guide

## Current Statistical Patterns

The package uses a pseudo-pattern system where you define templates with placeholders in curly braces like `{n} ({pct}%)`.

### ✅ **NEW: Minimal Hardcoding**
- **BIGN Patterns**: Single maximal pattern `N={n}` replaces 8+ hardcoded regex patterns
- **Statistical Patterns**: Template-based system `{mean} ({sd})` instead of complex regex
- **Context-Aware**: Uses variable labels to intelligently select patterns

### Currently Supported Patterns

```r
# View all current patterns
library(stateful)
update_stat_patterns_to_pseudo()  # Initialize patterns
patterns <- get_stat_patterns()
names(patterns)
```

| Pattern Name | Template | Example Input | Parsed Output |
|-------------|----------|---------------|---------------|
| n_pct | `{n} ({pct}%)` | "149 (42.5%)" | n=149, pct=42.5 |
| n_pct_tight | `{n}({pct}%)` | "149(42.5%)" | n=149, pct=42.5 |
| mean_sd | `{mean} ({sd})` | "12.5 (3.2)" | mean=12.5, sd=3.2 |
| median_range | `{median} ({min}, {max})` | "45 (21, 86)" | median=45, min=21, max=86 |
| median_iqr | `{median} ({q1}, {q3})` | "52 (43, 61)" | median=52, q1=43, q3=61 |
| range_comma | `{min}, {max}` | "43.0, 60.0" | min=43.0, max=60.0 |
| ci | `{estimate} ({ci_lower}, {ci_upper})` | "1.5 (0.8, 2.2)" | estimate=1.5, ci_lower=0.8, ci_upper=2.2 |
| count_only | `{n}` | "351" | n=351 |
| pct_only | `{pct}%` | "42.5%" | pct=42.5 |

### Adding New Patterns

```r
# Method 1: Add a single pattern
add_pseudo_pattern(
  name = "my_pattern",
  template = "{value} [{units}]",
  labels = c("Value", "Units")  # Optional custom labels
)

# Method 2: Add pattern directly to registry
add_stat_pattern(
  name = "ci_spaces",
  template = "{estimate}  ( {ci_lower}, {ci_upper})",  # Note: handles extra spaces
  regex = "generated_automatically",  # Will be generated
  stats = c("estimate", "ci_lower", "ci_upper"),
  labels = c("Estimate", "CI Lower", "CI Upper")
)
```

### Removing Patterns

```r
# Get current patterns
patterns <- get_stat_patterns()

# Remove a pattern
patterns$my_pattern <- NULL

# Update the registry
set_stat_patterns(patterns)
```

### Pattern Matching Order

Patterns are matched in the order they appear in the list. The first matching pattern wins, so order matters:

```r
# Re-order patterns (put more specific patterns first)
patterns <- get_stat_patterns()
patterns <- patterns[c("n_pct_tight", "n_pct", names(patterns)[!names(patterns) %in% c("n_pct_tight", "n_pct")])]
set_stat_patterns(patterns)
```

### Testing Patterns

```r
# Test a single value
parse_stat_value_pseudo("43.0, 60.0")
# Returns: data.frame with stat_name = c("min", "max"), stat = c("43.0", "60.0")

# Test multiple values
test_values <- c("149 (42.5%)", "43.0, 60.0", "-0.6 (-2.8, 1.6)")
lapply(test_values, parse_stat_value_pseudo)
```

### Pattern Tips

1. **Greedy Matching**: The parser uses non-greedy matching, so `{n}` will capture the minimal valid string
2. **Whitespace**: The parser automatically handles flexible whitespace between elements
3. **Special Characters**: Parentheses, brackets, and other special characters in the template are automatically escaped
4. **Percent Signs**: The `%` character is made optional in the regex, so "42.5%" and "42.5" both work

### Common Issues

1. **Extra Spaces**: For patterns with inconsistent spacing, create variations:
   ```r
   add_pseudo_pattern("ci_extra_spaces", "{estimate}  ( {ci_lower}, {ci_upper})")
   ```

2. **Missing Values**: Handle "N.A." or similar by pre-processing:
   ```r
   # In your processing code
   if (stat_value == "N.A.") {
     return(data.frame(stat = NA, stat_name = "missing", stat_label = "Missing"))
   }
   ```

3. **Priority**: If a value matches multiple patterns, only the first match is used. Order patterns from most specific to least specific.