# stateful

A modern state-based parser for RTF tables that converts clinical trial data into Analysis Results Data (ARD) format. Features global pattern registries, position-aware parsing, and template-based statistical extraction.

## Installation

```r
devtools::install_github(repo = sprintf("GBDS-SP/%s@%s", "stateful", "main"), 
                        host = "github.web.bms.com/api/v3")
```

## Quick Start

### Basic RTF to ARD Conversion

```r
library(stateful)

# Convert single RTF file to ARD JSON
rtf_to_ard_json("path/to/table.rtf")

# Convert all RTF files in a directory
convert_all_rtf_to_json()
```

### Pseudo-Pattern System

Define statistical patterns using simple templates instead of complex regex:

```r
# Add a new statistical pattern
add_pseudo_pattern("mean_ci", "{mean} ({ci_lower}, {ci_upper})")

# Parse "12.5 (10.2, 14.8)" -> 3 separate ARD rows
parse_stat_value_pseudo("12.5 (10.2, 14.8)")
```

### BIGN Pattern Parsing

Flexible extraction of sample sizes from treatment headers:

```r
# All these formats work automatically:
parse_bign_value_pseudo("Arm A: Nivo (N = 351)")     # N=351, Label="Arm A: Nivo"
parse_bign_value_pseudo("Treatment Group N=125")     # N=125, Label="Treatment Group" 
parse_bign_value_pseudo("Control (n: 87)")           # N=87, Label="Control"
```

### State-Based RTF Parsing

Parse RTF tables by identifying distinct states (header, table, footnotes):

```r
# Parse RTF using state detection
sections <- parse_rtf_table_states("safety_table.rtf")

# Convert to ARD format
ard_data <- state_table_to_ard(sections)

# Extract metadata
metadata <- extract_state_metadata(sections, "safety_table.rtf")
```

### LLM Query Interface

Generate SQL queries from natural language using Azure OpenAI and execute with sqldf:

```r
# Setup (requires environment variables)
Sys.setenv(
  AZURE_OPENAI_ENDPOINT = "your-endpoint",
  AZURE_OPENAI_API_KEY = "your-key",
  AZURE_OPENAI_DEPLOYMENT_ID = "your-deployment"
)

# Load ARD data from JSON file
ard_data <- load_ard_json("table_ard.json")

# Generate SQL from natural language (summary questions only)
result <- generate_sql_from_question(
  ard_data, 
  "Which treatment group has the highest adverse event percentage?"
)

# Execute the generated SQL using sqldf
execute_ard_sql(ard_data, result$sql)

# Valid questions (summary data only):
# - "How many subjects in each treatment group?"
# - "Compare death rates between treatments"
# - "What is the mean age by treatment?"

# Invalid questions (patient-level data):
# - "Which patients had adverse events?" (NOT SUPPORTED)
# - "Show me patient demographics" (NOT SUPPORTED)
```

### Shiny Application

Launch interactive web interface for RTF processing:

```r
launch_stateful_app()
```

## Key Features

### Pseudo-Pattern System
- **Simple Templates**: Use `{n} ({pct}%)` instead of complex regex
- **Automatic Parsing**: Converts combined statistics into separate ARD rows
- **Extensible**: Add custom patterns for new statistical formats

### Flexible BIGN Parsing
- **Greedy Matching**: Single pattern handles all BIGN format variations
- **Robust**: Works with different whitespace, punctuation, and indicators
- **Clean Labels**: Automatically extracts treatment names from headers

### State-Based RTF Processing
- **Structure Detection**: Identifies table sections using RTF markers
- **Position-Aware**: Uses cell positions for accurate column mapping
- **Hierarchical Support**: Handles nested headers and indented variables

### ARD Compliance
- **Standard Format**: Follows CDISC ARD specifications
- **Numeric Values**: Ensures all `stat` fields contain only numbers
- **Meaningful Labels**: Uses proper statistical classifications
- **Metadata Rich**: Includes source file, conversion method, timestamps

## Example Output

Input RTF table converts to structured ARD JSON:

```json
{
  "metadata": {
    "source_file": "rt-dm-sum.rtf",
    "title": "Table 14.3.2.1.1.1",
    "population": "All Treated Subjects",
    "conversion_date": "2025-06-28 18:21:02"
  },
  "ard_data": [
    {
      "group1": "TRT",
      "group1_level": "Arm A: Nivo",
      "variable_level": "NUMBER OF SUBJECTS WHO DIED (%)",
      "stat_name": "n",
      "stat_label": "n", 
      "stat": "149"
    },
    {
      "group1": "TRT",
      "group1_level": "Arm A: Nivo",
      "variable_level": "NUMBER OF SUBJECTS WHO DIED (%)", 
      "stat_name": "pct",
      "stat_label": "pct",
      "stat": "42.5"
    }
  ]
}
```

## Configuration

### Global Pattern Management

```r
# View current patterns
show_pattern_config()

# Add custom patterns
add_title_pattern("Study [0-9]+ Results")
add_population_pattern("Modified ITT Population")

# Configure table-specific patterns
configure_table_patterns(
  table_type = "safety",
  title_patterns = c("Safety Table", "Adverse Events"),
  population_patterns = c("Safety Population")
)
```

### Statistical Patterns

```r
# Built-in patterns handle common formats
patterns <- c(
  "137 (39.0%)",      # -> n=137, pct=39.0
  "12.5 (2.1)",       # -> mean=12.5, sd=2.1  
  "8.2 (6.1, 10.3)",  # -> median=8.2, q1=6.1, q3=10.3
  "1.23 (0.98-1.45)"  # -> hr=1.23, ci_lower=0.98, ci_upper=1.45
)

# Parse any format
lapply(patterns, parse_stat_value_pseudo)
```

## Requirements

- R >= 4.0.0
- Pandoc >= 2.14.2 (for RTF input support)
- Required packages: stringr, dplyr, tidyr, purrr, jsonlite, tools
- Optional: shiny, ellmer (for LLM features)

## License

MIT License