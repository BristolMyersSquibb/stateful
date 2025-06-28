# stateful: Direct RTF to ARD Converter with AI-Powered Query

Convert RTF clinical tables directly to ARD (Analysis Results Data) format and query them using natural language powered by Azure OpenAI.

## Quick Start

```r
library(stateful)

# Convert RTF table to ARD JSON
rtf_to_ard_pipeline("clinical_table.rtf")

# Launch interactive Shiny app with AI query features
launch_stateful_app()
```

## Key Features

- 📄 **Direct RTF parsing** - No HTML conversion needed
- 🤖 **AI-powered SQL generation** - Ask questions in plain English
- 📊 **ARD-compliant output** - CDISC standard format
- 🎯 **Pattern-based parsing** - Configurable extraction patterns
- 🔍 **Full traceability** - All generated SQL is visible
- 🚀 **Interactive Shiny app** - User-friendly interface

## Processing Pipeline

The package converts RTF tables through 4 clear stages:

```
RTF File → DataFrame → ARD (unparsed) → ARD (parsed) → JSON
```

### Stage 1: RTF → DataFrame
Extract table structure directly from RTF tags (no HTML conversion needed).

### Stage 2: DataFrame → ARD (unparsed) 
Convert to ARD format with combined statistics like `"137 (39.0%)"`.

### Stage 3: ARD (unparsed) → ARD (parsed)
Parse combined statistics into separate rows:
- `"137 (39.0%)"` → Row 1: `stat="137", stat_name="n"` 
- `"137 (39.0%)"` → Row 2: `stat="39.0", stat_name="pct"`

### Stage 4: ARD (parsed) → JSON
Generate final JSON output for submission or web applications.

**Why extract ARD before JSON?** The intermediate ARD dataset enables double programming - you can validate against the parsed data before JSON conversion.

## RTF Structure Detection

The parser automatically identifies these RTF sections:

### 📄 Pre-header (Titles & Population)
- Table numbers: `Table 14.3.2.1.1.1`
- Population: `All Treated Subjects`

### 📊 Header (Column Structure)  
- Treatment groups: `Arm A: Nivo (N = 351)`
- Column headers: `Any Grade | Grade 3-4`

### 📋 Table (Data Rows)
- Variable names: `All-Causality AEs`
- Statistics: `336 (95.7%)`, `151 (43.0%)`

### 📝 Footnotes
- Study notes: `MedDRA Version 27.0`
- Sources: `Program Source: path/program`

## Pattern Configuration

### View Current Patterns
```r
show_pattern_config()
```

### Add New Table Type
```r
configure_table_patterns(
  table_type = "efficacy",
  title_patterns = c("Overall Survival", "Progression Free"),
  population_patterns = c("ITT Population", "Efficacy Set"),
  footnote_patterns = c("HR =", "Hazard Ratio", "95% CI"),
  header_patterns = c("Median.*95% CI", "Events.*Censored")
)
```

### Pattern Precedence (First Match Wins)
Patterns are tried in the order they're added:
```r
# High priority pattern (added first)
add_title_pattern("Table \\d+\\.\\d+\\.\\d+")

# Lower priority pattern (added second)  
add_title_pattern("Table \\d+")

# "Table 14.3.2.1" matches the first pattern, not the second
```

## Statistical Pattern Examples

### Default Patterns (Priority Order)
1. **n_pct**: `137 (39.0%)` → `n=137, pct=39.0`
2. **mean_sd**: `65.2 (8.1)` → `mean=65.2, sd=8.1`
3. **median_iqr**: `67.1 (45.2, 89.3)` → `median=67.1, q1=45.2, q3=89.3`
4. **min_max**: `45.2 - 78.9` → `min=45.2, max=78.9`

### Add Custom Pattern
```r
add_stat_pattern(
  name = "events_total",
  template = "{events}/{total} ({rate}%)",
  regex = "^(\\d+)/(\\d+)\\s*\\(\\s*(\\d+\\.?\\d*)%\\s*\\)$",
  stats = c("events", "total", "rate"),
  labels = c("Events", "Total", "Rate")
)

# Now "45/120 (37.5%)" → events=45, total=120, rate=37.5
```

## AI-Powered Query Feature

Query your parsed data using natural language:

```r
# In the Shiny app, just type questions like:
"What is the mean age by treatment group?"
"Show me all adverse events with >10% incidence"
"Which treatment has the highest serious AE rate?"

# The system generates SQL like:
SELECT group1_level, CAST(stat AS NUMERIC) as mean_age
FROM ard_data
WHERE variable = 'AGE' AND stat_name = 'mean'
ORDER BY group1_level;
```

### Why SQL Generation?
- **Full Traceability**: Every query is visible and reviewable
- **Reproducibility**: Save and re-run queries
- **Validation**: Domain experts can verify logic
- **No Black Box**: Complete transparency in data analysis

See [LLM_QUERY_GUIDE.md](LLM_QUERY_GUIDE.md) for setup and details.

## Example Output

**Input RTF table:**
```
                     Arm A: Nivo    Arm B: Nivo+Ipi
                     (N = 351)      (N = 352)
All-Causality AEs    336 (95.7%)    349 (99.1%)
  Grade 3-4          151 (43.0%)    168 (47.7%)
```

**Output ARD JSON:**
```json
{
  "metadata": {
    "source_file": "safety_table.rtf",
    "title": "Table 8.1-1", 
    "population": "All Treated Subjects"
  },
  "ard_data": [
    {
      "group1": "TRT",
      "group1_level": "Arm A: Nivo",
      "variable": "BIGN", 
      "stat": "351",
      "stat_name": "n"
    },
    {
      "group1": "TRT",
      "group1_level": "Arm A: Nivo", 
      "variable": "All-Causality AEs",
      "stat": "336",
      "stat_name": "n"
    },
    {
      "group1": "TRT",
      "group1_level": "Arm A: Nivo",
      "variable": "All-Causality AEs", 
      "stat": "95.7",
      "stat_name": "pct"
    }
  ]
}
```

## Installation

```r
# Development version
devtools::install_github("username/stateful")

# Local installation
devtools::install_local("path/to/stateful")
```

## API Functions

### Main Pipeline
- `rtf_to_ard_pipeline()` - Complete RTF → JSON conversion
- `parse_rtf_to_dataframe()` - Stage 1: RTF → DataFrame
- `dataframe_to_ard_unparsed()` - Stage 2: DataFrame → ARD (unparsed)
- `parse_ard_statistics()` - Stage 3: ARD (unparsed) → ARD (parsed)

### Pattern Management
- `configure_table_patterns()` - Add patterns for new table types
- `add_title_pattern()`, `add_population_pattern()`, `add_footnote_pattern()`
- `get_title_patterns()`, `get_population_patterns()`, `get_footnote_patterns()`
- `show_pattern_config()` - Display all current patterns

### Legacy Function
- `rtf_to_ard_json()` - Original function (still works, uses old pipeline)

## Use Cases

- **Double Programming**: Extract ARD dataset to validate against independent implementation
- **Regulatory Submission**: Generate JSON ARD files for FDA/EMA submissions  
- **Web Applications**: JSON format ready for clinical data dashboards
- **Data Validation**: Compare parsed results against source RTF tables
- **Cross-Study Analysis**: Standardize tables from different studies into common ARD format

## License

MIT License