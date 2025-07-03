# Stateful Package Architecture and Design Documentation

## Overview

Stateful is a modern state-based RTF table parser for converting clinical trial data tables from RTF format into Analysis Results Data (ARD) format. The package features global pattern registries, position-aware parsing, and template-based statistical extraction.

## Core Architecture

### 1. State-Based RTF Parser (`R/rtf_parser.R`)

The parser identifies document sections using RTF structural markers:
- **Pre-header**: Table titles and population information
- **Header**: Column headers (supports multi-level)
- **Table**: Data rows
- **Footnotes**: Additional metadata

Key features:
- Uses RTF markers (`\trowd`, `\trhdr`, borders) not content patterns
- Robust across different RTF formats
- No HTML conversion or external dependencies

### 2. Pattern Management System

#### Pseudo-Pattern System (`R/pseudo_pattern.R`)
- Converts templates like `{n} ({pct}%)` to regex
- Single BIGN pattern replaces 8+ regex patterns
- Context-aware matching based on variable names

#### Global Pattern Registry (`.stateful_patterns`)
- BIGN patterns for sample sizes
- Statistical patterns for combined statistics
- Title, population, and footnote patterns
- All patterns configurable and extensible

### 3. Hierarchical Data Handling

Detects and processes hierarchical structures:
- **Rule 1**: Empty parent + indented children = concatenated labels
- **Rule 2**: Value parent + indented children = nested groups
- Uses RTF indentation markers (`\li`, `\fi`)

## Data Flow Pipeline

```
RTF File → DataFrame → ARD (unparsed) → ARD (parsed) → JSON
```

### Stage 1: RTF → DataFrame (`parse_rtf_to_dataframe`)
- Extracts table structure from RTF
- State-based section identification
- Preserves all formatting information

### Stage 2: DataFrame → ARD (unparsed) (`dataframe_to_ard_unparsed`)
- Converts wide to long format
- Preserves combined statistics (e.g., "137 (39.0%)")
- Extracts BIGN from headers

### Stage 3: ARD (unparsed) → ARD (parsed) (`parse_ard_statistics`)
- Splits combined statistics into rows
- Uses pseudo-pattern matching
- Context-aware parsing

### Stage 4: ARD (parsed) → JSON (`ard_to_json`)
- Adds metadata
- Includes summary statistics
- Structured JSON output

## ARD Data Structure

Standard columns:
- `group1`, `group1_level`: Primary grouping (treatment)
- `group2`, `group2_level`: Secondary grouping
- `variable`: Measured parameter
- `variable_level`: Hierarchical path
- `stat`: Statistic value
- `stat_name`: Type (n, pct, mean, etc.)
- `stat_label`: Human-readable label

## Key Components

### Main Functions (`R/main.R`)
- `rtf_to_ard_json()`: Main entry point
- `rtf_to_ard()`: Returns data.frame
- Configurable via options parameter

### Shiny Application (`inst/shiny-app/`)
- `app.R`: Main application
- `R/modules/`: UI modules
  - `ard_viewer_module.R`: Data viewing
  - `pattern_module.R`: Pattern management
  - `upload_module.R`: File upload
- Interactive RTF parsing and viewing

### LLM Query System (`R/llm_query.R`)
- Azure OpenAI integration via ellmer
- Natural language to SQL conversion
- `query_ard_with_llm()`: Main interface
- `inspect_ard_structure()`: Schema inspection
- Executes queries with sqldf

### Utility Functions
- `R/utils.R`: Common utilities
- `R/ard_utils.R`: ARD-specific helpers
- `R/json_io.R`: JSON I/O operations

## Configuration System

### Pattern Configuration
```r
configure_table_patterns(
  title_patterns = custom_title_patterns,
  population_patterns = custom_pop_patterns
)
```

### Pseudo-Pattern Extension
```r
add_pseudo_pattern("custom", "{stat1} - {stat2}")
```

### BIGN Pattern Addition
```r
add_bign_pattern("N = {n} subjects", "n")
```

## File Organization

```
stateful/
├── R/                    # Core R functions
│   ├── rtf_parser.R      # RTF parsing engine
│   ├── pseudo_pattern.R  # Pattern system
│   ├── ard_utils.R       # ARD utilities
│   ├── llm_query.R       # LLM integration
│   └── main.R            # Main entry points
├── inst/
│   ├── shiny-app/        # Shiny application
│   └── extdata/          # Sample data files
├── tests/                # Test suite
└── man/                  # Documentation
```

## Design Principles

1. **Minimal Hardcoding**: Use configurable patterns
2. **State-Based Parsing**: Rely on RTF structure not content
3. **Extensibility**: Easy to add new patterns/formats
4. **Clean Pipeline**: Clear data transformation stages
5. **Standard Output**: Consistent ARD format

## Common Workflows

### Basic Usage
```r
rtf_to_ard_json("table.rtf", "output.json")
```

### With Configuration
```r
config <- list(
  group2_name = "TIMEPOINT",
  stat_patterns = custom_patterns
)
rtf_to_ard_json("table.rtf", "output.json", config)
```

### LLM Query
```r
ard_data <- load_ard_json("output.json")
result <- query_ard_with_llm(
  ard_data,
  "What is the mean age by treatment group?"
)
```

## Testing Strategy

- Unit tests in `tests/testthat/`
- Sample RTF files for regression testing
- Pattern matching validation
- ARD structure verification

## Known Limitations

1. RTF parsing depends on consistent structure
2. Pattern matching may need adjustment for novel formats
3. LLM queries require Azure OpenAI configuration

## Extension Points

1. Add new pseudo-patterns for statistics
2. Configure BIGN extraction patterns
3. Customize section detection patterns
4. Add new output formats beyond JSON
5. Extend LLM query capabilities

This architecture enables flexible, maintainable parsing of clinical trial RTF tables with minimal hardcoding and maximum extensibility.