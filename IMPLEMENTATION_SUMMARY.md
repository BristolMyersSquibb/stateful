# Implementation Summary: Pseudo-Patterns & LLM Query Features

## Overview

Two major features have been successfully implemented in the Stateful package:

1. **Pseudo-Pattern System**: Simplified pattern definition using templates like `{n} ({pct}%)`
2. **LLM Query Feature**: AI-powered SQL generation using Azure OpenAI

## 1. Pseudo-Pattern System ✅

### Problem Solved
- **Before**: Users had to write complex regex patterns like `^(\\d+)\\s*\\(\\s*(\\d+\\.?\\d*)%?\\s*\\)\\s*;?\\s*$`
- **After**: Users can write simple templates like `{n} ({pct}%)`

### Implementation Details

#### Core Functions (`R/pseudo_pattern.R`)
- `pseudo_to_regex()`: Converts templates to regex patterns
- `pseudo_to_regex_simple()`: Simplified version using numbered capture groups
- `update_stat_patterns_to_pseudo()`: Updates global patterns to use pseudo-patterns
- `parse_stat_value_pseudo()`: Parses statistics using the new system
- `add_pseudo_pattern()`: User-friendly function to add new patterns

#### Key Features
- **17 built-in patterns** covering common clinical statistics
- **Automatic boundary detection** using whitespace and punctuation
- **Clean value extraction** (removes % from percentages automatically)
- **Backward compatibility** with existing code
- **Priority-based matching** (greedy - first match wins)

#### Pattern Examples
```r
# User writes this:
"{n} ({pct}%)"

# System generates this:
"^\\s*([^\\s()]+)\\s*\\(([^\\s()]+)%?\\)\\s*$"

# Parses this: "137 (39.0%)"
# Into: stat="137", stat_name="n" AND stat="39.0", stat_name="pct"
```

### Updated Components
- **Shiny App UI**: Removed complex regex inputs, added template dropdown
- **Core Parser**: All parsing functions now use pseudo-pattern system
- **Pattern Management**: Simplified API for pattern addition

## 2. LLM Query Feature ✅

### Problem Solved
- **Before**: Users needed SQL knowledge to query ARD data
- **After**: Users can ask questions in plain English and get executable SQL

### Implementation Details

#### Core Functions (`R/llm_query.R`)
- `generate_sql_from_question()`: Main function using Azure OpenAI
- `generate_table_schema()`: Creates schema description for LLM
- `execute_ard_sql()`: Executes generated SQL on ARD data
- `parse_sql_response()`: Extracts SQL from LLM response
- `get_example_queries()`: Provides common query examples

#### Azure OpenAI Integration
```r
# Configuration (in .Renviron)
AZURE_OPENAI_ENDPOINT=https://bms-openai-services-eastus2-1-nonprod.azu.bms.com
AZURE_OPENAI_DEPLOYMENT_ID=o3
AZURE_OPENAI_API_VERSION=2025-04-01-preview
AZURE_OPENAI_API_KEY=your-key-here

# Usage
result <- generate_sql_from_question(
  question = "What is the mean age by treatment group?",
  table_schema = generate_table_schema(ard_data)
)
```

#### Query Examples Supported
- "What are the total number of patients in each treatment group?"
- "Compare adverse event rates between treatments"
- "Show me all demographics by treatment group"
- "Which treatment has the highest serious AE rate?"

### Traceability Features
- **Generated SQL is always visible** to users
- **Explanations** of what each query does
- **Assumptions** made by the LLM are stated
- **Query results** can be downloaded as CSV
- **SQL can be copied** for manual modification

### Shiny App Integration
- **Enhanced LLM Query Tab** with professional UI
- **Real-time SQL generation** with progress indicators
- **Query result display** with download options
- **Template suggestions** for common questions
- **Debug information** for troubleshooting

## Technical Architecture

### File Structure
```
R/
├── pseudo_pattern.R          # Pseudo-pattern system
├── llm_query.R              # LLM query functionality
├── rtf_parser.R             # Updated to use pseudo-patterns
├── rtf_pipeline.R           # Updated parser pipeline
├── parse.R                  # Updated statistical parsing
└── stateful.R               # Package initialization

inst/
├── shiny-app/app.R          # Updated Shiny application
└── examples/
    ├── llm_query_example.R  # LLM usage examples
    └── ...

docs/
├── LLM_QUERY_GUIDE.md       # Comprehensive LLM guide
└── IMPLEMENTATION_SUMMARY.md # This file
```

### Dependencies Added
```r
# DESCRIPTION file updates
Suggests: 
    ellmer,        # Azure OpenAI integration
    RSQLite,       # SQL execution
    ...
```

### Environment Variables Required
```bash
# .Renviron file
AZURE_OPENAI_ENDPOINT=https://your-endpoint.openai.azure.com
AZURE_OPENAI_DEPLOYMENT_ID=your-deployment-name
AZURE_OPENAI_API_VERSION=2025-04-01-preview
AZURE_OPENAI_API_KEY=your-api-key
```

## User Experience Improvements

### Pattern Management (Before vs After)

**Before:**
```r
# User had to write complex regex
add_stat_pattern(
  name = "hr_ci",
  regex = "^(\\\\d+\\\\.?\\\\d*)\\\\s*\\\\(\\\\s*(\\\\d+\\\\.?\\\\d*)\\\\s*-\\\\s*(\\\\d+\\\\.?\\\\d*)\\\\s*\\\\)$",
  stats = c("hr", "ci_lower", "ci_upper"),
  labels = c("Hazard Ratio", "CI Lower", "CI Upper")
)
```

**After:**
```r
# User writes simple template
add_pseudo_pattern("hr_ci", "{hr} ({ci_lower}-{ci_upper})")
```

### Data Query (Before vs After)

**Before:**
```r
# User needed SQL knowledge
library(RSQLite)
con <- dbConnect(SQLite(), ":memory:")
dbWriteTable(con, "ard_data", data)
result <- dbGetQuery(con, "
  SELECT group1_level, CAST(stat AS NUMERIC) as mean_age
  FROM ard_data 
  WHERE variable = 'AGE' AND stat_name = 'mean'
")
```

**After:**
```r
# User asks in plain English (in Shiny app)
"What is the mean age by treatment group?"

# System generates SQL automatically and executes it
```

## Testing & Validation

### Test Coverage
- ✅ Pseudo-pattern conversion with 17+ patterns
- ✅ ARD parsing with sample clinical data
- ✅ SQL generation components (without API calls)
- ✅ Schema generation for multiple table types
- ✅ Pattern precedence and matching logic

### Example Test Results
```r
# Pattern conversion test
pseudo_to_regex("{n} ({pct}%)")
# Output: "^\\s*([^\\s()]+)\\s*\\(([^\\s()]+)%?\\)\\s*$"

# Parsing test
parse_stat_value_pseudo("137 (39.0%)")
# Output: 
#   stat="137", stat_name="n", stat_label="n"
#   stat="39.0", stat_name="pct", stat_label="pct"
```

## Security & Compliance

### Data Security
- **No ARD data sent to Azure**: Only schema metadata is shared
- **API keys in .Renviron**: Never exposed in code
- **Read-only queries**: Generated SQL is SELECT-only
- **In-memory execution**: No persistent database storage

### Traceability
- **All queries logged**: Complete audit trail
- **Manual review possible**: Users can modify generated SQL
- **Version control friendly**: SQL queries can be saved and tracked

## Future Enhancements

### Potential Improvements
1. **Query optimization**: Cache common patterns
2. **Multi-table joins**: Enhanced cross-table analysis
3. **Query history**: Save and reuse previous queries
4. **Advanced patterns**: Support for more complex statistical formats
5. **Export options**: Generate R/SAS code in addition to SQL

## Conclusion

Both features significantly improve the user experience:

1. **Pseudo-patterns** make the system accessible to non-regex experts
2. **LLM queries** enable natural language data exploration
3. **Full traceability** maintains scientific rigor and reproducibility
4. **Enterprise integration** leverages existing Azure OpenAI infrastructure

The implementation maintains backward compatibility while adding powerful new capabilities that make clinical data analysis more accessible and transparent.