# LLM Query Feature Guide

## Overview

The Stateful package now includes an AI-powered SQL query generation feature that uses Azure OpenAI to convert natural language questions into executable SQL queries. This provides full traceability while making data analysis accessible to users without SQL expertise.

## Key Features

- **Natural Language Input**: Ask questions in plain English
- **SQL Generation**: Automatically generates optimized SQL queries
- **Full Traceability**: All generated SQL is visible and can be reviewed
- **Azure OpenAI Integration**: Uses enterprise Azure OpenAI services
- **ARD-Aware**: Understands the CDISC ARD data structure

## Setup

### 1. Environment Variables

The following environment variables must be set in your `.Renviron` file:

```
AZURE_OPENAI_ENDPOINT=https://your-endpoint.openai.azure.com
AZURE_OPENAI_DEPLOYMENT_ID=your-deployment-name
AZURE_OPENAI_API_VERSION=2023-12-01-preview
AZURE_OPENAI_API_KEY=your-api-key
```

**Implementation Note**: This uses `ellmer::chat_azure_openai()` which creates a chat instance and returns raw text responses for maximum traceability.

### 2. Required Packages

```r
# Install required packages
install.packages(c("RSQLite", "DBI"))
remotes::install_github("ellmer/ellmer")  # If not already installed
```

## Using the LLM Query Feature

### In the Shiny App

1. **Upload and Parse RTF Files**: First load your RTF files in the "Upload & Parse" tab

2. **Navigate to LLM Query Tab**: Click on the "LLM Query" menu item

3. **Ask Your Question**: Enter a natural language question like:
   - "What are the total number of patients in each treatment group?"
   - "Compare adverse event rates between treatments"
   - "Show me demographic variables by treatment group"

4. **Review Generated SQL**: The system will display:
   - The generated SQL query
   - An explanation of what the query does
   - Any assumptions made
   - Query execution results

5. **Download Results**: Export query results as CSV if needed

### Programmatically

```r
library(stateful)

# Generate SQL from a question
result <- generate_sql_from_question(
  question = "What is the mean age by treatment group?",
  table_schema = generate_table_schema(your_ard_data),
  examples = get_example_queries()
)

# Execute the generated SQL
if (result$success) {
  exec_result <- execute_ard_sql(result$sql, your_ard_data)
  if (exec_result$success) {
    print(exec_result$data)
  }
}
```

## Example Questions

Here are some example questions the system can handle:

### Demographics
- "What is the mean age in each treatment group?"
- "Show me the gender distribution across all treatments"
- "Which group has the oldest patients on average?"

### Safety/Adverse Events
- "How many patients experienced serious adverse events?"
- "What are the most common adverse events in the drug group?"
- "Compare mild vs severe AE rates between treatments"

### General Statistics
- "Count the total number of patients per group"
- "Show all variables that have data for the placebo group"
- "List all demographic characteristics by treatment"

## SQL Generation Details

### ARD Structure Understanding

The LLM understands the standard ARD columns:
- `group1`, `group1_level`: Primary grouping (usually treatment)
- `group2`, `group2_level`: Secondary grouping if present
- `variable`: What is being measured
- `variable_level`: Specific level of the variable
- `stat_name`: Type of statistic (n, mean, sd, pct, etc.)
- `stat`: The numeric value (stored as text)

### Type Casting

The system automatically handles type casting:
```sql
-- Numeric values
CAST(stat AS NUMERIC) for calculations

-- Integer counts
CAST(stat AS INTEGER) for whole numbers
```

### Common Patterns

The generated SQL follows these patterns:

```sql
-- Getting counts by group
SELECT group1_level, CAST(stat AS INTEGER) as count
FROM table_name
WHERE variable = 'BIGN' AND stat_name = 'N'

-- Getting means with filtering
SELECT group1_level, CAST(stat AS NUMERIC) as mean_value
FROM table_name  
WHERE variable LIKE '%AGE%' AND stat_name = 'mean'

-- Combining multiple statistics
SELECT 
  group1_level,
  MAX(CASE WHEN stat_name = 'n' THEN CAST(stat AS INTEGER) END) as count,
  MAX(CASE WHEN stat_name = 'pct' THEN CAST(stat AS NUMERIC) END) as percentage
FROM table_name
WHERE variable = 'AE_CATEGORY'
GROUP BY group1_level, variable_level
```

## Traceability and Validation

### Why SQL Generation?

1. **Full Transparency**: Every query is visible and can be reviewed
2. **Reproducibility**: SQL queries can be saved and re-run
3. **Validation**: Domain experts can verify the logic
4. **Modification**: Queries can be manually adjusted if needed
5. **Documentation**: Queries serve as documentation of analyses

### Best Practices

1. **Review Generated SQL**: Always review the SQL before accepting results
2. **Check Assumptions**: The LLM states assumptions - verify they're correct
3. **Test on Sample Data**: Run queries on small datasets first
4. **Save Important Queries**: Export SQL for important analyses
5. **Version Control**: Track query changes over time

## Limitations

- Requires Azure OpenAI access and API credentials
- Complex multi-table joins may need manual adjustment
- Performance depends on Azure OpenAI availability
- SQL dialect is SQLite-compatible

## Troubleshooting

### Common Issues

1. **"ellmer package not found"**
   ```r
   remotes::install_github("ellmer/ellmer")
   ```

2. **"API key not found"**
   - Check your `.Renviron` file has the correct variables
   - Restart R after updating `.Renviron`

3. **"SQL execution failed"**
   - Review the generated SQL for syntax errors
   - Check that column names match your data
   - Ensure stat values can be cast to the expected type

### Debug Mode

For debugging, you can access the raw LLM response:
```r
result <- generate_sql_from_question(question, schema)
print(result$raw_response)  # See full LLM response
```

## Security Considerations

- API keys are stored in `.Renviron` and never exposed in code
- All queries are executed in a temporary in-memory database
- No data is sent to Azure OpenAI - only the schema structure
- Generated SQL is read-only (SELECT statements only)