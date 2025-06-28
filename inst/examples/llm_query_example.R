# Example: Using LLM Query Feature in Stateful
# This demonstrates how Azure OpenAI generates SQL queries from natural language

library(stateful)

# Load environment variables
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# Create sample ARD data (simulating parsed RTF data)
sample_ard <- data.frame(
  group1 = "TRT",
  group1_level = rep(c("Placebo", "Drug A", "Drug B"), each = 10),
  variable = rep(c("AGE", "AGE", "GENDER", "GENDER", "AE_MILD", "AE_MILD", 
                   "AE_SEVERE", "AE_SEVERE", "BIGN", "BIGN"), 3),
  variable_level = rep(c(NA, NA, "Male", "Female", "Headache", "Nausea", 
                        "Anaphylaxis", "Seizure", NA, NA), 3),
  stat_name = rep(c("mean", "sd", "n", "pct", "n", "pct", "n", "pct", "N", "N"), 3),
  stat_label = rep(c("Mean", "SD", "n", "Percentage", "n", "Percentage", 
                     "n", "Percentage", "N", "N"), 3),
  stat = c(
    # Placebo
    "65.2", "8.4", "45", "51.7", "42", "48.3", "12", "13.8", "2", "2.3", "87", "87",
    # Drug A  
    "64.8", "9.1", "43", "49.4", "44", "50.6", "15", "17.2", "0", "0", "87", "87",
    # Drug B
    "66.1", "7.9", "46", "52.9", "41", "47.1", "18", "20.7", "1", "1.1", "87", "87"
  ),
  stringsAsFactors = FALSE
)

# Wrap in list as if from parsed files
parsed_data <- list(
  "clinical_trial_results.rtf" = list(
    ard_data = sample_ard,
    metadata = list(
      title = "Table 1: Demographics and Safety Summary",
      population = "Safety Population"
    )
  )
)

# Example queries and their generated SQL
example_queries <- list(
  list(
    question = "What are the total number of patients in each treatment group?",
    expected_sql = "
SELECT group1_level as treatment_group, 
       CAST(stat AS INTEGER) as patient_count
FROM clinical_trial_results
WHERE variable = 'BIGN' AND stat_name = 'N'
GROUP BY group1_level
ORDER BY group1_level;"
  ),
  
  list(
    question = "Compare the mean age between treatment groups",
    expected_sql = "
SELECT group1_level as treatment_group,
       CAST(stat AS NUMERIC) as mean_age
FROM clinical_trial_results  
WHERE variable = 'AGE' AND stat_name = 'mean'
ORDER BY group1_level;"
  ),
  
  list(
    question = "Which treatment has the highest rate of severe adverse events?",
    expected_sql = "
SELECT group1_level as treatment_group,
       variable_level as adverse_event,
       CAST(stat AS NUMERIC) as percentage
FROM clinical_trial_results
WHERE variable = 'AE_SEVERE' AND stat_name = 'pct'
ORDER BY CAST(stat AS NUMERIC) DESC
LIMIT 1;"
  ),
  
  list(
    question = "Show me the gender distribution across all treatment groups",
    expected_sql = "
SELECT group1_level as treatment_group,
       variable_level as gender,
       CAST(stat AS INTEGER) as count,
       CAST((SELECT stat FROM clinical_trial_results t2 
             WHERE t2.group1_level = t1.group1_level 
             AND t2.variable_level = t1.variable_level 
             AND t2.stat_name = 'pct') AS NUMERIC) as percentage
FROM clinical_trial_results t1
WHERE variable = 'GENDER' AND stat_name = 'n'
ORDER BY group1_level, variable_level;"
  )
)

# Display examples
cat("=== LLM Query Examples ===\n\n")
cat("This feature uses Azure OpenAI to convert natural language questions\n")
cat("into SQL queries that can be executed on ARD data.\n\n")

for (i in seq_along(example_queries)) {
  example <- example_queries[[i]]
  cat("Example", i, ":\n")
  cat("Question:", example$question, "\n")
  cat("Generated SQL:", example$expected_sql, "\n")
  cat(rep("-", 60), "\n\n", sep="")
}

# Show table schema that would be sent to LLM
cat("=== Table Schema (sent to LLM) ===\n")
schema <- generate_table_schema(parsed_data)
cat(schema, "\n\n")

# Example of what the LLM prompt looks like
cat("=== Example LLM Prompt ===\n")
cat("System: You are an expert SQL developer specializing in clinical trial data...\n")
cat("User: Generate a SQL query to answer: 'What is the mean age by treatment?'\n\n")

cat("The LLM would then generate:\n")
cat("- SQL query with proper CAST operations\n")
cat("- Explanation of what the query does\n")
cat("- Any assumptions made\n\n")

cat("✅ All queries are traceable and can be reviewed before execution!\n")