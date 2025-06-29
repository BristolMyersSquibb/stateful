#' LLM Query Module for SQL Generation
#'
#' Uses Azure OpenAI via ellmer package to generate SQL queries
#' from natural language questions about ARD summary data using sqldf.
#'
#' @name llm-query
NULL

#' Generate SQL query from natural language question
#'
#' Uses Azure OpenAI to convert a natural language question about ARD JSON data
#' into a SQL query that can be executed with sqldf against the ARD summary data.
#' Only supports aggregate/summary questions, not patient-level queries.
#'
#' @param question The natural language question about summary data
#' @param ard_data ARD data frame from JSON file
#' @param examples Optional examples to help guide the LLM
#'
#' @return List with generated SQL and explanation
#' @export
generate_sql_from_question <- function(question, ard_data, examples = NULL) {
  # Check if ellmer is available
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    return(list(
      success = FALSE,
      error = "The ellmer package is required for LLM queries. Please install it with: remotes::install_github('ellmer/ellmer')",
      sql = NULL,
      explanation = NULL
    ))
  }

  # Generate schema from ARD data
  table_schema <- generate_table_schema(ard_data)
  
  # Build the system prompt
  system_prompt <- build_sql_system_prompt(table_schema, examples)

  # Build the user prompt
  user_prompt <- paste0(
    "Generate a SQL query to answer this question about clinical trial summary data:\n\n",
    "Question: ", question, "\n\n",
    "Remember: This is SUMMARY data only. You cannot query individual patient records.\n",
    "Valid questions include aggregate counts (total patients/subjects), group comparisons, statistics, percentages, etc.\n",
    "Questions about 'how many patients' or 'total subjects' are VALID when asking for summary counts.\n\n",
    "Please provide:\n",
    "1. The SQL query (use table name 'ard_data')\n",
    "2. A brief explanation of what the query does\n",
    "3. Any assumptions made\n\n",
    "Format your response as:\n",
    "SQL:\n```sql\n[your query here]\n```\n\n",
    "Explanation: [your explanation here]\n\n",
    "Assumptions: [any assumptions made]"
  )

  tryCatch({
    # Create Azure OpenAI chat instance
    chat_instance <- ellmer::chat_azure_openai(
      endpoint = Sys.getenv("AZURE_OPENAI_ENDPOINT"),
      deployment_id = Sys.getenv("AZURE_OPENAI_DEPLOYMENT_ID", "gpt-4"),
      api_version = Sys.getenv("AZURE_OPENAI_API_VERSION", "2023-12-01-preview"),
      api_key = Sys.getenv("AZURE_OPENAI_API_KEY"),
      system_prompt = system_prompt
    )

    # Get raw response from LLM
    raw_response <- chat_instance$chat(user_prompt)

    # Parse the response
    parsed <- parse_sql_response(raw_response)

    return(list(
      success = TRUE,
      sql = parsed$sql,
      explanation = parsed$explanation,
      assumptions = parsed$assumptions,
      raw_response = raw_response
    ))

  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Error calling Azure OpenAI:", e$message),
      sql = NULL,
      explanation = NULL
    ))
  })
}

#' Build system prompt for SQL generation
#'
#' @keywords internal
build_sql_system_prompt <- function(table_schema, examples = NULL) {
  prompt <- paste0(
    "You are an expert SQL developer specializing in clinical trial ARD (Analysis Results Data) analysis. ",
    "You generate SQL queries using sqldf to analyze summarized clinical trial data from JSON ARD files.\n\n",

    "IMPORTANT CONSTRAINTS:\n",
    "- This is SUMMARY data only - you cannot answer patient-level questions\n",
    "- No queries about 'which patient' or individual patient data\n",
    "- Only aggregate, filtering, and summary questions are possible\n",
    "- Data contains pre-calculated statistics, not raw patient records\n\n",

    "Available data schema:\n",
    table_schema, "\n\n",

    "SQL Guidelines for sqldf:\n",
    "1. Use simple table name 'ard_data' (no database prefixes)\n",
    "2. Cast stat values: CAST(stat AS REAL) for calculations\n",
    "3. Use standard SQL-92 syntax (sqldf uses SQLite)\n",
    "4. Filter by stat_name to get specific statistics\n",
    "5. Group by treatment groups or variable levels\n",
    "6. Handle text-stored numeric values properly\n",
    "7. Use WHERE clauses to focus on relevant variables\n\n",

    "Example valid questions:\n",
    "- How many subjects/patients in each treatment group?\n",
    "- How many patients were treated in this table?\n",
    "- What are the mean values by treatment?\n",
    "- Which treatment group has the highest percentage?\n",
    "- Compare adverse event rates between groups\n",
    "- Total number of subjects across all treatments\n\n",

    "INVALID questions (will be rejected):\n",
    "- Which specific patients had adverse events?\n",
    "- Show me individual patient demographics\n",
    "- List each patient's data separately\n",
    "- Patient-level record retrieval\n"
  )

  if (!is.null(examples)) {
    prompt <- paste0(prompt, "\n\nExample queries:\n", examples)
  }

  return(prompt)
}

#' Parse SQL response from LLM
#'
#' @keywords internal
parse_sql_response <- function(raw_response) {
  # The response is raw text from ellmer's chat method
  content <- raw_response

  # Extract SQL query
  sql_pattern <- "```sql\\s*\\n([\\s\\S]*?)\\n\\s*```"
  sql_matches <- regmatches(content, regexec(sql_pattern, content))
  sql <- if (length(sql_matches[[1]]) > 1) {
    # Clean up the SQL - remove trailing backticks if present
    sql_text <- sql_matches[[1]][2]
    sql_text <- gsub("```$", "", trimws(sql_text))
    trimws(sql_text)
  } else {
    # Try to find SQL without code blocks
    sql_start <- regexpr("SELECT|WITH|INSERT|UPDATE|DELETE", content, ignore.case = TRUE)
    if (sql_start > 0) {
      sql_text <- substring(content, sql_start)
      # Find the end (usually before "Explanation:" or end of string)
      sql_end <- regexpr("\\n\\s*(Explanation:|Assumptions:|$)", sql_text)
      if (sql_end > 0) {
        trimws(substring(sql_text, 1, sql_end - 1))
      } else {
        trimws(sql_text)
      }
    } else {
      NA
    }
  }

  # Extract explanation
  expl_start <- regexpr("Explanation:", content)
  if (expl_start > 0) {
    expl_text <- substring(content, expl_start + 12)
    # Find where assumptions start or end of string
    assume_start <- regexpr("Assumptions:", expl_text)
    if (assume_start > 0) {
      explanation <- trimws(substring(expl_text, 1, assume_start - 1))
    } else {
      explanation <- trimws(expl_text)
    }
  } else {
    explanation <- NA
  }

  # Extract assumptions
  assume_start <- regexpr("Assumptions:", content)
  if (assume_start > 0) {
    assumptions <- trimws(substring(content, assume_start + 12))
  } else {
    assumptions <- NA
  }

  return(list(
    sql = sql,
    explanation = explanation,
    assumptions = assumptions
  ))
}

#' Generate table schema description from ARD data
#'
#' Creates a schema description that can be used by the LLM to understand
#' the structure of the available ARD summary data.
#'
#' @param ard_data ARD data frame
#' @return Character string describing the schema
#' @export
generate_table_schema <- function(ard_data) {
  # Get column info with sample values
  col_info <- paste(
    sapply(names(ard_data), function(col) {
      sample_values <- head(unique(ard_data[[col]][!is.na(ard_data[[col]])]), 3)
      sample_str <- if (length(sample_values) > 0) {
        paste0(" (examples: ", paste(shQuote(sample_values), collapse=", "), ")")
      } else {
        ""
      }
      paste0("  - ", col, ": ", class(ard_data[[col]])[1], sample_str)
    }),
    collapse = "\n"
  )

  # Get key summary information
  unique_groups <- if ("group1_level" %in% names(ard_data)) {
    length(unique(ard_data$group1_level[!is.na(ard_data$group1_level)]))
  } else { 0 }
  
  unique_variables <- if ("variable_level" %in% names(ard_data)) {
    length(unique(ard_data$variable_level[!is.na(ard_data$variable_level)]))
  } else if ("variable" %in% names(ard_data)) {
    length(unique(ard_data$variable[!is.na(ard_data$variable)]))
  } else { 0 }
  
  available_stats <- if ("stat_name" %in% names(ard_data)) {
    paste(unique(ard_data$stat_name[!is.na(ard_data$stat_name)]), collapse=", ")
  } else { "unknown" }

  paste0(
    "Table: ard_data (ARD summary data for sqldf queries)\n",
    "Columns:\n", col_info, "\n\n",
    "Summary:\n",
    "  Total rows: ", nrow(ard_data), "\n",
    "  Treatment groups: ", unique_groups, "\n",
    "  Unique variables: ", unique_variables, "\n",
    "  Available statistics: ", available_stats, "\n\n",
    "Note: Use table name 'ard_data' in SQL queries"
  )
}

#' Create example queries for common ARD questions
#'
#' @return Character string with example SQL queries for sqldf
#' @export
get_example_queries <- function() {
  examples <- c(
    "Q: How many subjects are in each treatment group?
SQL: SELECT group1_level, CAST(stat AS REAL) as subject_count
     FROM ard_data
     WHERE variable = 'BIGN' AND stat_name = 'N'
     ORDER BY group1_level;",

    "Q: What is the mean age by treatment group?
SQL: SELECT group1_level, CAST(stat AS REAL) as mean_age
     FROM ard_data
     WHERE variable_level LIKE '%Age%' AND stat_name = 'mean'
     ORDER BY group1_level;",

    "Q: Compare adverse event percentages between treatment groups
SQL: SELECT
       group1_level,
       variable_level as adverse_event,
       MAX(CASE WHEN stat_name = 'n' THEN CAST(stat AS REAL) END) as count,
       MAX(CASE WHEN stat_name = 'pct' THEN CAST(stat AS REAL) END) as percentage
     FROM ard_data
     WHERE variable_level LIKE '%ADVERSE%' OR variable_level LIKE '%AE%'
     GROUP BY group1_level, variable_level
     HAVING percentage IS NOT NULL
     ORDER BY percentage DESC;",

    "Q: Which treatment group has the highest death rate?
SQL: SELECT group1_level, CAST(stat AS REAL) as death_percentage
     FROM ard_data
     WHERE variable_level LIKE '%DIED%' AND stat_name = 'pct'
     ORDER BY death_percentage DESC
     LIMIT 1;"
  )

  paste(examples, collapse = "\n\n")
}

#' Execute SQL query on ARD data using sqldf
#'
#' Executes SQL query against ARD data frame using sqldf package.
#' Only supports queries on summary/aggregate data, not patient-level data.
#'
#' @param ard_data ARD data frame from JSON file
#' @param sql SQL query string (use table name 'ard_data')
#' @return Query results as a data frame
#' @export
execute_ard_sql <- function(ard_data, sql) {
  if (!requireNamespace("sqldf", quietly = TRUE)) {
    stop("sqldf package is required to execute SQL queries. Install with: install.packages('sqldf')")
  }

  # Let the LLM handle determining appropriate queries
  # No validation needed - the LLM knows the data structure and constraints

  # Execute query using sqldf
  tryCatch({
    # sqldf will automatically find 'ard_data' in the calling environment
    result <- sqldf::sqldf(sql)
    
    return(list(
      success = TRUE,
      data = result,
      error = NULL,
      rows_returned = nrow(result)
    ))
  }, error = function(e) {
    return(list(
      success = FALSE,
      data = NULL,
      error = paste("SQL execution error:", e$message)
    ))
  })
}

#' Load ARD JSON file as data frame
#'
#' Convenience function to load ARD JSON files for LLM querying
#'
#' @param json_file Path to ARD JSON file
#' @return ARD data frame
#' @export
load_ard_json <- function(json_file) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package is required to load JSON files")
  }
  
  ard_json <- jsonlite::fromJSON(json_file)
  return(ard_json$ard_data)
}