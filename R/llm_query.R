#' LLM Query Module for SQL Generation
#'
#' Uses Azure OpenAI via ellmer package to generate SQL queries
#' from natural language questions about ARD data.
#'
#' @name llm-query
NULL

#' Generate SQL query from natural language question
#'
#' Uses Azure OpenAI to convert a natural language question about ARD data
#' into a SQL query that can be executed against the data.
#'
#' @param question The natural language question
#' @param table_schema Description of available tables and columns
#' @param examples Optional examples to help guide the LLM
#'
#' @return List with generated SQL and explanation
#' @export
generate_sql_from_question <- function(question, table_schema, examples = NULL) {
  # Check if ellmer is available
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    return(list(
      success = FALSE,
      error = "The ellmer package is required for LLM queries. Please install it with: remotes::install_github('ellmer/ellmer')",
      sql = NULL,
      explanation = NULL
    ))
  }

  # Build the system prompt
  system_prompt <- build_sql_system_prompt(table_schema, examples)

  # Build the user prompt
  user_prompt <- paste0(
    "Generate a SQL query to answer this question about clinical trial data:\n\n",
    "Question: ", question, "\n\n",
    "Please provide:\n",
    "1. The SQL query\n",
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
    "You are an expert SQL developer specializing in clinical trial data analysis. ",
    "You generate precise, efficient SQL queries for ARD (Analysis Results Data) format data.\n\n",

    "The data follows the CDISC ARD standard with these key columns:\n",
    "- group1, group1_level: Primary grouping (usually treatment groups)\n",
    "- group2, group2_level: Secondary grouping (if present)\n",
    "- variable: The variable being measured\n",
    "- variable_level: Specific level of the variable\n",
    "- stat_name: Type of statistic (n, pct, mean, sd, etc.)\n",
    "- stat_label: Human-readable label for the statistic\n",
    "- stat: The numeric value\n\n",

    "Available tables and their schemas:\n",
    table_schema, "\n\n",

    "Guidelines:\n",
    "1. Always use proper SQL syntax compatible with standard SQL or SQLite\n",
    "2. Include appropriate JOINs when querying multiple tables\n",
    "3. Use CAST(stat AS NUMERIC) when performing calculations on stat values\n",
    "4. Consider that stat values are stored as text but contain numeric data\n",
    "5. Use meaningful aliases for clarity\n",
    "6. Include comments in complex queries\n",
    "7. Handle NULL values appropriately\n"
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
#' the structure of the available data.
#'
#' @param ard_data_list Named list of ARD data frames
#' @return Character string describing the schema
#' @export
generate_table_schema <- function(ard_data_list) {
  schema_parts <- lapply(names(ard_data_list), function(table_name) {
    df <- ard_data_list[[table_name]]$ard_data

    # Get column info
    col_info <- paste(
      sapply(names(df), function(col) {
        sample_values <- head(unique(df[[col]][!is.na(df[[col]])]), 3)
        sample_str <- if (length(sample_values) > 0) {
          paste0(" (e.g., ", paste(sample_values, collapse=", "), ")")
        } else {
          ""
        }
        paste0("  - ", col, ": ", class(df[[col]])[1], sample_str)
      }),
      collapse = "\n"
    )

    # Get summary stats
    summary_info <- paste0(
      "  Total rows: ", nrow(df), "\n",
      "  Unique groups: ", length(unique(df$group1_level)), "\n",
      "  Unique variables: ", length(unique(df$variable))
    )

    paste0(
      "Table: ", table_name, "\n",
      "Columns:\n", col_info, "\n",
      "Summary:\n", summary_info
    )
  })

  paste(schema_parts, collapse = "\n\n")
}

#' Create example queries for common questions
#'
#' @return Character string with example SQL queries
#' @export
get_example_queries <- function() {
  examples <- c(
    "Q: How many patients are in each treatment group?
SQL: SELECT group1_level, CAST(stat AS INTEGER) as patient_count
     FROM ard_data
     WHERE variable = 'BIGN' AND stat_name = 'N'
     ORDER BY group1_level;",

    "Q: What is the mean age by treatment group?
SQL: SELECT group1_level, CAST(stat AS NUMERIC) as mean_age
     FROM ard_data
     WHERE variable LIKE '%Age%' AND stat_name = 'mean'
     ORDER BY group1_level;",

    "Q: Compare adverse event rates between groups
SQL: SELECT
       group1_level,
       variable_level as adverse_event,
       MAX(CASE WHEN stat_name = 'n' THEN CAST(stat AS INTEGER) END) as count,
       MAX(CASE WHEN stat_name = 'pct' THEN CAST(stat AS NUMERIC) END) as percentage
     FROM ard_data
     WHERE variable LIKE '%Adverse%' OR variable LIKE '%AE%'
     GROUP BY group1_level, variable_level
     ORDER BY percentage DESC;"
  )

  paste(examples, collapse = "\n\n")
}

#' Execute SQL query on ARD data
#'
#' Converts ARD data to a temporary SQLite database and executes the query.
#'
#' @param sql SQL query string
#' @param ard_data_list Named list of ARD data frames
#' @return Query results as a data frame
#' @export
execute_ard_sql <- function(sql, ard_data_list) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("RSQLite package is required to execute SQL queries")
  }

  # Create temporary SQLite connection
  con <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  # Load each table into the database
  for (table_name in names(ard_data_list)) {
    ard_data <- ard_data_list[[table_name]]$ard_data
    RSQLite::dbWriteTable(con, table_name, ard_data, overwrite = TRUE)
  }

  # Execute query
  tryCatch({
    result <- RSQLite::dbGetQuery(con, sql)
    return(list(
      success = TRUE,
      data = result,
      error = NULL
    ))
  }, error = function(e) {
    return(list(
      success = FALSE,
      data = NULL,
      error = e$message
    ))
  })
}