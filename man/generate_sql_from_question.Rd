% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/llm_query.R
\name{generate_sql_from_question}
\alias{generate_sql_from_question}
\title{Generate SQL query from natural language question}
\usage{
generate_sql_from_question(question, ard_data, examples = NULL)
}
\arguments{
\item{question}{The natural language question about summary data}

\item{ard_data}{ARD data frame from JSON file}

\item{examples}{Optional examples to help guide the LLM}
}
\value{
List with generated SQL and explanation
}
\description{
Uses Azure OpenAI to convert a natural language question about ARD JSON data
into a SQL query that can be executed with sqldf against the ARD summary data.
Only supports aggregate/summary questions, not patient-level queries.
}
