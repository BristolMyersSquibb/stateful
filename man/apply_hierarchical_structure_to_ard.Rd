% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtf_parser.R
\name{apply_hierarchical_structure_to_ard}
\alias{apply_hierarchical_structure_to_ard}
\title{Apply hierarchical structure to ARD data}
\usage{
apply_hierarchical_structure_to_ard(
  ard_data,
  hierarchical_structure,
  row_analysis
)
}
\arguments{
\item{ard_data}{Base ARD data frame}

\item{hierarchical_structure}{Output from build_hierarchical_structure}

\item{row_analysis}{Output from detect_table_indentation_patterns}
}
\value{
Updated ARD data frame with hierarchical information
}
\description{
Updates ARD data frame with hierarchical variable labels and group structure
based on the detected indentation patterns.
}
\keyword{internal}
