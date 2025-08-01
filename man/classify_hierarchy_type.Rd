% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtf_hierarchy_utils.R
\name{classify_hierarchy_type}
\alias{classify_hierarchy_type}
\title{Classify hierarchy type based on presence of statistics}
\usage{
classify_hierarchy_type(row_data)
}
\arguments{
\item{row_data}{Data frame for a single row}
}
\value{
Character "label", "group", or "none"
}
\description{
Determine if this is a label-type (no stats) or group-type (with stats) hierarchy
}
\keyword{internal}
