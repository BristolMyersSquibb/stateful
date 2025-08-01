% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtf_parser.R
\name{rtf-parser}
\alias{rtf-parser}
\title{Enhanced RTF State Parser}
\description{
Enhanced state-based RTF table parser using structural markers with support for:
\itemize{
\item Multi-level header detection and column group mapping
\item Cell position tracking for accurate column assignment
\item Row hierarchy detection (label-type and group-type)
\item Page-aware processing (titles/footnotes only from page 1)
}
}
\details{
States: 0=pre_header, 1=header, 2=table_body, 3=footnotes
Transitions only on structural markers, never content patterns
}
