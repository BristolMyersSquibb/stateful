% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtf_header_utils.R
\name{map_cell_to_colgroups}
\alias{map_cell_to_colgroups}
\title{Map data cell to column groups}
\usage{
map_cell_to_colgroups(cell_position, header_structure)
}
\arguments{
\item{cell_position}{Numeric position of the data cell}

\item{header_structure}{Header structure object from build_header_structure}
}
\value{
List with colgroup1, colgroup2, colgroup3 assignments
}
\description{
For a data cell at a specific position, determine its colgroup assignments
}
