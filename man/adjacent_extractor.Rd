% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjacent_extractor.R
\name{adjacent_extractor}
\alias{adjacent_extractor}
\title{Extract from adjacent cell to label}
\usage{
adjacent_extractor(string, label, ignore.case = TRUE)
}
\arguments{
\item{string}{A character string that will be checked for the presence of the label, and
from which the data will be extracted if the label is present. The character string will
be generated by loading the dataframe using read.lines, otherwise it may not be in the right
format. fill_parameter takes care of this for you.}

\item{label}{A regex expression (can be treated as a character string if special characters are
avoided) that will match what should be found to the left of the data completely}

\item{ignore.case}{should the search return a hit regardless if in upper or lower case. Defaults
to TRUE}
}
\description{
A function designed for use specifically with the fill_parameter function. fill_parameter,
when given this extractor function, will return whatever data is found located one cell
to the right of the label, if the label is found. For this use, the meta_only parameter
should be set to TRUE and full_existing_check set to FALSE. If string is a character vector
of length greater than one then a character vector of hits from all strings is returned.
}
