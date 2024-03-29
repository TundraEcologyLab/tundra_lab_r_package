% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tomst_id_extractor.R
\name{tomst_id_extractor}
\alias{tomst_id_extractor}
\title{Extract tomst ID from a string}
\usage{
tomst_id_extractor(string, unique, as_null = TRUE)
}
\arguments{
\item{string}{A character vector that is to be examined for tomst IDs}

\item{unique}{Should result return all unique hits or a full list for counting occurrences}

\item{as_null}{If TRUE then NULL is returned if there is no hits, which is good for counting.
If False, then NA is returned if no hits, good for working with dataframes}
}
\description{
A function which accepts a character vector and returns a tomst ID, if present in the string.
A tomst ID is defined as a 8 digit number starting with a 9.  If no IDs are found then NULL is
returned, unless as_null is set to FALSE, then NA will be returned. Setting unique to FALSE means
all hits are returned, which is useful for counting them. Set as_null to FALSE if trying to fill
a dataframe with the results.
}
