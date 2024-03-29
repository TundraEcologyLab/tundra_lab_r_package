% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_year_from_string.R
\name{DOY_extractor}
\alias{DOY_extractor}
\title{Find Day of Year}
\usage{
DOY_extractor(string, three_digits = FALSE, breaks = FALSE, unique = TRUE)
}
\arguments{
\item{string}{A character vector that is to be examined for day of year}

\item{three_digits}{Should search be limited to 3 digit results}
}
\description{
A function which accepts a character vector and returns all unique instances within that string
that match a day of year format. If no days are found then it returns NULL. If three_digits is
set to TRUE then it will match any valid 3 digit number (100-366), else it matches any valid day
(1-366).
}
