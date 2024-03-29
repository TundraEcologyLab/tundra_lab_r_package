% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fill_parameter.R
\name{fill_parameter}
\alias{fill_parameter}
\title{Fill Parameter}
\usage{
fill_parameter(
  dataframe,
  extractor_function,
  output_dir,
  file_path,
  col_name,
  breaks = TRUE,
  parameter_names = col_name,
  meta_only = FALSE,
  full_existing_check = TRUE,
  ...
)
}
\arguments{
\item{dataframe}{A dataframe to be searched for the appropriate data, and which will have the data
added to it if it can be found}

\item{extractor_function}{A function which can search a string, or series of strings for the data
in question and return all examples it finds.}

\item{output_dir}{The directory that a file will be written to detailing all file_paths which did
not have the parameter in question, and for which it could not be found.}

\item{file_path}{The file_path for the original data. Needed so that it can be included in a file
detailing all files for which this function was unsuccessful}

\item{col_name}{The name of the column that, if present, should already contain the parameter in
question}

\item{breaks}{Does the extractor function have a breaks option}

\item{parameter_names}{A character vector containing strings detailing all key words that should
be searched for when looking for metadata}

\item{meta_only}{If TRUE, then only the meta data within the file will be searched, not the file path}

\item{full_existing_check}{If FALSE then the check that the data already exists within the dataframe
simply checks for a column of the correct name. If TRUE then it also uses the extractor function to
check that the column is actually populated with relevant data. Must be FALSE when using the
adjacent_extractor function.}
}
\description{
A function which examines a file's metadata and file path for a given piece of data and,
if it is found, adds that data to the dataframe if it isn't already present. As well as
a dataframe, the function requires an extractor_function: A function which uses regular
expressions to locate instances of data, and returns all matching examples of the data it
finds. Examples of extractor functions are year_extractor and DOY_extractor. The expected column
name of the parameter to be searched for is needed to determine if it is already present in
the dataframe, and a series of parameter_names may be provided that will be searched for when
looking for metadata.
}
