#' Fill Parameter
#'
#' A function which examines a file's metadata and file path for a given piece of data and,
#' if it is found, adds that data to the dataframe if it isn't already present. As well as
#' a dataframe, the function requires an extractor_function: A function which uses regular
#' expressions to locate instances of data, and returns all matching examples of the data it
#' finds. Examples of extractor functions are year_extractor and DOY_extractor. The expected column
#' name of the parameter to be searched for is needed to determine if it is already present in
#' the dataframe, and a series of parameter_names may be provided that will be searched for when
#' looking for metadata.
#' @param dataframe A dataframe to be searched for the appropriate data, and which will have the data
#' added to it if it can be found
#' @param extractor_function A function which can search a string, or series of strings for the data
#' in question and return all examples it finds.
#' @param output_dir The directory that a file will be written to detailing all file_paths which did
#' not have the parameter in question, and for which it could not be found.
#' @param file_path The file_path for the original data. Needed so that it can be included in a file
#' detailing all files for which this function was unsuccessful
#' @param col_name The name of the column that, if present, should already contain the parameter in
#' question
#' @param parameter_names A character vector containing strings detailing all key words that should
#' be searched for when looking for metadata
#' @export


fill_parameter <- function(dataframe, extractor_function, output_dir, file_path, col_name,
                           parameter_names = col_name, ...){

  contains_parameter <- FALSE
  # If the column already exists, check that it contains the correct data. There are many examples
  # of year appearing as a column header when it was actually a single piece of unfortunately positioned
  # meta data
  if (col_name %in% names(dataframe)){
    # Find the index for the column containing col_name
    col_index <- which(names(dataframe) == col_name, arr.ind = TRUE)
    # Determine what percentage of the data with col_name fits the description defined within the
    # extractor function
    hit_ratio <- length(extractor_function(dataframe[[col_index]],
                                           breaks = TRUE,
                                           unique = FALSE,
                                           ...))/
      length(dataframe[[col_index]])
    # If at least 50% of the data appears correct then consider the dataframe to be correct as is.
    if (hit_ratio > 0.4){contains_parameter <- TRUE}
    # Else, remove the erroneous column with name col_name
    if (!contains_parameter){
      dataframe <- select(dataframe, !dplyr::matches(col_name))
    }
  }
  # If the parameter is not already part of the dataframe, load the whole file as a string to search
  # for metadata, especially that which may come before the header line and so not be included in the
  # dataframe
  if (!contains_parameter){
    # load all data
    file_lines <- read_lines(file_path)
    # combine the different names that should identify the meta data into one string separated by
    # "|" to create a regex expression
    parameter_pattern <- stringr::str_flatten(parameter_names, "|")
    # Identify any lines in the data that contain the parameter names
    parameter_lines <- grep(parameter_pattern, file_lines, ignore.case = TRUE, value = TRUE)
    # Confirm the presence and extract the actual meta data from the lines containing the
    # parameter names
    valid_hits <- vector("integer")
    for (line in parameter_lines){
      valid_hits <- append(valid_hits, extractor_function(parameter_lines, ...))
    }
    # If no meta data is found within the file, attempt to extract the data from the file path
    if (length(valid_hits) == 0){
      valid_hits <- extractor_function(file_path)
    }
    # If the extractor function is successful in finding data, either within the file or file_path,
    # Check that the data is consistent, then add it to the dataframe
    if (length(valid_hits) > 0){
      # Check that all entries in valid_hits are the same
      parameter_comparison <- rep(valid_hits[1], length(valid_hits))
      # Add the parameter to the dataframe if all entries are the same
      if (identical(valid_hits, parameter_comparison)){
        dataframe <- mutate(dataframe, "{col_name}" := valid_hits[1])
      }
    }
  }
  # Check that the dataframe now contains the parameter, if not, write the filepath to file for future
  # investigation
  contains_parameter <- FALSE
  if (col_name %in% names(dataframe)){
    col_index <- which(names(dataframe) == col_name, arr.ind = TRUE)
    hit_ratio <- length(extractor_function(dataframe[[col_index]],
                                           breaks = TRUE,
                                           unique = FALSE,
                                           ...))/
      length(dataframe[[col_index]])
    if (hit_ratio > 0.4){contains_parameter <- TRUE}
  }
  if (!contains_parameter){
    write_lines(file_path, append = TRUE, file = paste0(output_dir, "/missing_", col_name," .txt"))

  }
  dataframe
}
