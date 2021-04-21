#' Load CSV or Tab deliminated files despite asymmetry
#'
#' A function to load the data from the file identified in file_path. If no separator is provided
#' it uses file_type_identifier to predict it. It also uses find_header()
#' to attempt to locate meaningful header names, and sets the variable skip accordingly (see
#' find_header() for details). If the header does not cover every column in the file then the
#' header is ignored. load_file is configured to handle asymmetric files, and to strip white
#' space from fields. All columns are read as characters to provide consistent data retrieval.
#' If split_file TRUE, then a list of dataframes is returned, each dataframe consisting of all
#' rows with the same number of columns
#' @param file_path A path to the file which is to be loaded
#' @param sep A character vector of the separator used by the file to distinguish columns
#' @param split_file If TRUE, a list of dataframes is returned, each dataframe consisting of all
#' rows with the same number of columns
#' @export

load_file <- function(file_path, sep = NULL, split_file = FALSE){
  # If the separator has not been provided attempt to predict it
  if (is.null(sep)){
    sep <- file_type_identifier(file_path)
  }
  # If file is not to be split, load the whole file
  if (!split_file){
  # Determine the number of rows to skip such that the first row is the header line
  skip <- find_header(file_path, sep)
  # If header found, read in the marked data into dataframe, else read all data into dataframe. All
  # data read as character class to avoid misleading factors or missing data.
  if (skip != "no header"){


    # If the header does not have a column name for every column with data then read.table will
    # return an error. This will result in dataframe having a length of 0.
    dataframe <- data.frame()
    try(dataframe <- dplyr::as_tibble(read.table(file = file_path,
                                          skip = skip,
                                          header = TRUE,
                                          colClasses = "character",
                                          sep = sep,
                                          fill = TRUE,
                                          strip.white = TRUE,
                                          check.names = TRUE,
                                          row.names = NULL,
    )))

    # If the found header does not cover all data read the csv file as if no header was found
    if (length(dataframe) == 0){
      # count.fields provides the number of columns per line. This is required to prevent R
      # from truncating later columns that have additional columns. count.fields returns NA
      # when a field contains a character string with cariage returns.
      # The manual naming of columns to the length of the longest row ensures all data is loaded
      number_of_cols <- max(
        count.fields(file_path, sep = sep, quote = '""')[!is.na(count.fields(
          file_path, sep = sep, quote = '""'))])
      dataframe <- dplyr::as_tibble(read.table(file = file_path,
                                        header = FALSE,
                                        col.names = paste0("V", seq_len(number_of_cols)),
                                        colClasses = "character",
                                        sep = sep,
                                        fill = TRUE,
                                        strip.white = TRUE,
                                        check.names = TRUE,
                                        row.names = NULL
      ))
    }
  } else {
    # count.fields provides the number of columns per line. This is required to prevent R
    # from truncating later columns that have additional columns. count.fields returns NA
    # when a field contains a character string with cariage returns.
    # The manual naming of columns to the length of the longest row ensures all data is loaded
    number_of_cols <- max(
      count.fields(file_path, sep = sep, quote = '""')[!is.na(count.fields(
        file_path, sep = sep, quote = '""'))])
    dataframe <- dplyr::as_tibble(read.table(file = file_path,
                                      header = FALSE,
                                      col.names = paste0("V", seq_len(number_of_cols)),
                                      colClasses = "character",
                                      sep = sep,
                                      fill = TRUE,
                                      strip.white = TRUE,
                                      check.names = TRUE,
                                      row.names = NULL
    ))
  }
  } else {
    # If files are to be split, produce a list of dataframes - each dataframe
    # containing all rows with the same number of columns

    # Determine length of every row in file
    row_lengths <- count.fields(file_path, sep, '""')
    # Determine unique row lengths in file
    unique_row_lengths <- unique(sort(row_lengths))


    # create list to store segregated file parts
    dataframe <- list()
    # open connection to file

    # read data into dataframes, grouped by the unique_row_lengths
    dataframe <- lapply(unique_row_lengths,
                     function(file, row_lengths, sep, unique_row_length){
                       load_specific_row_length(file, row_lengths, sep, unique_row_length)},
                     file = file_path,
                     row_lengths = row_lengths,
                     sep = sep)



  }
  # fix duplicate, missing, or invalid column names
  names(dataframe) <- vctrs::vec_as_names(names(dataframe), repair = "universal")
  dataframe
}

