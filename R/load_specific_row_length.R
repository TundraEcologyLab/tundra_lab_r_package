#' A function to read a file and extract only the rows with a length corresponding to
#' row_length. This function is used by load_file to produce a list of dataframes, where
#' each element of the list is a dataframe of all rows with the same row_length.
#' @param file_path The file path to the tabular data which is to be loaded
#' @param row_lengths The number of columns in every line of the file. Can be produced using the
#' count.fields function
#' @param sep A character vector detailing the delimiter for the file: either "," or ""
#' @param row_length The number of columns a row should have to be included in the outputed
#' dataframe
#' @export

load_specific_row_length <- function(file_path, row_lengths, sep = ",", row_length){
  # Get indexes for each of the rows to be loaded
  rows_to_load <- which(row_lengths == row_length, TRUE)
  # Determine the number of rows to skip such that the first row read will be correct
  first_skip <- rows_to_load[1] - 1
  # A file connection remembers the last line to be read, and automatically begins again at the 
  # proceeding line. Thus, all skips must be calculated relative to the last line read, as opposed 
  # to their absolute position in the file.
  if (length(rows_to_load) > 1){
    skips <- (rows_to_load[2:length(rows_to_load)] - rows_to_load[1:(length(rows_to_load) - 1)]) - 1
    
    skips <- append(first_skip, skips)
  } else {skips <- first_skip}
  
  # open connection to file
  con <- file(file_path)
  open(con)
  # Create a list of all the individual rows with the correct length
  data_list <- lapply(skips, FUN = function(con, sep, skip){
    row <- read.table(file = con,
                      header = FALSE,
                      sep = sep,
                      colClasses = "character",
                      nrows = 1, 
                      skip = skip
    )
  }, con = con, sep = sep)
  # close connection
  close(con)
  
  # combine all the individual rows into one dataframe
  dataframe <- do.call(rbind, data_list)
  # convert dataframe to a tibble
  dataframe <- dplyr::as_tibble(dataframe)
}