#' Locate header line within CSV or Tab deliminated file
#'
#' A function which accepts a file_path and separator and attempts to Find the position
#' of the column headers, if any, within that file. It ignores meaningless header names
#' "...(number)" and "X(number)". A row is assumed to be a header line if columns 2,3,
#' and 4 do not contain numerical data. up to the first 10 rows are checked. If a header is
#' found, the number of rows to skip while reading in the file to make the header the first
#' row read is returned. If no header is found the string "no header" is returned.
#' @param file_path A path to the file that is to be checked for a header line
#' @param sep A character vector containing the character used to deliminate the file
#' @export
find_header <- function(file_path, sep, multi_header = FALSE){

  number_of_rows <- length(count.fields(file_path, sep))
  # set number of rows to examine for a header to 10, unless there are less than 10 rows in file
  # or multi_header = TRUE
  if (number_of_rows > 10 & !multi_header){
      search_limit <- 10
  } else {
      search_limit <- number_of_rows
  }
  # Load top of file to search for header line
  # count.fields provides the number of columns per line. This is required to prevent R
  # from truncating later columns that have additional columns. count.fields returns NA
  # when a field contains a character string with carriage returns.
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
                                           row.names = NULL,
                                           nrows = search_limit,
                                           quote = "\""
  ))
    # Determine number of rows to skip to laod file correctly with found header line
    skips <- search_dataframe_for_header(dataframe, search_limit, multi_header)
    if (skips != "no header"){
        skips <- skips - 1
    }
    skips
}
