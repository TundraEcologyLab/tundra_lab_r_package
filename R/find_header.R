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
find_header <- function(file_path, sep){
  # number of rows to skip when reading file so that the first row read is the header
  skip <- vector("integer")
  row_number <- 1  # The current row being read
  # total number of rows in file
  number_of_rows <- length(count.fields(file_path, sep))
  # set number of rows to examine for a header to 10, unless there are less than 10 rows in file
  if (number_of_rows > 10){
      search_limit <- 10
  } else {
      search_limit <- number_of_rows
  }
  # While no header has been found and 10 rows have not yet been checked, read the next
  # row and check if it has non-numerical data in columns 2, 3, and 4.
  while (length(skip) == 0 && row_number < search_limit){
    # read next row of file, all data as characters
    row <- dplyr::as_tibble(read.table(file = file_path,
                                skip = row_number - 1,
                                header = FALSE,
                                colClasses = "character",
                                sep = sep,
                                strip.white = TRUE,
                                row.names = NULL,
                                nrows = 1))

    # Only continue if there are at least 4 columns
    if (length(row) > 3){
      # If columns 2,3 and 4 contain data and not common meaningless header names
      if (!is.na(row[2]) && !is.na(row[3]) && !is.na(row[4]) &&
          !grepl("\\.\\.\\.[0-9]", row[2]) && !grepl("(^|,) *X[0-9] *(,|$)", row[2])){
        # And if all that data is not numeric then set skip to one less row number
        row <- as.numeric(row)
        if (is.na(row[2]) && is.na(row[3]) && is.na(row[4])){skip <- row_number - 1}
      }
    }
    row_number <- row_number + 1
  }
  # If no header line found set skip to "no header"
  if (length(skip) == 0){skip = "no header"}
  skip # Return number of rows to skip when loading file to make the first row the headers
}
