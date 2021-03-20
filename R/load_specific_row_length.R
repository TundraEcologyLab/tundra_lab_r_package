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
    # Adjust dataframe if header line is detected
    dataframe <- reheader_dataframe(dataframe)

}
#' A function which accepts a dataframe as input. It checks the first 10 rows using the
#' search_dataframe_for_header function, to determine if a header line is found within the
#' data. If such a header is detected it returns a new dataframe excluding any data above
#' the header line and with the columns named after the found header line. If no header
#' is detected it returns the original dataframe unchanged
reheader_dataframe <- function(dataframe){
    # Check if the dataframe contains a header line within the data
    header_row <- search_dataframe_for_header(dataframe)
    # total number of observations in dataframe
    total_rows <- length(dataframe[[1]])
    # If a header was found create a new dataframe with names taken from the found header line
    # and excluding all data (if any) before the header
    if (header_row != "no header"){
        # Create new dataframe with all data below found header line
        new_dataframe <- dataframe[(header_row + 1):total_rows,]
        # Set header names of new dataframe to the names found in the detected header line
        header_names <- as.character(dataframe[header_row,])
        names(new_dataframe) <- header_names
        dataframe <- new_dataframe
    }
    dataframe
}
