#' Locate header line within CSV or Tab deliminated file
#'
#' A function which accepts a file_path and separator and attempts to Find the position
#' of the column headers, if any, within that file. It ignores meaningless header names
#' "...(number)" and "X(number)". A row is assumed to be a header line if columns 2,3, 4,
#' and 5 do not contain numerical data. up to the first 10 rows are checked. Otherwise, or if more than 2
#' potential headers are detected, then a second approach is tried. The first five columns are checked
#'  for repeating data (these columns often include data such as site or date which can be the
#'  same for many rows). If repeating data is found (at least five repetitions), then the row
#'  immediately before the first instance of the repetitive data is considered to be the header
#'  line. If this fails, or if the dataframe has less than 5 columns then the string "no header"
#'  is returned. If multi_header == TRUE (if the file is anticipated to consist
#' of multiple dataframes on top of each other separated by header lines) then it is attempted to
#' detect all header lines. If more than 10 headers are detected it is assumed to be erroneous and
#' and "no header" is returned. The repetitive string approach will not attempt to detect multi_headers
#' and so only the first header will be detected if this approach is required.
#' If headers are found, the number of rows to skip while reading in the file to make the header the first
#' row read is returned. If no header is found the string "no header" is returned.
#' @param file_path A path to the file that is to be checked for a header line
#' @param sep A character vector containing the character used to deliminate the file
#' @param multi_header Is the file is anticipated to consist of multiple dataframes on top of each
#'  other separated by header lines, and should all headers be detected?
#' @export
find_header <- function(file_path, sep, multi_header = FALSE){

    number_of_rows <- length(count.fields(file_path, sep))
    # set number of rows to examine for a header to 10 (requires loading 20 rows to look for repetitive
    # strings), unless there are less than 20 rows in file, or multi_header = TRUE
    # or multi_header = TRUE
    if (number_of_rows > 20 & !multi_header){
        search_limit <- 10
        rows_to_load <- 20
    } else {
        search_limit <- number_of_rows
        rows_to_load <- number_of_rows
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
                                             nrows = rows_to_load,
                                             quote = "\""
    ))
    # Determine number of rows to skip to laod file correctly with found header line
    skips <- search_dataframe_for_header(dataframe, search_limit, multi_header)
    if (skips != "no header"){
        skips <- skips - 1
    }
    skips
}
