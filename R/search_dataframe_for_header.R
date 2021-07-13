#' A function which takes a dataframe as input, and returns the row number where it
#' has detected a possible header line. A line is considered to be a header if in rows
#' 2,3, 4 and 5 it contains non-numeric, non-blank data. If 2 potential header lines are
#' found consecutively, the upper line is checked for known meaningless headers (V1-9, X1-9
#' ...1-9), and if so the lower row is returned as header. Otherwise, or if more than 2
#' potential headers are detected, then a second approach is tried. The first five columns are checked
#'  for repeating data (these columns often include data such as site or date which can be the
#'  same for many rows). If repeating data is found (at least five repetitions), then the row
#'  immediately before the first instance of the repetitive data is considered to be the header
#'  line. If this fails, or if the dataframe has less than 5 columns then the string "no header"
#'  is returned. If multi_header == TRUE (if the file is anticipated to consist
#' of multiple dataframes on top of each other separated by header lines) then a list of row indices for
#' each header line is returned. If more than 10 headers are detected it is assumed to be erroneous and
#' and "no header" is returned. The repetitive string approach will not attempt to detect multi_headers
#' and so only the first header will be detected if this approach is required
#' @param dataframe The dataframe which is to be checked for the presence of a header line
#' within its data
#' @param search_limit The number of rows from the top to check for a header. Defaults to
#' 10. If the limit is set higher than the number of rows in dataframe then all rows are
#' checked
#' @param multi_header Is the file is anticipated to consist of multiple dataframes on top of each
#'  other separated by header lines, and should all headers be detected?
#' @export

search_dataframe_for_header <- function(dataframe, search_limit = 10, multi_header = FALSE){
    # return "no header" if dataframe has less than 4 columns. Such dataframes do not
    # have enough data to reliably determine headers
    # set numeric_result to NULL. If searching for numeric data is insufficient to differentiate between
    # the header line and the data then numeric_result will be set to FALSE
    numeric_result <- NULL
    if (length(dataframe) < 5){
        return("no header")
    }
    # Adjust search_limit if it is longer than the dataframe
    if (length(dataframe[[1]]) < search_limit){
        search_limit <- length(dataframe[[1]])
    }
    # Create a logical vector where TRUE indicates a row meets the header row definitions
    # and FALSE indicates it does not, for all rows within the search limit
    search_rows <- 1:search_limit
    is_header <- sapply(search_rows, FUN = function(dataframe, row){
        header_identifier(dataframe[[2]][row], dataframe[[3]][row], dataframe[[4]][row],
                          dataframe[[5]][row])
    }, dataframe = dataframe)

    if (sum(is_header) == 1){ # If one header is detected return the row number of the header line
        header_row <- which(is_header, arr.ind = TRUE)
        return(header_row)
    } else if (sum(is_header) == 0){ # If no header, set numeric_result to FALSE
        numeric_result <- FALSE
        #' If multi_header is set to TRUE, first check that there is not an unreasonable number of headers
        #' detected. If there are then it is likely data is being mistaken for header lines.
        #' Otherwise, check that each found header is not of a known meaningless format, and return an
        #' integer vector of the row numbers of all non-meaningless headers
    } else if (multi_header & sum(is_header) < 11){
        # The rows in the file for which headers have been detected
        header_rows <- which(is_header, arr.ind = TRUE)
        # The rows that have headers confirmed not to be nonsense
        confirmed_header_rows <- vector("integer")
        # For each detected header, confirm it is not in the form ...<num>, X<num>, or V<num>.
        # If header doesn't match nonsense then append to confirmed_header_rows
        for (row in header_rows){
            nonesense_header <- grepl("\\.\\.\\.[0-9]", dataframe[[2]][row]) |
                grepl("^ *X[0-9] *$", dataframe[[2]][row]) |
                grepl("^ *V[0-9] *$", dataframe[[2]][row])
            if (!nonesense_header){
                confirmed_header_rows <- append(confirmed_header_rows, row)
            }
        }
        return(confirmed_header_rows)
        # If more than 2 headers are detected it is likely that file contains multiple character
        # columns and thus header_identifier is unreliable. return "no header"
    } else  if (sum(is_header) > 2){
        numeric_result <- FALSE
    } else if (sum(is_header) == 1){ # If one header is detected return the row number of the header line
        header_row <- which(is_header, arr.ind = TRUE)
        return(header_row)
    } else if (sum(is_header) == 2){
        # If 2 header lines are detected, check if the top header matches known
        # meaningless headers. If so, return row number of lower header. Else,
        # return "no header"
        header_rows <- which(is_header, arr.ind = TRUE)
        #if ((header_rows[2] - header_rows[1]) != 1){return("no header")}
        nonesense_header <- grepl("\\.\\.\\.[0-9]", dataframe[[2]][header_rows[1]]) |
            grepl("^ *X[0-9] *$", dataframe[[2]][header_rows[1]]) |
            grepl("^ *V[0-9] *$", dataframe[[2]][header_rows[1]])
        if (nonesense_header){
            return(header_rows[2])
        } else {
            numeric_result <- FALSE
        }
    }
    # If trying to differentiate the header line from the data by looking for numeric and non_numeric
    # data didn't find a header, try searching for repeated strings. Often data will contain a site name,
    # plot number, date etc that is repeated for many entries and this can differentiate the data from the
    # header line.
    if (!is.null(numeric_result) & numeric_result == FALSE){
        # rows to check for repeated strings
        rows <- 1:10
        # return TRUE for every row where at least one of the columns 1:5 has a repeating
        # entry for the next 10 rows (inclusive), else FALSE
        is_header <- sapply(rows, function(dataframe, row){
            if (length(unique(dataframe[row:(row+5), 1][[1]])) == 1|
                length(unique(dataframe[row:(row+5), 2][[1]])) == 1|
                length(unique(dataframe[row:(row+5), 3][[1]])) == 1|
                length(unique(dataframe[row:(row+5), 4][[1]])) == 1|
                length(unique(dataframe[row:(row+5), 5][[1]])) == 1){
                return(TRUE)
            } else {
                return(FALSE)
            }
        }, dataframe = dataframe)
        # If at least one column was found with repeating data, and it didn't start repeating
        # from the first row (which would suggest the data starts from the first row and so no
        # header), then return the row index for the expected header, else return "no header".
        if (sum(is_header) > 0 & which(is_header == TRUE)[1] != 1){
            return(which(is_header == TRUE)[1] - 1)
        } else {
            return("no header")
        }
    } else {
        return("no header")
    }
}
# A function to check if three given cells all contain non-numeric data and are not blank.
# If so, returns TRUE, else FALSE
header_identifier <- function(cell_1, cell_2, cell_3, cell_4){
    # If cells 1,2 and 3 contain data
    if (!is.na(cell_1) & !is.na(cell_2) & !is.na(cell_3) & !is.na(cell_4) &
        !grepl("^ *$", cell_1) & !grepl("^ *$", cell_2) &
        !grepl("^ *$", cell_3) & !grepl("^ *$", cell_4)){
        # And if all that data is not numeric then return TRUE else FALSE
        cell_1 <- as.numeric(cell_1)
        cell_2 <- as.numeric(cell_2)
        cell_3 <- as.numeric(cell_3)
        cell_4 <- as.numeric(cell_4)
        if (is.na(cell_1) & is.na(cell_2) & is.na(cell_3) & is.na(cell_4)){
            return(TRUE)
        } else {return(FALSE)}
    } else {
        return(FALSE)
    }
}
