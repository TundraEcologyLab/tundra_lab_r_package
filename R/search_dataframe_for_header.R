#' A function which takes a dataframe as input, and returns the row number where it
#' has detected a posible header line. A line is considered to be a header if in rows
#' 2,3, and 4 it contains non-numeric, non-blank data. If 2 potential header lines are
#' found consecutively, the upper line is checked for known meaningless headers (V1-9, X1-9
#' ...1-9), and if so the lower row is returned as header. Otherwise, or if more than 2
#' potential headers are detected or dataframe has less than 4 columns, then the character
#'  string "no header" is returned.
#' @param dataframe The dataframe which is to be checked for the presence of a header line
#' within its data
#' @param search_limit The number of rows from the top to check for a header. Defaults to
#' 10. If the limit is set higher than the number of rows in dataframe then all rows are
#' checked
#' @export
search_dataframe_for_header <- function(dataframe, search_limit = 10){
  # return "no header" if dataframe has less than 4 columns. Such dataframes do not
  # have enough data to reliably determine headers
  if (length(dataframe) < 4){
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
    header_identifier(dataframe[[2]][row], dataframe[[3]][row], dataframe[[4]][row])
  }, dataframe = dataframe)
  # If more than 2 headers are detected it is likely that file contains multiple character
  # columns and thus header_identifier is unreliable. return "no header"
  if (sum(is_header) == 0 | sum(is_header) > 2){
    return("no header")
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
      return("no header")
    }
  }
}
# A function to check if three given cells all contain non-numeric data and are not blank.
# If so, returns TRUE, else FALSE
header_identifier <- function(cell_1, cell_2, cell_3){
  # If cells 1,2 and 3 contain data
  if (!is.na(cell_1) & !is.na(cell_2) & !is.na(cell_3) &
      !grepl("^ *$", cell_1) & !grepl("^ *$", cell_2)& !grepl("^ *$", cell_3)){
    # And if all that data is not numeric then return TRUE else FALSE
    cell_1 <- as.numeric(cell_1)
    cell_2 <- as.numeric(cell_2)
    cell_3 <- as.numeric(cell_3)
    if (is.na(cell_1) & is.na(cell_2) & is.na(cell_3)){
      return(TRUE)
    } else {return(FALSE)}
  } else {
    return(FALSE)
  }
}
