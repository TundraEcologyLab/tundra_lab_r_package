#' Create list of all identifiers found in identifier column of a dataframe
#'
#' A function which accepts a dataframe and a logical vector identifying each column
#' of the dataframe as either an identifier column or not. The function returns a list
#' of the identifiers found within the column identified by is_identifier. If is_identifier
#' indicates no identifier column is present then the function returns NULL
#' @param dataframe A dataframe which contains the identifier column to be inspected
#' @param is_identifier A logical vector with a value for each column in dataframe indicating
#' if it contains identifier data
#' @export


determine_identifiers <- function(dataframe, is_identifier){
    if (sum(is_identifier) > 0){
        identifiers <- unique(sort(unlist(dataframe[is_identifier], use.names = FALSE)))
    } else {
        identifiers <- NULL
    }
    identifiers
}
