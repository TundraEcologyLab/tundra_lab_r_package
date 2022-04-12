#' Extract tomst ID from a string
#'
#' A function which accepts a character vector and returns a tomst ID, if present in the string.
#' A tomst ID is defined as a 8 digit number starting with a 9.  If no IDs are found then NULL is
#' returned, unless as_null is set to FALSE, then NA will be returned. Setting unique to FALSE means
#' all hits are returned, which is useful for counting them. Set as_null to FALSE if trying to fill
#' a dataframe with the results.
#' @param string A character vector that is to be examined for tomst IDs
#' @param unique Should result return all unique hits or a full list for counting occurrences
#' @param as_null If TRUE then NULL is returned if there is no hits, which is good for counting.
#' If False, then NA is returned if no hits, good for working with dataframes
#' @export
tomst_id_extractor <- function(string, unique, as_null = TRUE){
    # If there is only one string then perform extraction, else call this function on each
    # string individually
    if (length(string) == 1){
        # Extract a 8 digit number starting with a 9 if present from string
       id <- grep_capture("(?:^|[^0-9])(9[0-9]{7})(?:[^0-9]|$)", string)

       # If no id was found return either NULL or NA depending on as_null
       if (length(id) != 1){
           if (as_null){
               id <- NULL
           } else {
               id <- NA
           }
       }
        # If multiple strings were provided then call plot_name_extractor separately for each of them
    } else if(length(string) > 1){
        id <- lapply(string, tomst_id_extractor, unique = unique, as_null = as_null)
        id <- unlist(id)
    } else {
        if (as_null){
            id <- NULL
        } else {
            id <- NA
        }
    }
    # If unique is set to TRUE then return only the unique results
    if (unique == TRUE){
        id <- unique(id)
    }
    id
}

