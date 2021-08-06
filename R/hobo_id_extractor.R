#' extract hobo id from string
#'
#' A function which accepts a character vector and returns all unique instances within that string
#' that identify a hobo serial number. If no sites are found then it returns NA. Setting unique to FALSE means
#' all hits are returned, which is useful for counting them. If multiple sites are present within one
#' string then an error is returned
#' @param string A character vector that is to be examined for sites
#' @param unique Should result return all unique hits or a full list for counting occurrences
#' @export

hobo_id_extractor <- function(string, unique = TRUE){
    # If only one string has been passed to the function attempt to extract a hobo ID, else call
    # this function separately for each string
    if (length(string) == 1){
        # If the string contains a hobo ID then store it in id, else set id to NA
        if (grepl("S.?N.?:? ?[0-9]{6,8}", string)){
            # Extract hobo id with leading identifier "SN:"
            id_plus <- grep("S.?N.?:? ?[0-9]{6,8}", string, value = TRUE)
            # eliminate the leading identifier from id_plus
            id <- sub(".*S.?N.?:? ?([0-9]{6,8}).*", "\\1", id_plus)
        } else {
            id <- NA
        }
    } else if (length(string) > 1){
        # Call this function for every string in string
        id <- lapply(string, hobo_id_extractor)
        id <- unlist(id)
    } else {
        id <- NA
    }
    # Return only unique values if unique == TRUE
    if (unique == TRUE){
        id <- unique(id)
    }
    id
}
