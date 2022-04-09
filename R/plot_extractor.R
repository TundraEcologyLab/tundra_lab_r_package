#' Extract plot information from a string
#' 
#' A function which accepts a character vector and returns a plot name, if present in the string.
#' Plot names consist of the plot number followed by an abbreviation for the treatment, for example
#' 1C, 11Cc, or 13OTC. This function does not work for the farm or fert sites, or for any data where
#' only the plot number is present unless the word "plot" directly precedes the plot name.  If no sites
#' are found then it returns NULL. Setting unique to FALSE means all hits are returned, which is useful
#' for counting them.
#' @param string A character vector that is to be examined for sites
#' @param unique Should result return all unique hits or a full list for counting occurrences
#' @param as_null If TRUE then NULL is returned if there is no hits, which is good for counting.
#' If False, then NA is returned if no hits, good for working with dataframes
#' @export

plot_name_extractor <- function(string, unique = TRUE, as_null = TRUE){
  # If there is only one string then perform extraction, else call this function on each
  # string individually
  if (length(string) == 1){
    # Set plot to NULL or NA depending on as_null
    if (as_null){
      plot <- NULL
    } else {
      plot <- NA
    }
    #' regex 1 matches plot names of format XT, XC, XW, XA, XR, XCc, XCp, where X is
    #' a one or two digit number. A space or _ may be present between number and treatment.
    #' regex 2 matches plot names of format XOTC, where X is a one or two digit number. 
    #' A space or _ may be present between number and treatment.
    #' regex 3 matches plot names of format Xctr, Xctl, where X is a one or two digit number.
    #' A space or _ may be present between number and treatment.
    #' regex 4-6 match the same as 1-3 except the number and treatment order is inverted
    #' regex 7 matches plot X, plot: X, plot_X, or plot:_X, where X is a one or two digit
    #' number, and extracts X if it is not in direct contact other alphanumeric characters
    regexes <- c("(?:[^0-9a-zA-Z]|^)([0-9]?[0-9][ _]?[TCWAR][cp]?)(?:[^0-9a-zA-Z]|$)",
                 "(?:[^0-9a-zA-Z]|^)([0-9]?[0-9][ _]?otc)(?:[^0-9a-zA-Z]|$)",
                 "(?:[^0-9a-zA-Z]|^)([0-9]?[0-9][ _]?ct[rl])(?:[^0-9a-zA-Z]|$)",
                 "(?:[^0-9a-zA-Z]|^)([TCWAR][cp]?[ _]?[0-9]?[0-9])(?:[^0-9a-zA-Z]|$)",
                 "(?:[^0-9a-zA-Z]|^)(otc[ _]?[0-9]?[0-9])(?:[^0-9a-zA-Z]|$)",
                 "(?:[^0-9a-zA-Z]|^)(ct[rl][ _]?[0-9]?[0-9])(?:[^0-9a-zA-Z]|$)",
                 "[Pp]lot:?[ _]?([0-9]?[0-9])(?:[^0-9a-zA-Z]|$)")
    # For each regex, if a hit is found then capture the plot name
    for (regex in regexes){
      if (grepl(regex, string, perl = TRUE, ignore.case = TRUE)){
        plot <- grep_capture(regex, string)
        break
      }
    }
    # If multiple strings were provided then call plot_name_extractor separately for each of them
  } else if(length(string) > 1){
    plot <- lapply(string, plot_name_extractor, unique = unique, as_null = as_null)
    plot <- unlist(plot)
  } else {
    if (as_null){
      plot <- NULL
    } else {
      plot <- NA
    }
  }
  # If unique is set to TRUE then return only the unique results
  if (unique == TRUE){
    plot <- unique(plot)
  }
  plot
}
