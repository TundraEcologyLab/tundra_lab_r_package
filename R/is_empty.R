#' Is a variable NA, Null, or an empty string
#'
#' A simple function returning the answer to the logical statement
#' is.na(var) | is.null(var) | var == ""
#' @param var The Variable to be examined
#' @export

is.empty <- function(var){
  is_empty <- is.na(var) | is.null(var) | var == "" | var == "NA"
  is_empty
}
