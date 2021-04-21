#' return first value from a vector which isn't NA
#' A function which accepts a vector and returns the first value which isn't NA.
#' Most notably used by the clean_data function to retain non NA data where available
#' @param values A vector containing the values to be searched for non NA data
#' @export


non_NA <- function(values){
  value <- values[!is.na(values)]
  if (length(value) == 0){
    value <- NA
  }
  value[[1]]
}
