#' Determine the Mode of a set of data
#' 
#'   A function to return the most repeated value in a set i.e. the mode. This function
#' can be applied to both numeric and categorical data. If there are multiple values
#' sharing the maximum number of repetitions then the first of these values is returned.
#' @param data A vector containing the data that should have its mode determined
#' @export

Mode <- function(data){
  # All the unique values contained in the data
  unique_values <- unique(data)
  # set the mode that shall be returned to NA
  mode <- NA
  # Set the number of occurrences of the mode to 0
  mode_reps <- 0
  # For each unique value in the data, determine the number of repetitions of this
  # value. If the number of repetitions is greater than the highest found so far then
  # set the mode to this value and mode_reps to the number of times it is repeated in 
  # the data
  for (value in unique_values){
    value_reps <- length(data[data == value])
    if (value_reps > mode_reps){
      mode <- value
      mode_reps <- value_reps
    }
  }
  mode
}