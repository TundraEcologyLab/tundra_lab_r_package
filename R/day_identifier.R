#'Identify a column containing Day data within a dataframe
#'
#' A function which analyses every column in dataframe and returns a logical vector where
#' the value true indicates the corresponding column contains a list of days (1-366).
#' A column is identified as containing day data if, ignoring the 1st and last quartiles to avoid
#' being thrown by errors, the data ranges from 30 - 366, as well as containing data that, at least 90%
#' of the time increases by exactly 1 between unique values.
#'
#' @param dataframe A dataframe which will be analysed to determine if its columns contain day of year data
#' @export
day_identifier <- function(dataframe){
  is_day <- vector("logical")
  # For each column in dataframe append TRUE to is_day if the column is a list of days
  # and FALSE if it isn't
  for (i in 1:length(dataframe)){
    col <- dataframe[[i]] # set col to next column in dataframe
    col <- as.numeric(col) # convert to class numeric to remove all non-numeric data
    col <- col[!is.na(col)] # remove all NA values
    # if col contained numeric data, examine it to deterine if it is a list of days
    if (length(col) > 0){
      # Squeezed range is the range minus the 1st and last quartiles. This is a conservative
      # method of checking the range that will not permit some rouge data preventing a hit.
      squeezed_range <- unname(quantile(col, na.rm = TRUE))[c(2,4)]
      # Remove repeated data
      col <- unique(col)
      # The calculation for steps will fail if there are not at least two unique values in col
      # Also, there is at least one example of 2 identifier numbers occurring sequentially and thus
      # looking like a day column. It is highly unlikely this will happen 5 times sequentially.
      if (length(col) < 5){
        is_day <- append(is_day, FALSE)
        next
      }
      # Subtract the values of col from itself, offset by one, to determine the step change
      # going from one value to the next
      steps <- col[2:length(col)] - col[1:(length(col) - 1)]
      # Calculate the proportion of step changes in col that are exactly 1.
      one_steps_ratio <- sum(steps == 1)/length(steps)
      # append TRUE to is_day if the 2nd and 3rd quartiles range from 30-366, and the one step
      # ratio is a numeric greater than 0.9, else append FALSE
      if (squeezed_range[1] > 30 &&
          squeezed_range[2] < 366 &&
          !is.na(one_steps_ratio) &&
          one_steps_ratio > 0.9){
        is_day <- append(is_day, TRUE)
      } else {is_day <- append(is_day, FALSE)}
    } else {is_day <- append(is_day, FALSE)}
  }
  # There should only be one day column. If more than one is found then data has likely been
  # miss-attributed. A likely cause is a column which merely counts the row number (present in
  # many of the data files), which always increases by 1 and is generally the first column of a sheet.
  # Given that the data rarely starts at the beginning of a calender year, if more than one day column
  # is detected, discard column one, then a column starting at 1, if this is only one of multiple columns.
  if (sum(is_day) > 1){is_day[1] = FALSE} # If more than one column discard column one
  if (sum(is_day) > 1){ # If still more than one column find how many begin at 1
    starts_at_one <- vector("integer")
    # For every column in dataframe identified as a Day column, check that the first value in column
    # is not NA and is == 1. If so, append i (the count of columns identified as Day, not the index
    # is_day) to starts_at_one
    for (i in 1:sum(is_day)){
      if (!is.na(dataframe[is_day][[i]][1]) &&
          dataframe[is_day][[i]][1] == 1){starts_at_one <- append(starts_at_one, i)}
    }
    # If exactly one of the multiple columns identified as day begins with one, discard it
    # If there are more than one then return them all, uncertainty too great to choose
    if (length(starts_at_one) == 1){
      # For every value in is_day, sequentially sum all values to that index, until the
      # nth value corresponding to starts_at_one is reached. Set that value to FALSE
      for (i in 1:length(is_day)){
        if (sum(is_day[1:i]) == starts_at_one){
          is_day[i] = FALSE
          break
        }
      }
    }
  }
  is_day
}
