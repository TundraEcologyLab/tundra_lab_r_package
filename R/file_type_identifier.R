#' Determine if a file is CSV or Tab deliminated
#'
#' A function which takes a file_path to a file and attempts to determine if this file
#' is either CSV or Tab delimited. It eliminates either possibility if
#' 1) more than 30% of rows are predicted to have only 1 column
#' 2) less than 90% of rows are predicted to have more than 3 columns
#' 3) There are predicted to be more than 5 different row lengths
#' At each test, if ambiguous, CSV is favoured. If all tests fail, file is predicted to
#' be CSV
#' @param file_path A path to the file to be analysed
#' @export


file_type_identifier <- function(file_path){
    # Set spaces to a vector of integers counting the number of columns predicted
    # per line if the separator is a space. Occasionally, when checking the wrong
    # delimiter, the quoting becomes confused and the error "quoted string on line
    # 1 terminated by EOF" is thrown by the count.fields function. The counting is
    # performed within a try statement to avoid this problem.
    spaces <- NA
    try(spaces <- count.fields(file_path, quote = '""'), silent = TRUE)
    # Set commas to a vector of integers counting the number of columns predicted
    # per line if the separator is a comma
    commas <- NA
    try(commas <- count.fields(file_path, sep = ",", quote = '""'), silent = TRUE)

    # Return an error if count.fields returns an error for both spaces and commas. If only
    # one separator returns an error then return the other
    if (is.na(spaces) & is.na(commas) & length(spaces) == 1 & length(commas) == 1){
        stop("count.fields failed when applied to this file testing for both comma and tab
             delimination. This is likely due to unusual use of quotes in the file. You can
             try manually setting the sep variable.")
    } else if (is.na(spaces) & length(spaces) == 1){
        return(",")
    } else if (is.na(commas) & length(commas) == 1){
        return("")
    }

    #remove NA values from both spaces and commas. NAs are generated if carriage returns
    # are encountered within a field
    spaces <- spaces[!is.na(spaces)]
    commas <- commas[!is.na(commas)]

    # Set not_spaces and not_commas to FALSE
    not_spaces <- FALSE
    not_commas <- FALSE

    # If the number of columns predicted is 1 at least 30% of the time, set not_spaces or
    # not_commas to TRUE
    if (length(spaces[spaces == 1])/length(spaces) > 0.3){
        not_spaces <- TRUE
    }
    if (length(commas[commas == 1])/length(commas) > 0.3){
        not_commas <- TRUE
    }

    # If not_spaces, predict CSV. If not_commas and not_spaces remains TRUE, predict
    # Tab deliminated
    if (not_spaces) {
        return(",")
    }
    if (!not_spaces & not_commas){
        return("")
    }

    # If the number of columns predicted is not > 3 at least 90% of the time, set
    # not_spaces or not_commas to TRUE
    if (length(spaces[spaces > 3])/length(spaces) < 0.9){
        not_spaces <- TRUE
    }
    if (length(commas[commas > 3])/length(commas) < 0.9){
        not_commas <- TRUE
    }
    # If not_spaces, predict CSV. If not_commas and not_spaces remains TRUE, predict
    # Tab deliminated
    if (not_spaces) {
        return(",")
    }
    if (!not_spaces & not_commas){
        return("")
    }

    # Reduce spaces and commas to their unique values
    spaces <- unique(sort(spaces))
    commas <- unique(sort(commas))

    # If there are more than 5 different lengths of row, set
    # not_spaces or not_commas to TRUE
    if (length(spaces) > 5){
        not_spaces <- TRUE
    }
    if (length(commas) > 5){
        not_commas <- TRUE
    }
    # If not_spaces, predict CSV. If not_commas and not_spaces remains TRUE, predict
    # Tab deliminated
    if (not_spaces) {
        return(",")
    }
    if (!not_spaces & not_commas){
        return("")
    }
    # If it was not possible to determine file type, predict CSV
    return(",")
}
