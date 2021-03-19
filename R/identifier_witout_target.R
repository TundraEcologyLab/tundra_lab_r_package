#' Identify which column of a dataframe contains identifier data without specifying a specific
#' identifier
#'
#' A function to identify the identifier column without looking for a specific identifier number.
#' The identifier column contains a numerical value that describes the type of data to expect
#' on that row. A column is determined to be an identifier column if at least 80% of its values
#' consist of only either 2 or 3 digits, and contains no more than ten unique values and is not
#' a day column. Only the first 5 columns are checked for efficiency.
#' The function returns a logical vector with TRUE/FALSE values for every column in dataframe.
#' @param dataframe A dataframe that should be analysed for the presence of an identifier column.
#' @param output_dir The directory to which a text file will be written identifying any files
#' for which multiple possible identifier columns were detected. If this happens, only the first
#' result is taken.
#' @param file_path The path to the file containing the data loaded into dataframe. Needed for
#' making the text file described in output_dir
#' @export
identifier_without_target <- function(dataframe, output_dir, file_path){
    is_identifier <- vector("logical")
    # The identifier column always occurs near the start of the file. Check only the first
    # five columns, or length of the dataframe, whichever is smaller
    if (length(dataframe) < 5){
        search_range <- length(dataframe)
    } else {
        search_range <- 5
    }
    # For each column to be checked for identifier data, check if the ratio of values in that column
    # which consist of either 2 or 3 digits only is at least 80%. Also check that the data contains less
    # than 10 unique values and is not a day column. If so, append TRUE to is_identifier, else FALSE.
    for (i in 1:search_range){
        if (sum(grepl('(^|,)"?[0-9][0-9][0-9]?"?(,|$)', dataframe[[i]]))/length(dataframe[[i]]) < 0.8) {
            is_identifier <- append(is_identifier, FALSE)
            next}
        unique_total <- length(unique(sort(dataframe[[i]])))
        if (unique_total > 10 | day_identifier(dataframe[[i]])){
            is_identifier <- append(is_identifier, FALSE)
        } else {
            is_identifier <- append(is_identifier, TRUE)}
    }

    # If multiple columns are identified as identifier columns, take only the one with the
    # lowest index, but write the path to file for manual validation
    identifier_indexes <- which(is_identifier == TRUE, arr.ind = TRUE)
    if (length(identifier_indexes) > 1){
        readr::write_lines(file_path,
                    paste0(output_dir, "/multiple_potential_identifier_columns.txt"),
                    append = TRUE,
                    sep = "\n")
    }
    # ensure that is_identifier has a logical value for every column in dataframe, and there
    # is at most one TRUE value, indicating the first column meeting the is_identifier criteria.
    is_identifier <- rep(FALSE, length(dataframe))
    if (length(identifier_indexes) > 0){
        is_identifier[identifier_indexes[1]] <- TRUE
    }

    is_identifier
}
