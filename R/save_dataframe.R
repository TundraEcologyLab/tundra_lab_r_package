#' Save a dataframe with standard naming format, avoiding overwrite potential
#'
#' A function which takes a dataframe and writes it to file. It names the file with
#' a name provided in the name parameter in the format <name>_FIRST-YEAR-LAST-YEAR.csv, if
#' the file spans multiple years, or <name>_YEAR_FIRST-DAY-LAST-DAY.csv if it doesn't.
#' If a file with the same name already exists, an iterator is added to the end. Files
#' are written to the output directory. Year and Day data should be identified by logical vectors
#' indicating which columns of the dataframe contain Year and Day of year data. If these
#' logical vectors are not provided they are generated using the functions year_identifier
#' and day_identifier.
#' @param dataframe A dataframe to be saved
#' @param output_dir The directory to receive the saved file
#' @param name The name for the file, which will be augmented with year and day data
#' @param is_year A logical vector indicating which column of the dataframe contains year
#' data. If absent, will be generated using function year_identifier
#' @param is_day A logical vector indicating which column of the dataframe contains day
#' data. If absent, will be generated using function day_identifier
#' @export


save_dataframe <- function (dataframe, output_dir, name){
    # Determine the position of the year and Day data within dataframe
    is_year <- year_identifier(dataframe)
    is_day <- day_identifier(dataframe)

    if (length(is_year) > 0 & sum(is_year) > 0){ #If a year column has been identified
        years <- dataframe[is_year][[1]] # obtain years from dataframe
        years <- as.numeric(years)
        years <- years[!is.na(years)]
        first_year <- min(years)
        last_year <- max(years)
    } else {
        first_year <- "unknown_year"
        last_year <- "unknown_year"
    }
    # If the file spans more than one year name file: <name>_YEAR1-YEAR2.csv
    if (first_year != last_year){
        file_name <- paste0(output_dir, "/", name, "_", as.character(first_year),
                            "-", as.character(last_year), ".csv")
    } else {
        # If the data is all from one year, determine the first and last day and name file
        # <name>_YEAR_day_DAY1-DAY2.csv
        days <- dataframe[is_day][[1]]
        days <- as.numeric(days)
        days <- days[!is.na(days)]
        first_day <- days[1]
        last_day <- days[length(days)]
        file_name <- paste0(output_dir, "/", name, "_", as.character(first_year),
                            "_day_", as.character(first_day), "-", as.character(last_day), ".csv")
    }
    # If file name already exists in directory, iterate with count such that file is
    # now named file_name_count.csv
    count <- 1
    original_file_name <- file_name
    while (file.exists(file_name)){
        file_name <- sub("\\.csv", paste0("_", count, ".csv"), original_file_name)
        count <- count + 1
    }
    readr::write_csv(dataframe, file_name)
}
