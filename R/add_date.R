#' Add a date column to a dataframe containing Year and Day columns
#'
#' A function which accepts a dataframe, a logical vector indicating which column of
#' the dataframe contains day data, and a logical vector indicating which column of
#' the dataframe contains year data. If no logical vectors are provided then they will
#' be produced using the day_identifier and year_identifier functions.
#' The day of year and Year data are used to produce a new column in the dataframe named
#' "date", using the lubridate package. The new dataframe, with appended date column is returned
#' @param datafrane A dataframe to which a date column should be added
#' @param is_day A logical vector with a value for every column in dataframe indicating if
#' the column contains day of year data. If not provided, then will attempt to generate it using
#' the is_day function
#' @param is_year A logical vector with a value for every column in dataframe indicating if
#' the column contains year data. If not provided, then will attempt to generate it using
#' the is_year function
#' @export

add_date <- function(dataframe, is_day = NULL, is_year = NULL){
    # if not provided, produce a logical vector indicating which column contains day of year data
    if (is.null(is_day)){
        is_day <- day_identifier(dataframe)
    }
    # if not provided, produce a logical vector indicating which column contains year data
    if (is.null(is_year)){
        is_year <- year_identifier(dataframe)
    }
    # Create a vector of dates with the years identified in dataframe
    date <- lubridate::ymd(paste0(dataframe[is_year][[1]], "0101"))
    # ammend date to accuratly reflect the day data contained in dataframe
    date <- lubridate::`yday<-`(date, as.numeric(dataframe[is_day][[1]]))
    # Add date column at end of dataframe
    dataframe <- dataframe %>% dplyr::mutate(date = date)
    dataframe
}
