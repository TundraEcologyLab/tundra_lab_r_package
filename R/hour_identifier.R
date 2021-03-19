#' Identify which column of a dataframe contains time data
#'
#' A function which accepts a dataframe that is to have its columns analysed to determine
#' if they contain time data or not, as well as optional logical vectors detailing which
#' columns contain day data and year data. If these are not provided then they will be generated
#' using the day_identifier and year_identifier functions.
#' hour identifier determines if a column contains time data by first ensuring 90% of the
#' values fit the format XX0 or YXX0, where Y is a digit between 0 and 2, and X is any
#' numeral. If the appropriate format is found, then the data is checked to see if all
#' hours (0-23) are present. Only the first column containing time data is identified as
#' a time column. Only the first 8 columns are investigated. A list is returned. The first
#' entry of which is a logical vector indicating which column contains time data. The second
#' entry is the dataframe modified to contain a decimalised fractional day column
#' @param dataframe A dataframe to be checked for the presence of time data
#' @param is_day An optional logical vector indicating which column contains the day data.
#' If not provided it is generated using the day_identifier function
#' @param is_year An optional logical vector indicating which column contains the day data.
#' If not provided it is generated using the year_identifier function
#' @export


hour_identifier <- function(dataframe, is_day = NULL, is_year = NULL){
    # If no is_day data provided, generate using is_day function
    if (is.null(is_day)){
        is_day <- day_identifier(dataframe)
    }
    # If no is_year data provided, generate using is_year function
    if (is.null(is_year)){
        is_year <- year_identifier(dataframe)
    }
    is_hour <- vector("logical")
    # Set the search_range to 8 or length of dataframe, whichever is shorter
    if (length(dataframe) < 8){
        search_range <- length(dataframe)
    } else {
        search_range <- 8
    }
    # Set column index to 1
    i <- 1
    # While no time column has been identified and the search_range has not yet been
    # exceeded, continue to analyse columns
    while((length(is_hour) == 0 | sum(is_hour) == 0) &  i < search_range){
        # Set col to next column in dataframe
        col <- dataframe[[i]]
        # Determine what percentage of data in col are in a time consistent format
        correct_format_ratio <- sum(grepl("^[0-2]?[0-9][0-5]0$", col))/length(col)
        # If at least 90% of data is in an appropriate format
        if(correct_format_ratio > 0.9){
            # ensure all times are in 4 digit format (eg 930 becomes 0930)
            col <- ifelse(grepl("^.[0-5][0-9]$", col), paste0("0", col), col)
            # create a datetime vector containing the datetime in ymd_hm format using
            # the day and year data stored in dataframe
            datetime <- lubridate::ymd_hm(paste0(dataframe[is_year][[1]], "0101", col))
            datetime <- lubridate::`yday<-`(datetime, as.numeric(dataframe[is_day][[1]]))
            hour <- lubridate::hour(datetime)
            hour <- unique(sort(hour))
            # If all possible hours are represented in the datetime data append TRUE to
            # is_hour, add a fractional_day column to dataframe, and extend the length of
            # is_day and is_year to reflect the addition of the new column. Else, append
            # FALSE to is_hour
            if (length(hour) > 23){
                is_hour <- append(is_hour, TRUE)
                dataframe <- dplyr::mutate(dataframe, fractional_day = lubridate::yday(datetime) +
                                        (lubridate::hour(datetime) + lubridate::minute(datetime)/60)/24)
                is_day <- append(is_day, FALSE)
                is_year <- append(is_year, FALSE)

            } else {
                is_hour <- append(is_hour, FALSE)
            }
        # If data was not in the required format, append FALSE to is_hour
        } else {
            is_hour <- append(is_hour, FALSE)
        }
        # Move to next column of dataframe
        i <- i + 1
    }
    # Extend is_hour to the full length of the dataframe if the initial dataframe was
    # longer than 8 columns
    if (length(dataframe) - length(is_hour) > 0){
    is_hour <- append(is_hour, rep(FALSE, length(dataframe) - length(is_hour)))
    }
    # Return a list containing both the logical vector is_hour, and the dataframe updated
    # with a fractional day column
    is_hour <- list(is_hour, dataframe)
    is_hour
}
