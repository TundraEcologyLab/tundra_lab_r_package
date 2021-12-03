#' Identify which columns of a dataframe contain solar data
#'
#' A function which accepts a dataframe and returns a logical vector with a value for
#' each of its columns determining if it contains solar data or not. The dataframe must
#' have either a date column named "date" or a Day and Year column (and hour if performing hourly
#' analysis). The function can be instructed to look for data in W/m2 or KW/m2 through the
#' W_or_KW parameter. If hourly_data is set to FALSE then the function checks for daily data, else
#' it checks for data recorded hourly. Data is determined to be solar data if it passes 3 tests:
#' 1) data is between 0 and 1 for KW/m2, 0 and 1000 for W/m2
#' 2) All winter data reads 0
#' 3) All summer data fits a sine curve with period of 1 year for daily data or a sine curve
#' with a period of one day for hourly data
#' Data just before or after winter is difficult to analyse. If more than 80% of data is
#' within this range, the file path is written to file in output_dir for manual inspection
#' @param dataframe A dataframe to be inspected for solar data. Must have a date column as its
#' last column. This can be added using the add_date function
#' @param file_path The path to the file from which the data was written. This file path will
#' be written to output_dir/cant_determine.txt if 80% or more of the data was from Oct, Feb, Mar
#' @param output_dir The directory that the file cant_determine.txt will be written to if
#' a file is found with 80% of its data from Oct, Feb, Mar, which can not be ruled out as not
#' solar data
#' @param W_or_KW A character vector of either "W" or "KW" to permit the setting of the units
#' expected. Defaults to KW/m2 if no value is given
#' @param hourly_data TRUE, will search for data recorded hourly, FALSE will search for data recorded
#' daily
#' @export

solar_identifier <- function(dataframe, file_path, output_dir, W_or_KW = "KW", hourly_data = FALSE){
    is_solar <- vector("logical")
    # set upper limit to 1 or 1000, depending on the units expected
    if (W_or_KW == "W"){
        upper_limit <- 1000
        effective_zero <- 1
    } else {
        upper_limit <- 1
        effective_zero <- 0.01
    }
    # A count of all columns added to the dataframe by this function for the purposes of detecting
    # solar data (these columns will be lost when the function concludes). The count ensures that
    # no results are returned for columns not part of the original dataframe.
    added_columns <- 0
    # Determine which column contains date data by checking column names
    date_index <- which(grepl("date", names(dataframe), ignore.case = TRUE), arr.ind = TRUE)
    # If only one date column is present check that it is in the date format then set good_date to
    # TRUE else FALSE
    if (length(date_index) == 1){
        good_date <- lubridate::is.Date(dataframe[[date_index]])
    } else{
        good_date <- FALSE
    }
    # If no date column is detected, or there are multiple making it unclear then create one based
    # on the detected year and day data. If the one detected date column can not be confirmed to be
    # in a date format then create a new date column
    if (!good_date){
        # change the name of any date column which is not to be used as date information by this
        # function from "date" to ".date", so that the add_date function does not overwrite a
        # column already called "date". This function does not return the dataframe, so no change
        # in name will be permanent.
        for (index in date_index){
            names(dataframe)[index] <- paste0(".", names(dataframe[index]))
        }
        # Add a date column at the end of the dataframe
        dataframe <- tundra::add_date(dataframe)
        added_columns <- added_columns + 1
        date_index <- length(dataframe)
    }
    # If hourly data is to be examined create a decimalised day column combining day and hour data
    if (hourly_data){
        # determine which columns contain hour and day data
        is_day <- tundra::day_identifier(dataframe)
        hour_analysis <- tundra::hour_identifier(dataframe)
        is_hour <- hour_analysis[[1]]
        dataframe <- hour_analysis[[2]]
        # # isolate the number of minutes from the hour data
        # minutes <- dataframe[is_hour]/100 - trunc(dataframe[is_hour]/100)
        # # convert 24 hour clock to decimalised number of hours
        # fractional_hour <- trunc(dataframe[is_hour]/100) + minutes*100/60
        # # calculate decimalised day from day + hours
        # fractional_day <- dataframe[is_day] + dataframe[is_hour]/24
        # # Add fractional day to the dataframe
        # dataframe <- dplyr::mutate(dataframe, fractional_day = fractional_day)
        # # record that a column was added to the dataframe so that it can be excluded from analysis
        # added_columns <- added_columns + 1
    }
    # convert all column types to numeric, except the date column
    dataframe <- dataframe %>% hablar::convert(
        hablar::num(!dplyr::matches("^date$", ignore.case = TRUE)))
    # For all non-date columns, decide if column contains solar data by checking the data
    # lies in an appropriate range, equals 0 in the winter, and fits a sinosoidal curve
    # through the summer
    for (i in 1:(length(dataframe) - added_columns)){
        # skip date column
        if (i == date_index){
            is_solar <- append(is_solar, FALSE)
            next
        }
        # remove error values and NAs
        # R decided after running this script quite happily 314 times that, on time 315
        # it couldn't subset the dataframe with the grepl statement within the subset command.
        # For reasons beyond reason defining it earlier (but in exactly the same way) fixed the
        # problem
        # Dataframe_i, is a dataframe containing every column of the original dataframe passed
        # to this function, but with every row that contained NA or 6999 in column i removed
        not_errors <- !grepl("6999", dataframe[[i]])
        dataframe_i <- subset(dataframe, not_errors)

        not_nas <- !is.na(dataframe_i[[i]])
        dataframe_i <- subset(dataframe_i, not_nas)

        # If there are no values left column had no solar data. This might happen if
        # the column contained only character data which became NA on conversion to numeric
        # at the start of the function
        if (length(dataframe_i[[i]]) == 0){
            is_solar <- append(is_solar, FALSE)
            next}
        # Dropping the first and last quartiles to prevent outliers derailing analysis,
        # check that range is appropriate for solar data (between 0 and 1 KW/m2)
        squeezed_range <- unname(quantile(dataframe_i[[i]], na.rm = TRUE))
        if (is.na(squeezed_range[2]) |
            is.na(squeezed_range[4]) |
            squeezed_range[2] < 0 |
            squeezed_range[4] > upper_limit |
            # Some files contain columns containing only 0's, which can appear like solar data
            # Also, for shorter summer files one number repeated consistently fits a sinusoidal curve
            (squeezed_range[1] == squeezed_range[5]) |
            # The day column can look sinusoidal for short summer files
            day_identifier(dataframe_i[i])) {
            is_solar <- append(is_solar, FALSE)
            next}

        # Check that over the winter months the reading was near zero at least 80% of the time
        winter_months <- dataframe_i %>% dplyr::filter(lubridate::month(date) %in% c(11, 12, 1))
        if(length(winter_months[[i]]) > 0){
            is_zero_ratio <- length(winter_months[[i]][winter_months[[i]] <
                                                           effective_zero])/length(winter_months[[i]])
            if(is_zero_ratio < 0.75){
                is_solar <- append(is_solar, FALSE)
                next}
        }

   if (!hourly_data){
       solar_found <- daily_solar(dataframe_i, output_dir, file_path, i, winter_months, upper_limit)
       if (!solar_found){
           is_solar <- append(is_solar, FALSE)
           next
       }
   } else {
       solar_found <- hourly_solar(dataframe_i, output_dir, file_path, i, upper_limit)
       if (!solar_found){
           is_solar <- append(is_solar, FALSE)
           next
       }
   }
    # If all tests have been passed, append TRUE to is_solar
    is_solar <- append(is_solar, TRUE)


    }

    is_solar
}

daily_solar <- function(dataframe_i, output_dir, file_path, i, winter_months, upper_limit){
    # Check that for the summer months, the data fits a sine curve with a period of one year
    summer_months <- dataframe_i %>%
        dplyr::filter(!(lubridate::month(date) %in% c(10, 11, 12, 1, 2, 3)))
    # If summer month period is too short to evaluate, and there is insufficient winter month data
    # to determine if data is solar take note of file and move on
    if (length(summer_months[[i]]) < 30 &
        length(winter_months[[i]]) < 30){

        # The time periods entering and leaving winter are hard to diagnose and so are ignored
        # in the analysis. If more than 80% of the data pertains to these periods, write the
        # file path to disk for manual inspection
        difficult_months <- dataframe_i %>%
            dplyr::filter(lubridate::month(date) %in% c(10,2,3))
        difficult_percent <- 100*length(difficult_months[[i]])/length(dataframe_i[[i]])
        if (difficult_percent > 80){
            readr::write_lines(paste0("% difficult months : ",
                                      difficult_percent,
                                      "file_path, : ", file_path),
                               file = paste0(output_dir, "cant_determine.txt"),
                               sep = "\n",
                               append = TRUE)
            return(FALSE)
        }
        return(FALSE)
    }
    # Fewer than 30 data points is deemed too low to reliably check if data is sinusoidal
    if(length(summer_months[[i]]) > 30){

        # On occasion (at least 2007 on Claude) a broken sensor records 0. This erroneous
        # data masks the good data collected after the sensor was fixed.
        # A logical vector detailing every row where column i has a value above zero
        summer_above_zero <- summer_months[[i]] > 0
        non_zero_summer_data <- subset(summer_months, summer_above_zero)
        if (length(non_zero_summer_data[[1]]) > 30){
            summer_months <- non_zero_summer_data
        }
        # A check to further eliminate implausible data
        if (median(summer_months[[i]]) < upper_limit/10){
            return(FALSE)
        }
        # Apply a regression model against a sinusoidal model with period of one year
        model <- lm(summer_months[[i]] ~
                        sin(2*pi*(365*lubridate::year(summer_months$date) +
                                      lubridate::yday(summer_months$date))/365) +
                        cos(2*pi*(365*lubridate::year(summer_months$date) +
                                      lubridate::yday(summer_months$date))/365))
        # Check that r2 is at least 0.5
        r2 <- summary(model)$r.squared
        if(is.na(r2) | (!is.na(r2) & r2 < 0.5)){
            return(FALSE)
        }
    }
    # The time periods entering and leaving winter are hard to diagnose and so are ignored
    # in the analysis. If more than 80% of the data pertains to these periods, write the
    # file path to disk for manual inspection
    difficult_months <- dataframe_i %>% dplyr::filter(lubridate::month(date) %in% c(10,2,3))
    difficult_percent <- 100*length(difficult_months[[i]])/length(dataframe_i[[i]])
    if (difficult_percent > 80){
        readr::write_lines(paste0("% difficult months : ",
                                  difficult_percent,
                                  "file_path, : ",
                                  file_path),
                           file = paste0(output_dir, "cant_determine.txt"),
                           sep = "\n",
                           append = TRUE)
        return(FALSE)
    }
    TRUE
}

hourly_solar <- function(dataframe_i, output_dir, file_path, i, upper_limit){
    # Check that the summer months fit a sine curve of period one day, if summer month data exists
    summer_months <- dataframe_i %>% filter(!(month(date) %in% c(10, 11, 12, 1, 2, 3)))
    # A check to further eliminate implausible data
    if (median(summer_months[[i]]) < upper_limit/10){
        return(FALSE)
    }
    if(length(summer_months[[i]]) > 0){
        # perform regression on a sine curve with period 1 day
        model <- lm(summer_months[[i]] ~ sin((2*pi*summer_months$fractional_day)) +
                        cos((2*pi*summer_months$fractional_day)))
        # Check if r^2 is greater than 0.3, else return FALSE (indicating no solar data detected)
        r2 <- summary(model)$r.squared
        if(r2 < 0.3){
            return(FALSE)
        }
    }
    # The months of little light do not produce data which fit a sine curve and is difficult to
    # analyse. If data is predominantly from this period then make a note of the file in
    # cant_determine.txt and skip the file
    difficult_months <- dataframe_i %>% filter(month(date) %in% c(10,2,3))
    difficult_percent <- 100*length(difficult_months[[i]])/length(dataframe_i[[i]])
    if (difficult_percent > 80){
        write_lines(paste0(file_path, ": ", difficult_percent, "% difficult months"),
                    file = paste0(output_dir, "cant_determine.txt"),
                    sep = "\n",
                    append = TRUE)
        return(FALSE)
    }
    TRUE
}
