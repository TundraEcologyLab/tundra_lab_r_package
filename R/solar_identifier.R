#' Identify which columns of a dataframe contain solar data
#'
#' A function which accepts a dataframe and returns a logical vector with a value for
#' each of its columns determining if it contains solar data or not. The dataframe must
#' have a date column named "date" as its last column. This can be added using the add_date
#' function. The function can be instructed to look for data in W/m2 or KW/m2 through the
#' W_or_KW parameter. Data is determined to be solar data if it passes 3 tests:
#' 1) data is between 0 and 1 for KW/m2, 0 and 1000 for W/m2
#' 2) All winter data reads 0
#' 3) All summer data fits a sine curve with period of 1 year
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
#' @export

solar_identifier <- function(dataframe, file_path, output_dir, W_or_KW = "KW"){
    is_solar <- vector("logical")
    # set upper limit to 1 or 1000, depending on the units expected
    if (W_or_KW == "W"){
        upper_limit <- 1000
        effective_zero <- 1
    } else {
        upper_limit <- 1
        effective_zero <- 0.01
    }
    # convert all column types to numeric, except the date column, which should be last
    dataframe <- dataframe %>% convert(num(names(dataframe[1:(length(dataframe) - 1)])))
    # For all non-date columns, decide if column contains solar data by checking the data
    # lies in an appropriate range, equals 0 in the winter, and fits a sinosoidal curve
    # through the summer
    for (i in 1:(length(dataframe) - 1)){
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

        # Check that for the summer months, the data fits a sine curve with a period of one year
        summer_months <- dataframe_i %>%
            dplyr::filter(!(lubridate::month(date) %in% c(10, 11, 12, 1, 2, 3)))
        # If summer month period is too short to evaluate, and there is insufficient winter month data
        # to determine if data is solar take note of file and move on
        if (length(summer_months[[i]]) < 30 &
                   length(winter_months[[i]]) < 30){
            is_solar <- append(is_solar, FALSE)
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
            next
            }
            next
        }
        # Fewer than 30 data points is deemed too low to reliably check if data is sinusoidal
        if(length(summer_months[[i]]) > 30){
            # A check to further eliminate implausible data
            if (median(summer_months[[i]]) < upper_limit/10){
                is_solar <- append(is_solar, FALSE)
                next
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
                is_solar <- append(is_solar, FALSE)
                next
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
            next
        }
        # If all tests have been passed, append TRUE to is_solar
        is_solar <- append(is_solar, TRUE)


    }
    # append FALSE to is_solar, for the final column which contains the date data
    is_solar <- append(is_solar, FALSE)
    is_solar
}
