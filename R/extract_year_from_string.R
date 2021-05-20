#' extract year from string
#'
#' A function which accepts a character vector and returns all unique instances within that string
#' that match a year format. If no years are found then it returns NULL. Accepted formats include:
#' 4 digits not directly next to any other digits
#' 8 digits fitting year-month-day, day-month-year, or month-day-year formats.
#' Certain dates will throw multiple years as they can be read in different formats, for example
#' 20062005 will return 2006 and 2005 because it fits both 20/jun/2005 and 20/may/2006. This can be
#' prevented by selecting a date format, which ensures all dates are read in only one way. Regardless
#' the date format, 4 digit years not directly touching other digits will always be returned. To further
#' prevent miss-atributation, only years 1980-2029 will be returned. Setting breaks to TRUE means that
#' only years isolated from other words are returned, for example "summer2012" would not produce a hit
#' but "summer_2012" or simply "2012" would. Setting unique to FALSE means all hits are returned, which
#' is useful for counting them. WARNING - if unique is set to FALSE, and a specific date format is not
#' selected, then extra hits may be generated, for example 01022003 will produce two hits, one as first
#' of February and a second as the second of January.
#' @param string A character vector that is to be examined for years
#' @param date_format which date formats will be checked. One of "all", "ymd" (year-month-day),
#' "dmy" (day-month-year), or "mdy" (month-day-year). Defaults to all. "four" only checks for
#' 4 digit years
#' @param breaks Should results be restricted to years encapsulated by word boundaries
#' @param unique Should result return all unique hits or a full list for counting occurrences
#' @export


year_extractor <- function(string, date_format = "all", breaks = FALSE, unique = TRUE){
    # regex statements, each return year named
    regex_list <- list()
    # Finds four digits corresponding to a year 1980-2029 with no immediately pre or pro-ceeding digits
    regex_list <- append(regex_list,
                         year_by_self <- "(?=(?<year>[12][01289][0-9][0-9]))(?![0-9]{5})(?<![0-9])")
    # Finds 8 digits starting with a year 1980-2029 and followed by month and year,
    # not preceeded by a digit
    if (date_format == "all" | date_format == "ymd"){
        regex_list <- append(regex_list,
                             year_month_day <- paste0(
                                 "(?=(?<year>[12][01289][0-9][0-9])(?:0[1-9]|1[0-2])",
                                 "(?:[0-2][0-9]|3[01]))(?![0-9]{9})(?<![0-9])")
        )
    }
    # as above but day-month-year format
    if (date_format == "all" | date_format == "dmy"){
        regex_list <- append(regex_list,
                             day_month_year <- paste0(
                                 "(?=(?:[0-2][0-9]|3[01])(?:0[1-9]|1[0-2])",
                                 "(?<year>[12][01289][0-9][0-9]))(?![0-9]{9})(?<![0-9])")
        )
    }
    # And month-day-year format
    if (date_format == "all" | date_format == "mdy"){
        regex_list <- append(regex_list,
                             month_day_year <- paste0(
                                 "(?=(?:0[1-9]|1[0-2])(?:[0-2][0-9]|3[01])",
                                 "(?<year>[12][01289][0-9][0-9]))(?![0-9]{9})(?<![0-9])")
        )
    }
    # If only years isolated from all words have to be returned, include boundary requirements
    # for all regex expressions
    if (breaks){
        # add "_" to the standard boundary definition
        extended_break <- '\\b|_'
        # Check that the found year/date is sandwitched by word boundaries or underscores
        breaks_regex <- paste0("(?<=",
                               extended_break,
                               ")(?=(?:.{8}(?:",
                               extended_break,
                               ")|.{4}(?:",
                               extended_break,
                               ")))"
        )
        # Add the breaks requirements to each regex expression
        regex_list <- lapply(regex_list, function(regex, breaks_regex){
            paste0(regex, breaks_regex)},
            breaks_regex = breaks_regex)
    }

    # Create an empty vector to contain all years found
    years <- NULL

    # Perform searches. Search_given_format appends the years it finds to the years variable if
    # one is provided
    if (length(string) < 2){

        years <- lapply(regex_list, search_given_format,
                        string = string, target = years)
        years <- unlist(years)
    } else {
        years <- lapply(string, year_extractor,
                        date_format = date_format,
                        breaks = breaks,
                        unique -unique)
        years <- unlist(years)
    }
    if (unique){
        # Remove duplicated years
        years <- unique(years)
    }
    years
}

#' Find Day of Year
#'
#' A function which accepts a character vector and returns all unique instances within that string
#' that match a day of year format. If no days are found then it returns NULL. If three_digits is
#' set to TRUE then it will match any valid 3 digit number (100-366), else it matches any valid day
#' (1-366).
#'
#' @param string A character vector that is to be examined for day of year
#' @param three_digits Should search be limited to 3 digit results
#' @export

DOY_extractor <- function(string, three_digits = FALSE, breaks = FALSE, unique = TRUE){
    # A regex expression fitting all numbers from 100-366
    three_digit <- "(?=(?<DOY>(?:[12][0-9]{2}|3[1-5][0-9]|36[0-6])))(?![0-9]{4})(?<![0-9])"
    # A regex expression fitting all numbers 1-366
    lower_digits <- "(?=(?<DOY>[1-9][0-9]?))(?![0-9]{3})(?<![0-9])"

    # An expression to include underscore within the definition of a word boundary
    extended_breaks <- "(?:\\b|_)"
    # A regex expression that finds the numbers 100-366 that are not connected to other words
    three_breaks <- paste0("(?=",
                           extended_breaks,
                           "(?<DOY>(?:[12][0-9]{2}|3[1-5][0-9]|36[0-6]))",
                           extended_breaks,
                           ")(?![0-9]{4})")

    # A regex expression that finds the numbers 1-366 that are not connected to other words
    lower_breaks <- paste0("(?=",
                           extended_breaks,
                           "(?<DOY>[1-9][0-9]?)",
                           extended_breaks,
                           ")(?![0-9]{3})")
    # Create a list of regex expressions which either do or don't respect word boundaries
    if (breaks){
        regex_list <- list(three_breaks, lower_breaks)
    } else {
        regex_list <- list(three_digit, lower_digits)
    }

    # If there are not multiple strings, perform search, else call this function individually on
    # each string present
    if (length(string) < 2){
        # Perform search for 3 digit long days with appropriate regex
        DOY <- search_given_format(regex_list[[1]], string)
        # If search is not to be limited to 3 digit hits then complete search for lower digits
        if (!three_digits){
            DOY <- search_given_format(regex_list[[2]], string, DOY)
        }
    } else {
        # Call this function on all strings included in string
        DOY <- lapply(string, DOY_extractor,
                      three_digits = three_digits,
                      breaks = breaks,
                      unique = unique)
        # unlist result
        DOY <- unlist(DOY)
    }
    # If unique is selected, only return unique results
    if (unique){
        DOY <- unique(DOY)
    }
    DOY
}



#' extract dates and components from within a string
#'
#' A function which accepts a character vector and attempts to identify dates within the string.
#' If no dates are found then it returns NULL. Accepted formats include:
#' year-month-day, day-month-year, or month-day-year, with or without separators between the date
#' components. date_day_month can be set to either "date", "day", or "month", defining whether the
#' function shall return the whole date, only the day, or only the month.
#' A date format can be specified resulting in only dates of this format producing results. If no
#' format is specified, then only days greater than 12 will be returned as it is otherwise not
#' possible to identify the day from the month. If searching for date components of an unknown format
#' the safest technique is to first extract the dates, then extract the day, and only proceed with
#' finding the month if a day is returned. To further prevent miss-atributation, only years
#' 1980-2029 will be returned. Setting breaks to TRUE means that
#' only dates isolated from other words are returned, for example "summer20120517" would not produce
#' a hit but "summer_20120517" or simply "20120517" would. Setting unique to FALSE means all hits are
#' returned, which is useful for counting them.
#' @param string A character vector that is to be examined for days
#' @param date_format which date formats will be checked. One of "all", "ymd" (year-month-day),
#' "dmy" (day-month-year), or "mdy" (month-day-year). Defaults to all.
#' @param breaks Should results be restricted to dates encapsulated by word boundaries
#' @param unique Should result return all unique hits or a full list for counting occurrences
#' @param date_day_month Should the whole date, only the day, or only the month be returned
#' @export
date_extractor <- function(string, date_format = "all", date_day_month = "date",
                           breaks = FALSE, unique = TRUE){
    # regex statements, each return year named
    regex_list <- list()

    # Finds 8 digits starting with a year 1980-2029 and followed by month and day,
    # not preceeded by a digit
    if (date_day_month == "date"){
        if (date_format == "all" | date_format == "ymd"){
            regex_list <- append(regex_list,
                                 year_month_day <- paste0(
                                     "(?:(?=(?<date>[12][01289][0-9][0-9](?:0[1-9]|1[0-2])",
                                     "(?:[0-2][0-9]|3[01])))(?![0-9]{9})(?<![0-9]))|",
                                     "(?:(?=(?<dateS>[12][01289][0-9][0-9](?:\\W|_)(?:0[1-9]|1[0-2])",
                                     "(?:\\W|_)(?:[0-2][0-9]|3[01])))(?![0-9]{11})(?<![0-9]))")
            )
        }
        # as above but day-month-year format
        if (date_format == "all" | date_format == "dmy"){
            regex_list <- append(regex_list,
                                 day_month_year <- paste0(
                                     "(?:(?=(?<date>(?:[0-2][0-9]|3[01])(?:0[1-9]|1[0-2])",
                                     "[12][01289][0-9][0-9]))(?![0-9]{9})(?<![0-9]))|",
                                     "(?:(?=(?<dateS>(?:[0-2][0-9]|3[01])(?:\\W|_)(?:0[1-9]|1[0-2])",
                                     "(?:\\W|_)[12][01289][0-9][0-9]))(?![0-9]{11})(?<![0-9]))")
            )
        }
        # And month-day-year format
        if (date_format == "all" | date_format == "mdy"){
            regex_list <- append(regex_list,
                                 month_day_year <- paste0(
                                     "(?:(?=(?<date>(?:0[1-9]|1[0-2])(?:[0-2][0-9]|3[01])",
                                     "[12][01289][0-9][0-9]))(?![0-9]{9})(?<![0-9]))|",
                                     "(?:(?=(?<dateS>(?:0[1-9]|1[0-2])(?:\\W|_)(?:[0-2][0-9]|3[01])",
                                     "(?:\\W|_)[12][01289][0-9][0-9]))(?![0-9]{11})(?<![0-9]))")
            )
        }
    } else if (date_day_month == "day"){
        if (date_format == "all" | date_format == "ymd"){
            regex_list <- append(regex_list,
                                 year_month_day <- paste0(
                                     "(?:(?=[12][01289][0-9][0-9](?:0[1-9]|1[0-2])",
                                     "(?<day>[0-2][0-9]|3[01]))(?![0-9]{9})(?<![0-9]))|",
                                     "(?:(?=[12][01289][0-9][0-9](?:\\W|_)(?:0[1-9]|1[0-2])",
                                     "(?:\\W|_)(?<day_sep>[0-2][0-9]|3[01]))(?![0-9]{11})(?<![0-9]))")
            )
        }
        # as above but day-month-year format
        if (date_format == "all" | date_format == "dmy"){
            regex_list <- append(regex_list,
                                 day_month_year <- paste0(
                                     "(?:(?=(?<day>[0-2][0-9]|3[01])(?:0[1-9]|1[0-2])",
                                     "[12][01289][0-9][0-9])(?![0-9]{9})(?<![0-9]))|",
                                     "(?:(?=(?<day_sep>[0-2][0-9]|3[01])(?:\\W|_)(?:0[1-9]|1[0-2])",
                                     "(?:\\W|_)[12][01289][0-9][0-9])(?![0-9]{11})(?<![0-9]))")
            )
        }
        # And month-day-year format
        if (date_format == "all" | date_format == "mdy"){
            regex_list <- append(regex_list,
                                 month_day_year <- paste0(
                                     "(?:(?=(?:0[1-9]|1[0-2])(?<day>[0-2][0-9]|3[01])",
                                     "[12][01289][0-9][0-9])(?![0-9]{9})(?<![0-9]))|",
                                     "(?:(?=(?:0[1-9]|1[0-2])(?:\\W|_)(?<day_sep>[0-2][0-9]|3[01])",
                                     "(?:\\W|_)[12][01289][0-9][0-9])(?![0-9]{11})(?<![0-9]))")
            )
        }
    } else if (date_day_month == "month"){
        if (date_format == "all" | date_format == "ymd"){
            regex_list <- append(regex_list,
                                 year_month_day <- paste0(
                                     "(?:(?=[12][01289][0-9][0-9](?<month>0[1-9]|1[0-2])",
                                     "(?:[0-2][0-9]|3[01]))(?![0-9]{9})(?<![0-9]))|",
                                     "(?:(?=[12][01289][0-9][0-9](?:\\W|_)(?<month_split>0[1-9]|1[0-2])",
                                     "(?:\\W|_)(?:[0-2][0-9]|3[01]))(?![0-9]{11})(?<![0-9]))")
            )
        }
        # as above but day-month-year format
        if (date_format == "all" | date_format == "dmy"){
            regex_list <- append(regex_list,
                                 day_month_year <- paste0(
                                     "(?:(?=(?:[0-2][0-9]|3[01])(?<month>0[1-9]|1[0-2])",
                                     "[12][01289][0-9][0-9])(?![0-9]{9})(?<![0-9]))|",
                                     "(?:(?=(?:[0-2][0-9]|3[01])(?:\\W|_)(?<month_split>0[1-9]|1[0-2])",
                                     "(?:\\W|_)[12][01289][0-9][0-9])(?![0-9]{11})(?<![0-9]))")
            )
        }
        # And month-day-year format
        if (date_format == "all" | date_format == "mdy"){
            regex_list <- append(regex_list,
                                 month_day_year <- paste0(
                                     "(?:(?=(?<month>0[1-9]|1[0-2])(?:[0-2][0-9]|3[01])",
                                     "[12][01289][0-9][0-9])(?![0-9]{9})(?<![0-9]))|",
                                     "(?:(?=(?<month_split>0[1-9]|1[0-2])(?:\\W|_)(?:[0-2][0-9]|3[01])",
                                     "(?:\\W|_)[12][01289][0-9][0-9])(?![0-9]{11})(?<![0-9]))")
            )
        }
    }

    # If only days isolated from all words have to be returned, include boundary requirements
    # for all regex expressions
    if (breaks){
        # add "_" to the standard boundary definition
        extended_break <- '\\b|_'
        # Check that the found date is sandwitched by word boundaries or underscores
        breaks_regex <- paste0("(?<=",
                               extended_break,
                               ")(?=(?:.{8}(?:",
                               extended_break,
                               ")|.{10}(?:",
                               extended_break,
                               ")))"
        )
        # Add the breaks requirements to each regex expression
        regex_list <- lapply(regex_list, function(regex, breaks_regex){
            paste0(regex, breaks_regex)},
            breaks_regex = breaks_regex)
    }

    # Create an empty vector to contain all days, months, or dates found
    component <- NULL

    # If multiple strings have been provided, run this function seperately on each, otherwise
    # perform search
    if (length(string) < 2){
        # Perform searches. Search_given_format appends the components it finds to the
        # components variable if one is provided
        component <- lapply(regex_list, search_given_format,
                            string = string, target = component)
        component <- unlist(component)
    } else {
        # re-call this function for each string in string individually
        component <- lapply(string, date_extractor,
                            date_format = date_format,
                            date_day_month = date_day_month,
                            breaks = breaks,
                            unique -unique)
        component <- unlist(component)
    }
    # If no date format was provided and the day is asked for, only return days greater than 12
    # to ensure no months are returned as days
    if (date_format == "all" & date_day_month == "day"){
        component <- as.numeric(component)
        component <- component[component > 12]
    }
    if (unique){
        # Remove duplicates
        component <- unique(component)
    }
    component
}
search_given_format <- function(regex, string, target = NULL){
    if (length(string) == 0){return()}
    if (is.na(string)){return()}
    # Perform grep search for given regex. Result is a list of attributes
    grep_result <- gregexpr(regex, string, perl = TRUE)
    # Extract the index for the starting character(s)
    target_starts <- attr(grep_result[[1]], "capture.start")
    target_length <- attr(grep_result[[1]], "capture.length")
    # If hit(s) were found, extract from each starting point till each end point
    for (i in seq_along(target_starts)){
        if (target_starts[i] != -1 & target_length[i] != 0){
            target <- append(target, substring(string, target_starts[i],
                                               target_starts[i] + target_length[i] - 1))
        }
    }
    target
}
