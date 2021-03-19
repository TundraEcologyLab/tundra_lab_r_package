#' Identify which column of a dataframe contains year data
#'
#' A function which analyses every column in dataframe and returns a logical vector where
#' the value true indicates the corresponding column contains a list of years.
#' A column is identified as containing year data if, ignoring all 6999 values, NAs,
#' non-numeric data, and the 1st and last quartiles, the data ranges from 1980-2030
#' and there are at least 10 data points for every unique value identified.
#' @param dataframe A dataframe which is to be analysed for the presence of a Year column
#' @export
year_identifier <- function(dataframe){
    is_year <- vector("logical")
    for (i in 1:length(dataframe)){
        col <- dataframe[[i]]
        col <- col[!grepl("6999", col)]
        col <- as.numeric(col)
        col <- col[!is.na(col)]
        if (length(col) > 0){
            # squeezed range is the range missing the first and last quartiles
            squeezed_range <- unname(quantile(col))[c(2,4)]
            # unique ratio is a measure of how little the variable changes. There should be
            # many results per year, so a low unique ratio is expected.
            unique_ratio <- length(unique(col))/length(col)
            if (squeezed_range[1] > 1980 &&
                squeezed_range[2] < 2030 && # 2030 was chosen to future proof the script
                unique_ratio < 0.1 ||
                squeezed_range[1] > 1980 &&
                squeezed_range[2] < 2030 &&
                squeezed_range[1] == squeezed_range[2]){is_year <- append(is_year, TRUE)}
            else {is_year <- append(is_year, FALSE)}
        } else {is_year <- append(is_year, FALSE)}
    }
    is_year
}
