#' Standardise plot column names
#'
#' A function which accepts a dataframe, and returns it with applicable column names
#' standardised to:
#' site
#' plot
#' otc_treatment
#' snow_treatment
#' fert_treatment
#' co2_plot
#' The output_dir and file_path of the original data are utilised by the find_treatment_cols
#' function to produce a warning file if it has to split a column containing a mix of snow and
#' otc treatment
#' @param dataframe The dataframe which is to have its column names standardised
#' @param output_dir The directory to which warning text files may be produced
#' @param file_path The file_path which will be used to identify the data in the warning
#' text files
#'
#' @export

standardise_plot_col_names <- function(dataframe, output_dir, file_path){

    # Ensure site and plot are written in lower case
    names(dataframe)[grepl("^site$", names(dataframe), ignore.case = TRUE)] <- "site"
    names(dataframe)[grepl("^plot$", names(dataframe), ignore.case = TRUE)] <- "plot"
    # Ensure columns with co2 in the name, and also contain strings of "Y" or "y" are named
    # co2_plot
    names(dataframe) <- unname(sapply(names(dataframe), function(dataframe, name){
        if (grepl("co2", name, ignore.case = TRUE)){
            if (sum(c("y", "Y") %in% dataframe[[name]]) > 0 |
                length(dataframe[[name]][!is.na(dataframe[[name]])]) == 0){
                name <- "co2_plot"
            }

        }
        name
    },
    dataframe = dataframe
    ))

    # Find all treatment columns and ensure they are named otc_treatment, snow_treatment,
    # and fert_treatment as appropriate
    dataframe <- find_treatment_cols(dataframe, output_dir, file_path)
    dataframe
}
