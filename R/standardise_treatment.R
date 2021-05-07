#' Standardise Treatment
#'
#' A function which accepts a dataframe and returns it with the contents of its treatment columns
#' standardised. find_treat_cols should be applied to the dataframe before treatment is standardised.
#'
#' For each treatment column present, each term is examined for the presence of "CO2" in its name.
#' If "CO2" is part of the treatment descriptor then, "Y" is added to the column "co2_plot" (this column
#' is created if no CO2 plot already exists, or renamed if a CO2 column does exist with a different name).
#' If an annex_BS site has its plot letters ABC stored within the treatment column this is moved to
#' the plot number column and removed from treatment.
#' Finally, the treatment descriptors are renamed so that they are all written the same. The accepted
#' terms are:
#' OTC,
#' cover
#' control
#' addition
#' removal
#' @param dataframe A dataframe containing columns named "site", "plot", and at least one column
#'with name matching regex "otc|treat" containing treatment data which is to be standardised
#' @param file_path A file path for the original data. Used in the output of a warning message if
#' snow and otc treatment data could not be clearly separated.
#' @param output_dir The directory that a warning file will be written to if snow and otc treatment
#' data can not be clearly separated.
#' @export


standardise_treatment <- function(dataframe){
    # Known treatment names for otc, cover, control, addition, and removal. If additional terms are
    # found they should be added to the appropriate list to ensure the function continues to work
    # correctly. In each case, the name is standardised to the first entry in the vector.
    OTC <- c("OTC", "T", "W", "W(CO2)", "CO2_T", "OTC (CO2)")
    cover <- c("cover", "Cover", "Cc")
    control <- c("control", "Pheno", "C", "Cp", "CO2_C", "Control", "Control (CO2)", "Control(CO2)")
    addition <- c("addition", "snow addition", "A")
    removal <- c("removal", "snow removal", "R")

    # Each of the treatment vectors are combined in one list so they can be iterated through later
    treatment_list <- list(OTC, cover, control, addition, removal)


    # If no CO2 column is present, create one called co2_plot. If one is present, ensure it is called
    # co2_plot
    if (sum(grepl("CO2", names(dataframe), ignore.case = TRUE)) > 0){
        CO2_name <- grep("CO2", names(dataframe), ignore.case = TRUE, value = TRUE)
        dataframe <- dplyr::rename(dataframe, co2_plot = .data[[CO2_name]])
    } else {
        dataframe <- dplyr::mutate(dataframe, co2_plot = NA)
    }
    # Find the names of all treatment columns present
    treatment_names <- grep("treatment", names(dataframe), ignore.case = TRUE, value = TRUE)
    # for each treatment column first extract any co2 information to a co2 column, then standardise the
    # treatment terms
    for (name in treatment_names){
        # Append BS plot data to BS plot numbers if this was instead recorded in the treatment column
        dataframe <- dplyr::mutate(
            dataframe,
            plot = ifelse(grepl("BS", .data[["site"]], ignore.case = TRUE) &
                              grepl("[A-C]", .data[[name]], ignore.case = TRUE)&
                              !grepl("[A-C]", .data[["plot"]], ignore.case = TRUE),
                          paste0(.data[["plot"]], .data[[name]]),
                          .data[["plot"]]),
            # Remove the BS plot data from the treatment column if present
            "{name}" := ifelse(
                grepl("BS", .data[["site"]], ignore.case = TRUE) &
                    grepl("[A-C]", .data[[name]], ignore.case = TRUE),
                NA, .data[[name]]))
        # if CO2 is present in a treatment term ensure co2_plot indicates "Y"
        dataframe <- dplyr::mutate(dataframe,
                                   co2_plot = ifelse(grepl("CO2", .data[[name]], ignore.case = TRUE),
                                                     "Y", .data[["co2_plot"]]))
        # For each treatment type in treatment list, find all examples of each treatment and ensure they
        # are all named as the first example in that treatment vector
        for (treat in treatment_list){
            dataframe <- dplyr::mutate(dataframe, "{name}" := ifelse(.data[[name]] %in% treat,
                                                                     treat[1], .data[[name]]))
        }
    }
    dataframe
}
