#' Standardise Plot Number
#'
#' A function which accepts a dataframe and returns it having standardised it's plot numbering.
#' First, if a column named sect (section) is present containing letters A-C, then the section is
#' appended to the plot number, before removing sect column. This is relevant for the BS_Annex sites.
#' At this point, if A cassiope or Willow site contains A-C in its plot number, then it is ensured the
#' site is named Annex_BS.
#' Next, any plot number containing A, R, or C and not belonging to an Annex or Farm site, has
#' "addition", "removal", or "control" added to its snow_treatment column. Finally, any plot not
#' belonging to an Annex, Farm, or Dome site has any letters removed from its plot number. Column name
#' standardisation should be performed before using this function. The required column names are:
#' "site"
#' "plot"
#' "sect"
#' "otc_treatment"
#' "snow_treatment" - This column will be added if it is not already present - however,
#' find_treatment_cols should be applied to the dataframe before the standardisation of plot number.
#' @param dataframe A dataframe that should have its plot numbers standardised
#' @export
#'

standardise_plot_number <- function(dataframe){
    # Ensure that if any of the 3 columns needed to run the standardise_plot_number function are missing
    # that they are added, filled NA
    needed_cols <- c("co2_plot", "otc_treatment", "snow_treatment")
    for (needed_col in needed_cols){
        if (!needed_col %in% names(dataframe)){
            dataframe <- dplyr::mutate(dataframe, "{needed_col}" := NA)
        }
    }
    # determine if there is a section column. If there is, and that column contains letters A-C, append
    # the letters to the plot numbers in the plot column and then remove the section column
    sect_col <- which(names(dataframe) == "sect", arr.ind = TRUE)
    if (length(sect_col) == 1){
        dataframe <- dplyr::mutate(dataframe,
                                   plot = ifelse(grepl("^[ABC]$", .data[["sect"]], ignore.case = TRUE),
                                                 paste0(.data[["plot"]], .data[["sect"]]),
                                                 .data[["plot"]]))
        dataframe <- dplyr::select(dataframe, !.data[["sect"]])
    }

    # determine if the plot number may contain otc_treatment info. i.e, if it contains non numeric data

    dataframe <- dplyr::mutate(
        dataframe, .has_otc = grepl("[A-Z]",
                                        .data[["plot"]], ignore.case = TRUE))
    dataframe <- dplyr::ungroup(dataframe)

    dataframe <- dplyr::mutate(
        dataframe,
        # If the plot number may contain otc information and no otc_information is otherwise provided,
        # then it is extracted into the otc_treatment column. For beach_ridge, extract into snow_treatment
        otc_treatment = ifelse(.has_otc == TRUE &
                                   site != "Beach_Ridge" &
                                   (is.na(otc_treatment) | otc_treatment == ""),
                               gsub("\\d", "", .data[["plot"]]),
                               .data[["otc_treatment"]]
                               ),
        snow_treatment = ifelse(.has_otc == TRUE &
                                   site == "Beach_Ridge" &
                                   (is.na(snow_treatment) | snow_treatment == ""),
                               gsub("\\d", "", .data[["plot"]]),
                               .data[["snow_treatment"]]
        ),
        # Ensure that if CO2 information is stored within co2_plot column that it is moved to its own
        # column
        co2_plot = ifelse(grepl("co2", .data[["plot"]], ignore.case = TRUE), "Y", .data[["co2_plot"]]),
        # Ensure that any Cassiope or Willow site with letters A-C contained within
        # their plot number are identified as an Annex_BS site, unless the c is from co2 or they contain
        # otc information
        site = ifelse(grepl("cass|will", .data[["site"]], ignore.case = TRUE)&
                          grepl("[A-C]", .data[["plot"]], ignore.case = TRUE)&
                          !grepl("co2", .data[["plot"]], ignore.case = TRUE) &
                          .has_otc == FALSE,
                      sub(".*(Cassiope|Willow).*", "Annex_BS_\\1",
                          .data[["site"]], ignore.case = TRUE),
                      .data[["site"]]),
        # Ensure that any Cassiope or willow site labeled with "new" in the plot number is
        # identified as a cover plot
        otc_treatment = ifelse(grepl("cass|will", .data[["site"]], ignore.case = TRUE)&
                          grepl("new", .data[["plot"]], ignore.case = TRUE),
                      "cover",
                      .data[["otc_treatment"]]),
        # Ensure letter in dome plots is right side of the plot number and upper case with no spacer
        plot = ifelse(grepl("dome", .data[["site"]], ignore.case = TRUE)&
                          grepl("^[dg].?[0-9]{1,2}$", .data[["plot"]], ignore.case = TRUE),
                      toupper(sub("([dg]).?([0-9]+)", "\\2\\1", .data[["plot"]], ignore.case = TRUE)),
                      ifelse(grepl("dome", .data[["site"]], ignore.case = TRUE)&
                                 grepl("^[0-9]{1,2}.?[dg]$", .data[["plot"]], ignore.case = TRUE),
                             toupper(sub("([0-9]{1,2}).?([dg])", "\\1\\2", .data[["plot"]], ignore.case = TRUE)),
                             .data[["plot"]])),


        # Ensure that if snow treatment information was stored within the plot number that
        # this information is moved to the snow_treatment column
        snow_treatment = ifelse(grepl("A", .data[["plot"]], ignore.case = TRUE)&
                                    !grepl("annex|farm", .data[["site"]], ignore.case = TRUE)&
                                    .has_otc == FALSE &
                                    is.na(.data[["snow_treatment"]]),
                                "addition", .data[["snow_treatment"]]),
        snow_treatment = ifelse(grepl("R", .data[["plot"]], ignore.case = TRUE)&
                                    !grepl("annex|farm", .data[["site"]], ignore.case = TRUE)&
                                    .has_otc == FALSE &
                                    is.na(.data[["snow_treatment"]]),
                                "removal", .data[["snow_treatment"]]),
        snow_treatment = ifelse(grepl("C", .data[["plot"]], ignore.case = TRUE)&
                                    !grepl("annex|farm", .data[["site"]], ignore.case = TRUE)&
                                    .has_otc == FALSE &
                                    is.na(.data[["snow_treatment"]]),
                                "control", .data[["snow_treatment"]]),)
    # Remove all letters from plot numbers, except for Annex_BS, Farm, or Dome sites which
    # justifiably contain such characters
    dataframe <- dplyr::mutate(
        dataframe,
        # remove co2 from plot number. Must be removed separately or the 2 remains
        plot = ifelse(grepl("co2", .data[["plot"]], ignore.case = TRUE),
                      gsub("co2", "", .data[["plot"]], ignore.case = TRUE),
                      .data[["plot"]]),
        plot = ifelse(!grepl("annex_BS|farm|dome|fert", .data[["site"]], ignore.case = TRUE)&
                          grepl("[^0-9]+", .data[["plot"]], ignore.case = TRUE),
                      gsub("[^0-9]", "", .data[["plot"]], ignore.case = TRUE),
                      .data[["plot"]])
    )
    # remove the .has_otc column from dataframe
    dataframe <- dplyr::select(dataframe, !.has_otc)

    dataframe
}
