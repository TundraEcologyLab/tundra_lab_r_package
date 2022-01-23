#' Standardise Sites
#'
#' A function which accepts a dataframe, standardises the names found within the column headed
#' "site", and returns that dataframe. If the name of the dome site is subset with "d" or "g",
#' the letter is appended to the plot number. If any site is identified as belonging to Anne then
#' "Annex_" is prepended to the site name if not already present.
#'  The sites which are currently attempted to be standardised are:
#' Dome
#' Sax_opp,
#' Fert,
#' Beach_Ridge,
#' Cassiope,
#' Willow,
#' Meadow,
#' Dryas,
#' Annex_Willow,
#' Annex_Cassiope,
#' Annex_BS_Willow,
#' Annex_BS_Cassiope,
#' Annex_Migration,
#' Baker_Lake,
#' Fosheim,
#' Claude/Tower,
#' Princess.Marie.Bay,
#' Sverdrup.Pass
#' @param dataframe The dataframe, which must contain a column named "site", which is to have it's
#' site names standardised
#' @export


standardise_sites <- function(dataframe){
    dataframe <- determine_annex(dataframe)
    dataframe <- dplyr::mutate(
        dataframe,
        # If site contains both "dome" and the letter d (not the d in dome) or g, then extract
        # the d or g and prepend to the plot number if the plot does not already include a
        # d or g subscript
        plot = ifelse(grepl("dome.*[dg]|[dg].*dome", .data[["site"]], ignore.case = TRUE),
                      ifelse(!grepl("^[0-9]{1,2}.?[dg]$|^[dg].?[0-9]{1,2}$", .data[["plot"]], ignore.case = TRUE),
                             paste0(.data[["plot"]],
                                    toupper(gsub(".*([dg])(?!ome).*",
                                                 "\\1",
                                                 .data[["site"]],
                                                 ignore.case = TRUE,
                                                 perl = TRUE)
                                    )),
                             as.character(.data[["plot"]])
                      ),
                      as.character(.data[["plot"]])
        ),
        site = ifelse(grepl("dome", x = .data[["site"]], ignore.case = TRUE),
                      "Dome", .data[["site"]]),
        site = ifelse(grepl("sax", x = .data[["site"]], ignore.case = TRUE),
                      "Sax_Opp", .data[["site"]]),
        site = ifelse(grepl("Fert", x = .data[["site"]], ignore.case = TRUE),
                      "Fert", .data[["site"]]),
        site = ifelse(grepl("beach", x = .data[["site"]], ignore.case = TRUE),
                      "Beach_Ridge", .data[["site"]]),
        site = ifelse(grepl("cas", x = .data[["site"]], ignore.case = TRUE)&
                          !grepl("an?nex", .data[["site"]], ignore.case = TRUE),
                      "Cassiope", .data[["site"]]),
        site = ifelse(grepl("will", x = .data[["site"]], ignore.case = TRUE)&
                          !grepl("an?nex", .data[["site"]], ignore.case = TRUE),
                      "Willow", .data[["site"]]),
        site = ifelse(grepl("mead|maed", x = .data[["site"]], ignore.case = TRUE)&
                          !grepl("an?nex", .data[["site"]], ignore.case = TRUE),
                      "Meadow", .data[["site"]]),
        site = ifelse(grepl("dry", x = .data[["site"]], ignore.case = TRUE)&
                          !grepl("an?nex", .data[["site"]], ignore.case = TRUE),
                      "Dryas", .data[["site"]]),
        site = ifelse(grepl("will", x = .data[["site"]], ignore.case = TRUE)&
                          grepl("an?nex", .data[["site"]], ignore.case = TRUE)&
                          !grepl("BS", .data[["site"]], ignore.case = TRUE),
                      "Annex_Willow", .data[["site"]]),
        site = ifelse(grepl("cas", x = .data[["site"]], ignore.case = TRUE)&
                          grepl("an?nex", .data[["site"]], ignore.case = TRUE)&
                          !grepl("BS", .data[["site"]], ignore.case = TRUE),
                      "Annex_Cassiope", .data[["site"]]),
        site = ifelse(grepl("will", x = .data[["site"]], ignore.case = TRUE)&
                          grepl("an?nex", .data[["site"]], ignore.case = TRUE)&
                          grepl("BS", .data[["site"]], ignore.case = TRUE),
                      "Annex_BS_Willow", .data[["site"]]),
        site = ifelse(grepl("cas", x = .data[["site"]], ignore.case = TRUE)&
                          grepl("an?nex", .data[["site"]], ignore.case = TRUE)&
                          grepl("BS", .data[["site"]], ignore.case = TRUE),
                      "Annex_Cassiope", .data[["site"]]),
        site = ifelse(grepl("migr", x = .data[["site"]], ignore.case = TRUE),
                      "Annex_Migration", .data[["site"]]),
        site = ifelse(grepl("baker", x = .data[["site"]], ignore.case = TRUE),
                      "Baker_Lake", .data[["site"]]),
        site = ifelse(grepl("PMB|Princess|Marie", x = .data[["site"]], ignore.case = TRUE),
                      "Princess.Maire.Bay", .data[["site"]]),
        site = ifelse(grepl("Fosh", x = .data[["site"]], ignore.case = TRUE),
                      "Fosheim", .data[["site"]]),
        site = ifelse(grepl("claude|tower", x = .data[["site"]], ignore.case = TRUE),
                      "Claude", .data[["site"]]),
        site = ifelse(grepl("sverdrup", x = .data[["site"]], ignore.case = TRUE),
                      "Sverdrup.Pass", .data[["site"]]),
    )
}

#' A function which first checks every row of the dataframe to see if "anne" can be found within
#' any of its columns. If so that row is marked as belonging to an Annex site. All rows deemed to
#' be belonging to annex sites in this way have "Annex_" prepended to their site names if they don't
#' already have so. If running standardise_plots, the standardise_plot_number function will later
#' identify cassiope and willow sites as annex if "new" is found within plot column.
determine_annex <- function(dataframe){
    # determine list of columns
    cols <- names(dataframe)
    # Create a column to store whether or not a row contains a reference to Anne
    dataframe <- dplyr::mutate(dataframe, is_annex = NA)
    # Iterate through columns, changing the value of is_annex to TRUE if "anne's plots or annex"
    # is found in any column
    for (name in cols){
        # Anne's name appears in many file paths so do not check the column file
        if (name != "file"){
            dataframe <- dplyr::mutate(dataframe, is_annex = ifelse(grepl("anne.?.?.?plot|annex", .data[[name]], ignore.case = TRUE),
                                                                    TRUE, is_annex))
        }
    }
    # Add FALSE to any is_annex value that isn't TRUE to remove NA values
    dataframe <- dplyr::mutate(dataframe, is_annex = ifelse(is.na(.data[["is_annex"]]), FALSE, is_annex))

    # Prepend "Annex_" to every identified annex site that is not already so identified
    dataframe <- dplyr::mutate(dataframe,
                               site = ifelse(!grepl("ann", .data[["site"]], ignore.case = TRUE)&
                                                 .data[["is_annex"]] == TRUE,
                                             paste0("Annex_", .data[["site"]]), .data[["site"]]))
    # Remove annex test from dataframe
    dataframe <- dplyr::select(dataframe, !is_annex)
    dataframe
}


