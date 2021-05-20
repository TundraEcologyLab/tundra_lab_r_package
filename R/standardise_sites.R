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
#' Baker_Lake
#' @param dataframe The dataframe, which must contain a column named "site", which is to have it's
#' site names standardised
#' @export


standardise_sites <- function(dataframe){
    dataframe <- determine_annex(dataframe)
    dataframe <- dplyr::mutate(
        dataframe,
        plot = ifelse(grepl("dome.*[dg]|[dg].*dome", .data[["site"]], ignore.case = TRUE),
                      paste0(.data[["plot"]],
                             toupper(gsub(".*([dg])(:?[^o].*|$)",
                                          "\\1",
                                          .data[["site"]],
                                          ignore.case = TRUE)
                             )),
                      .data[["plot"]]),
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
                      "Baker_Lake", .data[["site"]]))
}

#' A function which first checks every row of the dataframe to see if "ann" can be found within
#' any of its columns. If so that row is marked as belonging to an Annex site. All rows deemed to
#' be belonging to annex sites in this way have "Annex_" prepended to their site names if they don't
#' already have so.
determine_annex <- function(dataframe){
# determine list of rows of dataframe
    rows <- 1:length(dataframe[[1]])
# Itterate through rows, greping for "ann" across all columns. Return TRUE for every row
    # resulting in at least one hit, else FALSE
    is_annex <- sapply(rows, label_annex,
           dataframe = dataframe)
    # Add annex test to the dataframe
    dataframe["test_for_annex"] <- is_annex
    # Prepend "Annex_" to every identified annex site that is not already so identified
    dataframe <- mutate(dataframe,
                        site = ifelse(!grepl("ann", .data[["site"]], ignore.case = TRUE)&
                                          .data[["test_for_annex"]] == TRUE,
                                      paste0("Annex_", .data[["site"]]), .data[["site"]]))
    # Remove annex test from dataframe
    dataframe <- select(dataframe, !test_for_annex)
    dataframe
}

# Return TRUE if the dataframe in given row contains "ann" across any of its columns
label_annex <- function(dataframe, row){
    annex_plot <- FALSE
    row_data <- unname(unlist(dataframe[row,]))
    if (sum(grepl("anne", row_data, ignore.case = TRUE)) > 0){
        annex_plot <- TRUE
    }
    annex_plot
}
