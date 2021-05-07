#' Standardise Sites
#'
#' A function which accepts a dataframe, standardises the names found within the column headed
#' "site", and returns that dataframe. The sites which are currently attempted to be standardised are:
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
#' Annex_Migration
#' @param dataframe The dataframe, which must contain a column named "site", which is to have it's
#' site names standardised
#' @export


standardise_sites <- function(dataframe){
    dataframe <- dplyr::mutate(dataframe,
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
                                             "Annex_Migration", .data[["site"]]))
}
