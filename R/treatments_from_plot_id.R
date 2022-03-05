#' Clean treatment columns by looking up plot IDs
#'
#' A function which accepts a dataframe with plot IDs and returns a dataframe with all
#' treatment columns (OTC, snow and fert), as well as the site and plot data completed based on
#' the information available for that plot in tundra::plot_names. If the plot ID can not be
#' identified then the treatment info is unchanged. The input dataframe must be in the format
#' produced by the standardise_plots function, in that it must have columns named "plot_id",
#' "otc_treatment", "snow_treatment", "fert_treatment", "site", and "plot".
#' @param dataframe The dataframe that is to have its treatment data completed based on
#' the plot_ids it already contains and the corresponding treatment info for those plots found
#' in tundra::plot_names
#' @export


treatments_from_plot_id <- function(dataframe){
    # Add row_number to dataframe to line up the results of lookup_treatment with the data
    dataframe <- dplyr::mutate(dataframe, .row = 1:nrow(dataframe))
    # Produce a dataframe containing the treatment info, and the corresponding row number in dataframe
    dataframe <- dplyr::group_by(dataframe, plot_id)
    treatments <- dplyr::group_modify(dataframe, ~ {data.frame(.row = .x$.row,
                                                               .lookup_otc = rep(lookup_treatment(.y$plot_id)$otc,
                                                                                 nrow(.x)),
                                                               .lookup_snow = rep(lookup_treatment(.y$plot_id)$snow,
                                                                                  nrow(.x)),
                                                               .lookup_fert = rep(lookup_treatment(.y$plot_id)$fert,
                                                                                  nrow(.x)),
                                                               .lookup_site = rep(lookup_treatment(.y$plot_id)$site,
                                                                                  nrow(.x)),
                                                               .lookup_plot = rep(lookup_treatment(.y$plot_id)$plot,
                                                                                  nrow(.x))
    )
    })
    # Merge the dataframe with the treatments
    dataframe <- merge(dataframe, treatments)

    dataframe <- dplyr::ungroup(dataframe)

    # Update the treatment information where the lookup_treatment function returned non NA data
    dataframe <- dplyr::mutate(dataframe, otc_treatment = ifelse(is.na(.lookup_otc),
                                                                 otc_treatment, .lookup_otc),
                               snow_treatment = ifelse(is.na(.lookup_snow),
                                                       snow_treatment, .lookup_snow),
                               fert_treatment = ifelse(is.na(.lookup_fert),
                                                       fert_treatment, .lookup_fert),
                               site = ifelse(is.na(.lookup_site),
                                                       site, .lookup_site),
                               plot = ifelse(is.na(.lookup_plot),
                                                       plot, .lookup_plot)
    )
    # return dataframe to original sorting
    dataframe <- dplyr::arrange(dataframe, .row)
    # remove the .row column added as a key for the asigning of the treatments as well as the
    # lookup_treatment columns
    dataframe <- dplyr::select(dataframe, !c(.row, .lookup_otc, .lookup_snow, .lookup_fert, .lookup_site, .lookup_plot))
    dataframe
}
