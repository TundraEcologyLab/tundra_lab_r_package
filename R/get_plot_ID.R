#' Get plot ID
#'
#' A function which accepts as a minimum, a site name, and plot number, with additional options
#' for otc and snow treatment. The function looks up the plot_names dataframe stored within the
#' tundra package, and returns the plot ID which is unique to the provided site/plot characteristics.
#' If multiple plots could have the provided characteristics then all applicable plot IDs are
#' returned in one character string separated by /. If no valid plot Ids can be identified then
#' NA is returned.
#' @param site_name A character vector providing the name of the site
#' @param plot_number A character vector providing the plot number. Must be a string, as some
#' plot numbers either include or are exclusively letters
#' @param otc A character vector indicating the otc treatment applied
#' @param snow A character vector indicating the snow treatment applied
#' @param plot_df The dataframe containing the plot IDs. Defaults to the plot_names dataframe
#' stored within the tundra package
#' @export

get_plot_ID <- function(site_name,
                        plot_number,
                        otc = NA,
                        snow = NA,
                        plot_df = tundra::plot_names){
    # Filter plot database down to provided site and plot number
    plot_df <- dplyr::filter(plot_df, site == site_name, plot == plot_number)
    # If there is still more than one possible plot ID, and the otc provided is not NA
    # Further filter the plot database by otc treatment
    if (length(plot_df$plot_id) > 1 & !is.na(otc)){
        plot_df <- dplyr::filter(plot_df, otc_treatment == otc)
    }
    # If there is still more than one possible plot ID, and the snow provided is not NA
    # Further filter the plot database by snow treatment
    if (length(plot_df$plot_id) > 1 & !is.na(snow)){
        plot_df <- dplyr::filter(plot_df, snow_treatment == snow)
    }
    # If there is only one possible plot ID, return it. If there is more than one, combine
    # them all into one string separated by "/". If there are none, return NA
    if (length(plot_df$plot_id) > 1 ){
        plot_ID <- stringr::str_c(plot_df[["plot_id"]], collapse  = "/")
    } else if (length(plot_df$plot_id) == 1){
        plot_ID <- plot_df[["plot_id"]]
    } else {
        plot_ID <- NA
    }
    plot_ID
}
