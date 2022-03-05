#' Look up treatments given plot ID
#'
#' A function which accepts a plot_id and returns a named list providing the OTC,
#' Snow, and Fertaliser treatments. The data is looked up in the tundra::plot_names
#' dataframe. If the plot_id does not return one, and only one hit, then NA is returned.
#' @param id The plot Id to be looked up to find the corresponding treatment data
#' @param plot_df The dataframe which will be searched for the treatment data. Defaults
#' to tundra::plot_names
#' @export


lookup_treatment <- function(id, plot_df = tundra::plot_names){

  plot_data <- dplyr::filter(plot_df, plot_id == id)

  if (nrow(plot_data) != 1){
    treatment_data <- list(
      otc = NA,
      snow = NA,
      fert = NA,
      site = NA,
      plot = NA
    )
  } else {

    treatment_data <- list(
      otc = plot_data$otc_treatment,
      snow = plot_data$snow_treatment,
      fert = plot_data$fert_treatment,
      site = plot_data$site,
      plot = plot_data$plot
    )

  }
  treatment_data
}
