#' Standardise all plot identifying columns
#'
#'A function which accepts a dataframe and returns it with its plot descriptor data
#'standardised. Input dataframe should include a column named "site" detailing the site at Alex
#'(eg Cassiope) and a column named "plot" containing the plot_number. If no "plot" column is
#'present then the data is assumed to be site, rather than plot level, data and is labeled
#'accordingly in the created plot_id as well as being given the plot number 0.
#'Standardise_plots combines the functions:
#'standardise_plot_col_names - to ensure all column names are consistent
#'standardise_sites - To ensure that the site names are consistent
#'standardise_plot_number - To ensure any additional data stored within the plot column is moved to
#'                          the appropriate column
#'standardise_treatment - To ensure treatment names are consistent and treatment types (otc vs snow)
#'                        are not mixed
#'get_plot_ID - To look up the correct unique plot IDs from the plot_names dataframe
#'The exact workings of each of these functions is described in their own help files. The output_dir
#'and file_path are also needed to produce possible warning files if the separating of snow and otc
#'treatment data is uncertain.
#'The final result should follow the same format as the plot_names file included within the tundra
#'package and accessible running data(plot_names).
#'@param dataframe The dataframe that is to have its plot descriptor data standardised
#'@param output_dir The directory to which a warning file may be written if mixed treatment columns
#'are encountered
#'@param file_path The file_path that will be used to describe the origin of the data in a potential
#'warning file if mixed treatment columns are encountered
#'@export

standardise_plots <- function(dataframe, output_dir, file_path){
    # Ensure all column names related to plot identification have the expected names
    dataframe <- standardise_plot_col_names(dataframe, output_dir, file_path)
    # If no site is present then throw an error, as can not continue without such data
    if (!"site" %in% names(dataframe)){
        stop("dataframe does not contain a site column")
    }
    # If no plot column is present then assume data is at the site level and add a plot
    # column containing the plot number 0 to indicate this
    if (!"plot" %in% names(dataframe)){
        dataframe <- dplyr::mutate(dataframe, plot = 0)
    }
    # Ensure all sites have the same standardised names found in the plot_names dataframe
    dataframe <- standardise_sites(dataframe)

    # Ensure all treatment names are standardised to those found within the plot_names dataframe
    dataframe <- standardise_treatment(dataframe)

    # Ensure the data within the plot number is standardised, and moved to other columns as appropriate
    dataframe <- standardise_plot_number(dataframe)

    # Load the plot_names dataframe
    plot_list <- plot_names




    # Correct for CO2. If CO2 is present and plot number is 1-4, change to 11-14.
    dataframe <- dplyr::mutate(dataframe, plot = ifelse(grepl("^[1-4]$", .data[["plot"]])&
                                                      !is.na(.data[["co2_plot"]])&
                                                     .data[["co2_plot"]] == "Y",
                                                 sub("([1-4])", "1\\1", .data[["plot"]]),
                                                 .data[["plot"]]
    ))

    # Add row_number to dataframe to line up the results of get_plot_ID with the data
    dataframe <- dplyr::mutate(dataframe, .row = 1:nrow(dataframe))
    # Produce a dataframe containing the unique plot IDs, and the corresponding row number in dataframe
    dataframe <- dplyr::group_by(dataframe, site, plot, otc_treatment, snow_treatment)
    plot_ids <- dplyr::group_modify(dataframe, ~ {data.frame(.row = .x$.row,  plot_id = rep(get_plot_ID(.y$site,
                                                                            .y$plot,
                                                                            .y$otc_treatment,
                                                                            .y$snow_treatment),
                                                                nrow(.x))
                                   )
            })
    # Merge the dataframe with the plot_ids
    dataframe <- merge(dataframe, plot_ids)

    dataframe <- dplyr::ungroup(dataframe)
    # return dataframe to original sorting
    dataframe <- dplyr::arrange(dataframe, .row)
    # remove the .row column added as a key for the asigning of the plot_ids
    dataframe <- dplyr::select(dataframe, !.row)

    # A plot_id can be identified without all treatment info being present. Now that the
    # plots have been identified use the tundra::plot_names lookup table to fill in any
    # missing treatment data
    dataframe <- treatments_from_plot_id(dataframe)
    dataframe
}
