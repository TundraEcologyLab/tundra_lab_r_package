#' Standardise all plot identifying columns
#'
#'A function which accepts a dataframe and returns it with its plot descriptor data
#'standardised. It combines the functions:
#'standardise_plot_col_names - to ensure all column names are consistent
#'standardise_sites - To ensure that the site names are consistent
#'standardise_plot_number - To ensure any additional data stored within the plot column is moved
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
#'@param file_path The file_path that will be used to describe to origin of the data in a potential
#'warning file if mixed treatment columns are encountered
#' @export

standardise_plots <- function(dataframe, output_dir, file_path){
    # Ensure all column names related to plot identification have the expected names
    dataframe <- standardise_plot_col_names(dataframe, output_dir, file_path)
    # Ensure all sites have the same standardised names found in the plot_names dataframe
    dataframe <- standardise_sites(dataframe)
    # Ensure the data within the plot number is standardised, and moved to other columns as appropriate
    dataframe <- standardise_plot_number(dataframe)
    # Ensure all treatment names are standardised to those found within the plot_names dataframe
    dataframe <- standardise_treatment(dataframe)


    # Load the plot_names dataframe
    plot_list <- plot_names


    # Ensure that if any of the 4 columns needed to run the get_plot_ID function are missing
    # that they are added, filled NA
    needed_cols <- c("site", "plot", "otc_treatment", "snow_treatment")
    for (needed_col in needed_cols){
        if (!needed_col %in% names(dataframe)){
            dataframe <- mutate(dataframe, "{needed_col}" := NA)
        }
    }

    # Correct for CO2. If CO2 is present and plot number is 1-4, change to 11-14.
    dataframe <- mutate(dataframe, plot = ifelse(grepl("^[1-4]$", .data[["plot"]])&
                                                      !is.na(.data[["co2_plot"]])&
                                                     .data[["co2_plot"]] == "Y",
                                                 sub("([1-4])", "1\\1", .data[["plot"]]),
                                                 .data[["plot"]]
    ))

    # Run the get_plot_ID function on every row of the dataframe to give each row a unique plot_ID
    dataframe <- dataframe %>% rowwise() %>%
        mutate(plot_id = get_plot_ID(.data[["site"]],
                                     .data[["plot"]],
                                     .data[["otc_treatment"]],
                                     .data[["snow_treatment"]],
                                     plot_list))
    dataframe <- ungroup(dataframe)
    dataframe
}
