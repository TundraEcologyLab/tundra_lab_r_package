#' Combine dataframes from file
#'
#'  This function accepts as input a list of file paths and outputs a dataframe of
#'  All of the associated files combined together, with any columns not pressent for
#'  a given file populated NA. The function also produces a column called file, which
#'  contains the file path to the original data. This function uses the load_file
#'  function to automatically detect the header lines of the given files. This will
#'  work quite reliably so long as the files are at least 5 columns wide.
#'  @param file_list A list of files to be loaded and merged into one dataframe
#' @export

rough_compile <- function(file_list){
    # A function for loading a file and including the file_path to that file in its
    # dataframe
    process_file <- function(file_path){
        # load the file
        dataframe <- load_file(file_path)
        # Add a column called file containing the file_path to the original data
        dataframe <- dplyr::mutate(dataframe, file = file_path)
        dataframe
    }


    # load each file and add a file column containing the file_path
    dataframes <- lapply(file_list, process_file)
    # combine all dataframes into one
    dataframe <- plyr::rbind.fill(dataframes)
    dataframe
}



