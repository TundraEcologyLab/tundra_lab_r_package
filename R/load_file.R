#' Load CSV or Tab deliminated files despite asymmetry
#'
#' A function to load the data from the file identified in file_path. If no separator is provided
#' it uses file_type_identifier to predict it. It also uses find_header()
#' to attempt to locate meaningful header names, and sets the variable skip accordingly (see
#' find_header() for details). If the header does not cover every column in the file then the
#' header is ignored. load_file is configured to handle asymmetric files, and to strip white
#' space from fields. All columns are read as characters to provide consistent data retrieval.
#' If split_file TRUE, then a list of dataframes is returned, each dataframe consisting of all
#' rows with the same number of columns
#' If multi_header is TRUE, then the whole file is searched for possible header lines. If multiple
#' headers are detected, the data between each header is loaded into separate dataframes and a list
#' of these dataframes is returned
#' @param file_path A path to the file which is to be loaded
#' @param sep A character vector of the separator used by the file to distinguish columns
#' @param split_file If TRUE, a list of dataframes is returned, each dataframe consisting of all
#' rows with the same number of columns
#' @param multi_header Should multiple header lines be searched for within the file
#' @export

load_file <- function(file_path, sep = NULL, split_file = FALSE, multi_header = FALSE){
    # If the separator has not been provided attempt to predict it
    if (is.null(sep)){
        sep <- file_type_identifier(file_path)
    }
    # If file is not to be split, load the whole file
    if (!split_file){
        # Determine the number of rows to skip such that the first row is the header line
        # Can return a list of skips if multi_header = TRUE
        skip <- find_header(file_path, sep, multi_header)
        # Load the data between detected header lines into separate dataframes. If only one header
        # load all data under the header.
        dataframe <- sapply(seq_along(skip), FUN = load,
                            skip = skip,
                            file_path = file_path,
                            sep = sep
        )
        # If only one dataframe then return as a dataframe rather than a list
        if (length(dataframe) == 1){
            dataframe <- as.data.frame(dataframe)
        }

    } else {
        # If files are to be split, produce a list of dataframes - each dataframe
        # containing all rows with the same number of columns

        # Determine length of every row in file
        row_lengths <- count.fields(file_path, sep, '""')
        # Determine unique row lengths in file
        unique_row_lengths <- unique(sort(row_lengths))


        # create list to store segregated file parts
        dataframe <- list()
        # open connection to file

        # read data into dataframes, grouped by the unique_row_lengths
        dataframe <- lapply(unique_row_lengths,
                            function(file, row_lengths, sep, unique_row_length){
                                load_specific_row_length(file, row_lengths, sep, unique_row_length)},
                            file = file_path,
                            row_lengths = row_lengths,
                            sep = sep)



    }
    # simplify all names with repeated ".." with "_"
    # while (sum(grepl("([^\\.\\.]+)\\.\\.+([^\\.\\.]+)", names(dataframe))) > 0){
    #     names(dataframe) <- gsub("([^\\.\\.]+)\\.\\.+([^\\.\\.]+)", "\\1_\\2", names(dataframe))
    #}
    dataframe
}

load <- function(skip, file_path, sep, skip_index){
    # If skip index is for the last skip provided in skip (only a consideration when applying
    # multi_header), then load to the end of the file, else load between the current skip index
    # and skip_index + 1
    if (skip_index < length(skip)){
        n_rows <- skip[skip_index + 1] - skip[skip_index] - 1
    } else {
        # negative n_rows are ignored, so the file will be loaded till the end
        n_rows <- -1
    }
    skip <- skip[skip_index]
    if (skip != "no header"){
        # If the header does not have a column name for every column with data then read.table will
        # return an error. This will result in dataframe having a length of 0. The quote is set to "\""
        # to avoid apostorphes confusing the loading of file (the default for quote is both double and
        # single quotes). If no quote is set then commas within cells will confuse loading
        dataframe <- data.frame()
        try(dataframe <- read.table(file = file_path,
                                    skip = skip,
                                    header = TRUE,
                                    colClasses = "character",
                                    sep = sep,
                                    fill = TRUE,
                                    strip.white = TRUE,
                                    check.names = FALSE,
                                    row.names = NULL,
                                    nrows = n_rows,
                                    quote = "\""

        ))

        # If the found header does not cover all data read the csv file as if no header was found
        if (length(dataframe) == 0){
            # count.fields provides the number of columns per line. This is required to prevent R
            # from truncating later columns that have additional columns. count.fields returns NA
            # when a field contains a character string with cariage returns.
            # The manual naming of columns to the length of the longest row ensures all data is loaded
            number_of_cols <- max(
                count.fields(file_path, sep = sep, quote = '""')[!is.na(count.fields(
                    file_path, sep = sep, quote = '""'))])
            dataframe <- read.table(file = file_path,
                                    header = FALSE,
                                    col.names = paste0("V", seq_len(number_of_cols)),
                                    colClasses = "character",
                                    sep = sep,
                                    fill = TRUE,
                                    strip.white = TRUE,
                                    check.names = FALSE,
                                    row.names = NULL,
                                    nrows = n_rows,
                                    quote = "\""

            )
        }
    } else {
        # count.fields provides the number of columns per line. This is required to prevent R
        # from truncating later columns that have additional columns. count.fields returns NA
        # when a field contains a character string with cariage returns.
        # The manual naming of columns to the length of the longest row ensures all data is loaded
        number_of_cols <- max(
            count.fields(file_path, sep = sep, quote = '""')[!is.na(count.fields(
                file_path, sep = sep, quote = '""'))])
        dataframe <- read.table(file = file_path,
                                header = FALSE,
                                col.names = paste0("V", seq_len(number_of_cols)),
                                colClasses = "character",
                                sep = sep,
                                fill = TRUE,
                                strip.white = TRUE,
                                check.names = FALSE,
                                row.names = NULL,
                                nrows = n_rows,
                                quote = "\""
        )
    }
    # Clean the column names of the loaded dataframe. If multiple columns exist with the same name
    # iterate the duplicate names starting from one. Separate the name from the iterator by "_._"
    # to ensure it is clear that the number is an automatically generated iterator and not original
    # data
    dataframe <- janitor::clean_names(dataframe, unique_sep = "_._")
    dataframe <- dplyr::as_tibble(dataframe)
    list(dataframe)
}
