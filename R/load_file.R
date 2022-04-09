#' Load CSV, Tab deliminated, or excel files despite asymmetry
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
#' If file is an excel file this is determined using readxl::excel_format and read using
#' readxl::read_excel. If sheets == NULL then all sheets present will be loaded, else only the sheets
#' identified in sheets. If only one sheet is loaded then a dataframe is returned, else a named
#' list is returned containing all the sheets as separate dataframes. split_file may not be used when
#' loading excel files.
#' @param file_path A path to the file which is to be loaded
#' @param sep A character vector of the separator used by the file to distinguish columns
#' @param split_file If TRUE, a list of dataframes is returned, each dataframe consisting of all
#' rows with the same number of columns
#' @param multi_header Should multiple header lines be searched for within the file
#' @param skip To be used for files where the header line can not be found automatically, most
#' notably when the dataframe has 4 or less columns. skip is either set to the number of rows
#' to skip to reach the column headers, or to "no header" if no header is present in the file.
#' Setting the skip term will not work if split_file or multi_header are set to TRUE
#' @param sheets Only relevant for excel files. Either a character vector of the name or names
#' of the sheets to be loaded, or an integer vector of the index or indices of the sheets to
#' be loaded
#' @export

load_file <- function(file_path, sep = NULL, split_file = FALSE, multi_header = FALSE,
                      skip = NULL, sheets = NULL){
    # If the separator has not been provided attempt to predict it
    if (is.null(sep) & is.na(readxl::excel_format(file_path))){
        sep <- file_type_identifier(file_path)
    }
    # If file is not to be split, load the whole file
    if (!split_file){
        dataframe <- standard_load_procedure(file_path, sep, multi_header, skip, sheets)

    } else {
        # If files are to be split, produce a list of dataframes - each dataframe
        # containing all rows with the same number of columns

        # Determine length of every row in file
        row_lengths <- count.fields(file_path, sep, quote = '"', comment.char = "")
        # Determine unique row lengths in file
        unique_row_lengths <- unique(sort(row_lengths))

        # If all rows are of the same length then load as normal, else load lines with
        # different lengths separately and return a list of differently lengthed dataframes

        if (length(unique_row_lengths) == 1){
            dataframe <- standard_load_procedure(file_path, sep, multi_header)
        } else {


            # create list to store segregated file parts
            dataframe <- list()

            # read data into dataframes, grouped by the unique_row_lengths
            dataframe <- lapply(unique_row_lengths,
                                function(file, row_lengths, sep, unique_row_length){
                                    load_specific_row_length(file, row_lengths, sep, unique_row_length)},
                                file = file_path,
                                row_lengths = row_lengths,
                                sep = sep)
        }


    }

    dataframe
}

standard_load_procedure <- function(file_path, sep, multi_header, skip = NULL, sheets = NULL){
    # If file is an excel file then load using readxl package. Will return a named list for all
    # present sheets in file, or exclusively the sheets named if sheets != NULL. Returns a
    # single dataframe, not a list, if only one sheet is to be returned
    if (!is.na(readxl::excel_format(file_path))){
        dataframe <- load_from_excel(file_path, multi_header, skip, sheets)
    } else {
        # If skip has not been provided, determine the number of rows to skip such that
        # the first row is the header line. Can return a list of skips if multi_header = TRUE
        if (is.null(skip)){
            skip <- find_header(file_path, sep, multi_header)
        }
        # Load the data between detected header lines into separate dataframes. If only one header
        # load all data under the header.
        dataframe <- sapply(seq_along(skip), FUN = load,
                            skip = skip,
                            file_path = file_path,
                            sep = sep
        )
    }
    # If only one dataframe then return as a dataframe rather than a list
    if (length(dataframe) == 1){
        dataframe <- as.data.frame(dataframe)
        dataframe <- dplyr::as_tibble(dataframe)
    }
    dataframe
}

load <- function(skip, file_path, sep, skip_index, sheet = NULL){
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
    if (skip != "no header" & is.null(sheet)){
        # If the header does not have a column name for every column with data then read.table will
        # return an error. This will result in dataframe having a length of 0. The quote is set to "\""
        # to avoid apostorphes confusing the loading of file (the default for quote is both double and
        # single quotes). If no quote is set then commas within cells will confuse loading. comment.char
        # by defualt in read.table is set to "#", which means A # in a file will lead to the data beyond
        # it on that line being ignored, which can lead to the file failing to load at all. To avoid this
        # comment.char is set blank.
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
                                    quote = "\"",
                                    comment.char = ""

        ), silent = TRUE)

        # If the found header does not cover all data read the csv file as if no header was found
        if (length(dataframe) == 0){
            # count.fields provides the number of columns per line. This is required to prevent R
            # from truncating later columns that have additional columns. count.fields returns NA
            # when a field contains a character string with cariage returns.
            # The manual naming of columns to the length of the longest row ensures all data is loaded.
            # comment.char by defualt in read.table is set to "#", which means A  in a file will lead
            # to the data beyond it on that line being ignored, which can lead to the file failing to
            # load at all. To avoid this comment.char is set blank.
            number_of_cols <- max(
                count.fields(file_path, sep = sep, quote = '"', comment.char = "")[!is.na(count.fields(
                    file_path, sep = sep, quote = '"', comment.char = ""))])
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
                                    quote = "\"",
                                    comment.char = ""

            )
        }
    } else if (skip == "no header" & is.null(sheet)){
        # count.fields provides the number of columns per line. This is required to prevent R
        # from truncating later columns that have additional columns. count.fields returns NA
        # when a field contains a character string with cariage returns.
        # The manual naming of columns to the length of the longest row ensures all data is loaded
        # comment.char by defualt in read.table is set to "#", which means A  in a file will lead
        # to the data beyond it on that line being ignored, which can lead to the file failing to
        # load at all. To avoid this comment.char is set blank.
        number_of_cols <- max(
            count.fields(file_path, sep = sep, quote = '"', comment.char = "")[!is.na(count.fields(
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
                                quote = "\"",
                                comment.char = ""
        )
    } else if (skip != "no header" & !is.null(sheet) & n_rows != -1){
        dataframe <- readxl::read_excel(path = file_path,
                                        sheet = sheet,
                                        col_names = TRUE,
                                        col_types = "text",
                                        trim_ws = TRUE,
                                        skip = skip,
                                        n_max = n_rows,
                                        .name_repair = "minimal"
                                        )
    } else if (skip != "no header" & !is.null(sheet) & n_rows == -1){
        dataframe <- readxl::read_excel(path = file_path,
                                        sheet = sheet,
                                        col_names = TRUE,
                                        col_types = "text",
                                        trim_ws = TRUE,
                                        skip = skip,
                                        .name_repair = "minimal"
        )
    } else if (skip == "no header" & !is.null(sheet)){
        dataframe <- readxl::read_excel(path = file_path,
                                        sheet = sheet,
                                        col_names = FALSE,
                                        col_types = "text",
                                        trim_ws = TRUE,
                                        .name_repair = "minimal"
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

load_from_excel <- function(file_path, multi_header = FALSE, skip = NULL, sheets = NULL){
    # Create a list to contain all the sheets to be loaded from the excel file
    all_dataframes <- list()

    # Determine names of all sheets present in excel file
    sheet_names <- readxl::excel_sheets(file_path)
    # If sheets == NULL then set sheets to sheet names
    if (is.null(sheets)){
        sheets <- sheet_names
    }

    # For each sheet to be loaded, check that it exists, find the header line, load the sheet
    # and append it to the list all_dataframes
    for (sheet in sheets){
        # If sheet is not a name of a sheet in the excel file or an index to a sheet in the file
        # then return an error
        if (!sheet %in% sheet_names){
            if (!(is.numeric(as.numeric(sheet)) & sheet > 0 & sheet <= length(sheet_names))){
                stop(paste(sheet, "is not a sheet in", file_path))
            }
        }
        # load top of dataframe to determine if there is data present and to find the header
        # If multi_header == TRUE then load whole sheet for this purpose
        if (multi_header){
            dataframe_top <- readxl::read_excel(file_path,
                                                sheet,
                                                col_names = FALSE,
                                                col_types = "text"
            )
        } else {
            dataframe_top <- readxl::read_excel(file_path,
                                                sheet,
                                                col_names = FALSE,
                                                col_types = "text",
                                                n_max = 10
            )
        }
        # If sheet contains no data then skip it and don't include in final output.
        # remove the sheet from the sheet list
        if (length(dataframe_top) == 0){
            sheets <- sheets[sheets != sheet]
            next
            }
        # Find header line (or header lines if multi_header == TRUE) for the sheet, unless skip
        # has already been defined
        if (is.null(skip)){
            header_rows <- search_dataframe_for_header(dataframe_top, nrow(dataframe_top), multi_header)
            # The number of rows to skip is one less than the row containing the header so
            # subtract one to determine rows to skip when loading, unless no header was found
            # and no skipping is required
            if (header_rows == "no header"){
                skip <- "no header"
            } else {
                skip <- header_rows - 1
            }
        }
        # load the full sheet, separated into a list of multiple dataframes if multiple
        # headers were found
        dataframe <- sapply(seq_along(skip), FUN = load,
                            skip = skip,
                            file_path = file_path,
                            sep = sep,
                            sheet = sheet
        )

        # If only one dataframe then return as a dataframe rather than a list
        if (length(dataframe) == 1){
            dataframe <- as.data.frame(dataframe)
            dataframe <- dplyr::as_tibble(dataframe)
        }
        # append dataframe to all_dataframes
        all_dataframes <- append(all_dataframes, list(dataframe))

    }
    # name the dataframes in all_dataframes with the sheet names
    all_dataframes <- setNames(all_dataframes, sheets)


    all_dataframes
}
