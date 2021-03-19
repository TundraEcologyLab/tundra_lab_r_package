#' Produce list of miss-compiled files
#'
#' A function which takes the file path for an output directory for one of the compiler
#' scripts, as well as max and minimum number of columns those files should have if they
#' compiled correctly. It then produces a list of the erroneous files in the text document
#' erroneous_files.txt in the same directory. This list can then be used as input to efficiently
#' examine the miss-compiled files using the create_file_reader_function function, or the
#' original compilation may be investigated using the create_recompiler_function function.
#' @param directory The directory containing the compiled files to be examined. Also, where the
#' list of miss-compiled files will be written to.
#' @param min_cols The minimum number of columns a compiled file should have
#' @param max_cols The maximum number of columns a compiled file should have
#' @export

list_erronous_files <- function(directory, min_cols, max_cols){
    # Create a list of all csv files in the directory
    files <- list.files(directory, "\\.csv", ignore.case = TRUE, full.names = TRUE)
    # For all files in directory, add file path to erroneous_files.txt if number of columns
    # is outwith the accepted window
    for (file in files){
        data <- load_file(file)
        if (!(length(data) >= min_cols & length(data) <= max_cols)){
            readr::write_lines(file, paste0(directory, "/erroneous_files.txt"), sep = "\n", append = TRUE)
        }
    }
}
