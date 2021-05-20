#' Create function for reading files identified in text document
#'
#' A function which is designed to help efficiently read the files identified by the
#' warning text documents produced by other tundra functions. create_file_reader_function
#' takes a file path to a warning text document as input. It outputs a function which,
#' each time it is called, outputs the content of the next file identified by the warning
#' document. Files are loaded using the load_file function. Calling the outputed function
#' after every file has been read will result in the first file being read again. When reading
#' the text document, this function will ignore anything preceding the last colon on a line,
#' if any colon is present. Alternatively, a list of file paths can be provided as input in place
#' of a file path to a text document containing the paths. path_only can be set to TRUE, resulting in
#' only the file path being returned (the file is not its self loaded). This is mostly convenient
#' for use with the probe_errors function
#' @param files Either the path to a warning text document produced by some other tundra
#' functions, or a list of file paths
#' @param path_only If TRUE, only the file path is returned, not the data contained within that file
#' path. This is included to facilitate the probe_errors function.
#' @export


create_file_reader_function <- function(files, path_only = FALSE){
    if (is.list(files)){
        file_list <- unlist(files)
    } else {
    # read the contents of the text document
    file_list <- readr::read_lines(files, skip_empty_rows = TRUE)
    # eliminate any text preceding a colon
    file_list <- sub(".*: *", "", file_list)
    }
    # number of files identified in file list
    number_of_files <- length(file_list)
    # An iterator used by the next_file function to keep track of its place in reading
    # the file_list
    file_number <- 1
    # create a function which will print the file number and file path, as well as
    # output the contents of the file identified by the next file in the file_list.
    # Finally it iterates the file_number
    next_file <- function(){
        print(paste0("file ", file_number, " of ", number_of_files))
        print(file_list[file_number])
        # If only the file path is to be returned (file is not to be loaded), itterate to next file
        # and return current file_path
        if (path_only){
            file_path <- file_list[file_number]
            # increase file_number by one for next call of function, or return to one if
            # reading the final file
            if (file_number < number_of_files){
                file_number <<- file_number + 1
            } else {
                file_number <<- 1
            }
            return(file_path)
        }
        dataframe <- load_file(file_list[file_number])
        # increase file_number by one for next call of function, or return to one if
        # reading the final file
        if (file_number < number_of_files){
            file_number <<- file_number + 1
        } else {
            file_number <<- 1
        }
        dataframe
    }
    next_file
}
