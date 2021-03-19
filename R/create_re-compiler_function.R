#' Re-compile parent file
#'
#' A function designed for troubleshooting. After running list_errornous_files() a
#' file (errornous_files.txt) will be produced containing a list of all compiled files
#' with the wrong number of columns. create_re-compiler_function takes the file path to
#' this text document as input, as well as the function used to produce the output (or
#' concievable any other function you might wish to apply to the parent files). This
#' from this function is a function which combines the features of create_file_reader_function()
#' and the probe_errors() function on the parent files. The output is a function which,
#' each time it is run will produce a list of the next parent file, and of the output of
#' the test function (which will be the final compiled file if using the compiler function as
#' the test function). The main utility of this function is to use it in conjunction with
#' the R studio debugger to determine where the compiling went wrong. A list of file paths
#' may be used as input instead of a file path to a text document containing them.
#' @param files Either a file path to a list of compiled files, or a list of the compiled
#' files
#' @param test_function The function to be applied to the parent file
#' @param ... Any further parameters required by the test function
#' @export


create_recompiler_function <- function(file_path, test_function, ...){
    files <- readr::read_lines(file_path)
    compiled_files <- sapply(files, load_file)
    parent_file_paths <- sapply(seq_along(compiled_files),
                                function(files, i) {files[[i]]$file[1]},
                                files = compiled_files)
    next_file_func <- create_file_reader_function(parent_file_paths)
    recompile_parent_func <- function(){
        probe_errors(next_file_func, test_function, ...)
    }
}
