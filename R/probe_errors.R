#' A function for probing errors reported in text documents by tundra functions
#'
#' A function which accepts a next_file function (produced by the create_file_reader_function)
#' and the function which produced the error. The output is a list where the first element
#' is the dataframe that generated the error, and the second element is the undesired output
#' from the test function. ... is used to pass any required parameters to the test_function
#' @param next_file_func A function produced by the create_file_reader_function, which will retrieve
#' the data from the next file identified in an error reporting text document
#' @param test_function The function which is to be tested against the next file identified by
#' next_file_func
#' @param ... Any additional parameters required by test_function
#' @export


probe_errors <- function(next_file_func, test_function, ...){
    next_file_data <- next_file_func()
    function_output <- test_function(next_file_data, ...)
    listed_output <- list(next_file_data, function_output)
    listed_output
}
