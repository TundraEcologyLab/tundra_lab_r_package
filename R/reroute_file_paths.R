#' Change the route dir of a file path
#'
#' A vectorised function which accepts a list of file paths, and a path_to_route
#' detailing where the new location of the route directory. The function returns a
#' list of file paths pointing to the provided path_to_route. For example, if the
#' file path "C:/old/file/path/AF_database/things/stuff/data.csv" is to be updated
#' to "C:/new/path/to/file/AF_database/things/stuff/data.csv" then the path_to_route
#' would be "C:/new/path/to/file/AF_database". If the original file path does not
#' include the last directory of the path_to_route then the old file path is returned
#' unchanged and a warning is issued.
#' @param path_to_route The file path to the first common directory to both the
#' old and new file paths
#' @param file_paths All old file paths which have to be modified
#' @export


reroute_file_paths <- function(path_to_route, file_paths){
    # A function which takes a file_path and a path to a directory in said file_path
    # which is to be modified and returns a re-routed file path
    process_path <- function(path_to_route, file_path){
        # Extract the route directory from the path_to_route
        route_dir <- sub(".*//?([^/]+)/?/?$", "\\1", path_to_route)
        # If route_dir is present in file_path then modify all of the path before the
        # route_dir to create a new file_path. If the route_dir is not present in the
        # file_path then return the file_path unchanged
        if (grepl(route_dir, file_path)){
            new_path <- sub(paste0(".*//?", route_dir), path_to_route, file_path)
        } else {
            warning(paste0("route directory: ", route_dir, ", not present in file_path: ",
                           file_path))
            new_path <- file_path
        }
        new_path
    }

    # Create new file paths for each file_path provided
    new_paths <- lapply(file_paths, process_path, path_to_route = path_to_route)
    # Convert output to a character vector
    new_paths <- unlist(new_paths)
    new_paths
}


