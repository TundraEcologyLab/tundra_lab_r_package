#' Generic lookup function
#'
#' A function which accepts a key column name, key, and lookup table, searches the
#' key_column in the lookup table for the key, and returns a named list containing
#' all the other column entries that corespond with that key. If more than one hit
#' is found for the key, or no hits are found, then a list of NAs are returned. The
#' names for the returned list are taken from the names of the columns in the lookup
#' table.
#' @param key_col_name What is the name of the column that should be searched for the key
#' @param key The thing which should uniquely identify the data that is being searched for
#' such as an ID number
#' @param lookup_table The dataframe which should be searched for the data
#' @export


lookup <- function(key_col_name, key, lookup_table){
    # search the lookup table for the key
    hit <- lookup_table %>% dplyr::filter(.data[[key_col_name]] == key)
    # Get list of column names in the lookup table minus the key_col. This will be used
    # to identify all the attributes that should be returned
    non_key_cols <- names(lookup_table)[names(lookup_table) != key_col_name]

    # define function for finding the other attributes
    find_attributes <- function(attribute_col, hit){
        attribute <- hit[[attribute_col]]
    }
    if (nrow(hit) == 1){
        # Get a list of all the attributes to be returned
        result <- lapply(non_key_cols, find_attributes, hit = hit)
        # name the list
        result <- setNames(result, non_key_cols)
    } else {
        # return a list of NAs for all attributes
        result <- rep(NA, length(non_key_cols))
        # name list
        result <- setNames(result, non_key_cols)
    }
    result
}
