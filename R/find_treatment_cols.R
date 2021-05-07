#' Find Treatment Columns
#' 
#' A function which accepts a dataframe and returns it with any treatment columns identified as either
#' otc_treatment or snow_treatment. If a column is present containing both snow and otc treatment
#' then the data will be split into otc and snow treatment columns. If a mixed column is present as well
#' as a dedicated snow or otc column this will cause the function to crash as it is not possible to 
#' determine which data is to be kept. Any time a mixed otc/snow column is found a warning file is 
#' produced in the output_dir containing the file_path of the original data. This is because it is 
#' dificult to determine if "control" should belong to snow or otc.
#' @param dataframe A dataframe containing columns with names matching regex "otc|treat" containing
#' which are to be identified as either otc_treatment or snow_treatment
#' @param file_path A file path for the original data. Used in the output of a warning message if
#' snow and otc treatment data could not be clearly separated.
#' @param output_dir The directory that a warning file will be written to if snow and otc treatment
#' data can not be clearly separated.
#' @export

find_treatment_cols <- function(dataframe, output_dir, file_path){
  # A list of terms used exclusively to describe otc treatments. If none of these terms are
  # present then a column is assumed not to contain otc treatmnt data.
  # Likewise, if a colunm contains none of the strings in snow_specific it is assumed not to
  # contain any snow treatment data.
  OTC_specific <- c("OTC", "Cover", "Pheno", "T", "Cc", "CO2_T", "W", "W(CO2)", "OTC (CO2")
  snow_specific <- c("snow adition", "snow removal", "A", "R", "addition", "removal")
  
  
  # The regex used to identify any treatment column. Any column not matching this will be ignored.
  col_pattern <- "otc|treat"
  # A logical vector identifying which columns contain treatment data
  treatment_cols <- grepl(col_pattern, names(dataframe), ignore.case = TRUE)
  
  # All treatment columns are renamed "treatment", and then make_clean_names is used to ensure that
  # if this produces duplicate column names that they are itterated with "_._" and then a counting
  # number.
  names(dataframe)[treatment_cols] <- "treatment"
  names(dataframe) <- janitor::make_clean_names(names(dataframe), unique_sep = "_._")
  # For each treatment column present:

  # Determine whether the column contains either otc or snow data, or both, and rename the column
  # appropriately.
  for (name in names(dataframe)[treatment_cols]){
    # Determine the index of the treatment column
    name_index <- which(names(dataframe) == name, arr.ind = TRUE)
    
    # Determine if treatment column contains any terms unique to either OTC or snow data
    OTC_present <- sum(OTC_specific %in% dataframe[[name_index]]) > 0
    snow_present <- sum(snow_specific %in% dataframe[[name_index]]) > 0
    # If data from both otc and snow is present, create both an otc_treatment and snow_treatment column,
    # and attempt to separate the data between them. Also, create a warning file in output_dir
    if (OTC_present & snow_present){
      # Create a warning file identifying which files contain mixed treatment columns
      write_lines(file_path, paste0(output_dir, "/treatment_mixed.txt"), append = TRUE)
      # Throw an error if a snow or otc treatment column already exists, because splitting this
      # mixed column will wipe that data
      if (sum(c("otc_treatment", "snow_treatment") %in% names(dataframe)) > 0){
        simpleError("Dataframe contained a mixed otc/snow treatment column as well as individual
                        treatment columns")
      }
      # Create both treatment columns
      dataframe <- dplyr::mutate(dataframe, otc_treatment = NA)
      dataframe <- dplyr::mutate(dataframe, snow_treatment = NA)
      # Send all data in the mixed treatment column that has a snow specific term, or that is from
      # the beach ridge site to the snow_treatment column. All other data is sent to the otc
      # treatment column
      dataframe <- dplyr::mutate(dataframe,
                                 snow_treatment = ifelse(.data[["site"]] == "Beach_Ridge"|
                                                           .data[[name]] %in% snow_specific,
                                                         .data[[name]], snow_treatment),
                                 otc_treatment = ifelse(!(.data[["site"]] == "Beach_Ridge"|
                                                            .data[[name]] %in% snow_specific),
                                                        .data[[name]], otc_treatment))
      # Now that the data has been copied to snow and otc treatment columns, remove the original
      # mixed column from the dataframe
      dataframe <- dplyr::select(dataframe, !.data[[name]])
    } else if (OTC_present){
      # Rename the treatment column that contains only otc data as otc_treatment
      dataframe <- dplyr::rename(dataframe, otc_treatment = .data[[name]])
    } else if (snow_present){
      # Rename the treatment column that contains only snow data as snow_treatment
      dataframe <- dplyr::rename(dataframe, snow_treatment = .data[[name]])
    } else {
      # if no treatment data is found, and all data present is NA, remove the column from the
      # dataframe
      if (sum(is.na(dataframe[[name_index]] == length(dataframe[[name_index]])))){
        dataframe <- dplyr::select(dataframe, !.data[[name]])
      }
    }
  }
  dataframe
}