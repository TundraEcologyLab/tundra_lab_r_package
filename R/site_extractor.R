#' extract site from string
#'
#' A function which accepts a character vector and returns all unique instances within that string
#' that match a site name. If no sites are found then it returns NULL. Setting unique to FALSE means
#' all hits are returned, which is useful for counting them. If multiple sites are present within one
#' string then an error is returned
#' @param string A character vector that is to be examined for sites
#' @param unique Should result return all unique hits or a full list for counting occurrences
#' @export

site_extractor <- function(string, unique = TRUE){
  # create variable for site which shall be returned, and a count to ensure that only
  # one site is contained within the string
  site <- NULL
  count <- 0
  # If only one string has been passed to extract_site then extract site. Otherwise, call
  # extract_site for each string
  if (length(string) == 1){
  # Search for the site names within the string. If a hit is found update site and count. If multiple
    # hits are found for one string an error will be returned
  if (grepl("dome", x = string, ignore.case = TRUE)){
                site <- "Dome"
                count <- count + 1
  }
   if (grepl("sax", x = string, ignore.case = TRUE)){
                site <- "Sax_Opp"
                count <- count + 1
  }
   if (grepl("Fert", x = string, ignore.case = TRUE)){
    site <- "Fert"
    count <- count + 1
  }
   if (grepl("beach", x = string, ignore.case = TRUE)){
    site <- "Beach_Ridge"
    count <- count + 1
  }
   if (grepl("cas", x = string, ignore.case = TRUE)&
                  !grepl("an?nex", string, ignore.case = TRUE)){
    site <- "Cassiope"
    count <- count + 1
  }
   if (grepl("will", x = string, ignore.case = TRUE)&
                  !grepl("an?nex", string, ignore.case = TRUE)){
    site <- "Willow"
    count <- count + 1
  }
   if (grepl("mead|maed", x = string, ignore.case = TRUE)&
                  !grepl("an?nex", string, ignore.case = TRUE)){
    site <- "Meadow"
    count <- count + 1
  }
   if (grepl("dry", x = string, ignore.case = TRUE)&
                  !grepl("an?nex", string, ignore.case = TRUE)){
    site <- "Dryas"
    count <- count + 1
  }
   if (grepl("will", x = string, ignore.case = TRUE)&
                  grepl("an?nex", string, ignore.case = TRUE)&
                  !grepl("BS", string, ignore.case = TRUE)){
    site <- "Annex_Willow"
    count <- count + 1
  }
   if (grepl("cas", x = string, ignore.case = TRUE)&
                  grepl("an?nex", string, ignore.case = TRUE)&
                  !grepl("BS", string, ignore.case = TRUE)){
    site <- "Annex_Cassiope"
    count <- count + 1
  }
   if (grepl("will", x = string, ignore.case = TRUE)&
                  grepl("an?nex", string, ignore.case = TRUE)&
                  grepl("BS", string, ignore.case = TRUE)){
    site <- "Annex_BS_Willow"
    count <- count + 1
  }
   if (grepl("cas", x = string, ignore.case = TRUE)&
                  grepl("an?nex", string, ignore.case = TRUE)&
                  grepl("BS", string, ignore.case = TRUE)){
    site <- "Annex_BS_Cassiope"
    count <- count + 1
  }
   if (grepl("migr", x = string, ignore.case = TRUE)){
    site <- "Annex_Migration"
    count <- count + 1
  }
   if (grepl("baker", x = string, ignore.case = TRUE)){
    site <- "Baker_Lake"
    count <- count + 1
  }
  # If multiple site names are present within the string then return an error
  if (count > 1){
    stop("Multiple site names detected within same string")
  }
  # if multiple strings have been supplied then run the extract_site function individually on each
  # string
  } else {
    site <- lapply(string, site_extractor)
    site <- unlist(site)
  }
  # If unique is set to TRUE then return only unique entries
  if (unique == TRUE){
    site <- unique(site)
  }
  site
}
