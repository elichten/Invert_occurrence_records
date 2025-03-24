format_provider_data <- function(table = NULL, provider = NULL) {
  if (is.null(table)) {
    stop(" - Please provide a BeeBDC-formatted table")
  }
  if (is.null(provider)) {
    stop(" - Please indicate table source (gbif, itis, or dl)")
  }
  
  provider <- tolower(provider)
  
  if (!provider %in% c("itis", "dl", "gbif")) {
    stop(" - Please indicate provider (gbif, itis, or dl)")
  }
  
  # Create column for accepted ids
  table <- table %>%
              left_join(dplyr::select(table, id, canonical, authorship),
                        by = c("accid" = "id"),
                        suffix = c("", "_accepted"))
  
  # TODO: get_itis_authorship takes prohibitively long
  if (tolower(provider) == 'itis') {
    #### The ITIS API doesn't provide authorship information in taxonomic backbone downloads, but it can be queried using taxonID (aka tsn) with ritis::taxon_authorship. Below is a function that can be mapped to the "taxonID" column to extract the numeric tsn to query authorship. This function is best used when sppList <10 species in length, as larger datasets will cause throttling issues with the ITIS api, drastically increasing processing time.
    get_itis_authorship <- function(taxonID) {
      attempt <- 1
      max_attempts <- 10
      # Attempt 5 times for each tsn (ITIS sometimes has 404 problems)
      while (attempt <= max_attempts) {
        tryCatch({
          # Extract the TSN number from the taxonID string
          tsn <- stringr::str_extract(taxonID, "(?<=ITIS:)\\d+")
          
          # If no TSN is found, return NA
          if (is.na(tsn)) {
            message("No TSN found. Returning NA.")
            return(NA)
          }
          
          # Call the taxon_authorship function with the extracted TSN
          message("Getting authorship for tsn: ", tsn)
          result <- ritis::taxon_authorship(as.numeric(tsn))
          
          # If successful, set attempt to 1
          return(result$authorship)
          
        }, error = function(e) {
          # Catch errors (e.g., network issue, API issues) and retry
          message("Error in taxon_authorship: ", e$message)
          message("Attempt ", attempt, " of ", max_attempts, ". Retrying...")
          
          # Increment attempt count and try again
          attempt <<- attempt + 1
        })
      }
      message("No response in maximum attempts. Returning NA")
      return(NA)
    }
    
    message('Getting ITIS authorship data...')
    message('This may take a moment...')
    table <- table %>%
      mutate(table, authorship = map_chr(taxonID, get_itis_authorship),
             validName = paste0(validName, " ", authorship))
  }
  
  # Create column to be used for joining accepted info to synonym rows
  # Prefix ID values with "provider_"
  table <- table %>% 
              mutate(joinCol = paste0(taxonomic_status, 
                                      gsub("[, ]", 
                                           "", 
                                           validName)
                                      ),
                     accid = paste0(provider, "_", accid),  # Prefix ID values with "provider_"
                     id = paste0(provider, "_", id)) %>%
  
              # Suffix all column names with "_provider"
              rename_with(~ paste0(., "_", provider), -joinCol)
  message('Done')
  return(table)
}