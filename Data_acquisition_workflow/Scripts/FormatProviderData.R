format_provider_data <- function(table = NULL, provider = NULL, verbose = FALSE) {
  # Argument errors
  if (is.null(table)) {
    stop(" - Please provide a BeeBDC-formatted table")
  }
  if (is.null(provider)) {
    stop(" - Please indicate table source (gbif, itis, or dl)")
  }
  
  # Reformat provider name to allow for case differences, check if accepted
  provider <- tolower(provider)
  if (!provider %in% c("itis", "dl", "gbif")) {
    stop(" - Please indicate provider (gbif, itis, or dl)")
  }
  
  # Create column for accepted ids
  table <- table %>%
              left_join(dplyr::select(table, id, canonical, authorship),
                        by = c("accid" = "id"),
                        suffix = c("", "_accepted"))
  
  # TODO: Separate this function out
  # If provider is ITIS, get authorship information (this can take a long time)
  if (tolower(provider) == 'itis') {
    get_itis_authorship <- function(taxonID) {
      # Set maximum retry attempts for each tsn when encountering issues
      attempt <- 1
      max_attempts <- 10
      while (attempt <= max_attempts) {
        tryCatch({
          # Extract the TSN number from the taxonID string
          tsn <- stringr::str_extract(taxonID, "(?<=ITIS:)\\d+")
          
          # If no TSN can be extracted, return NA as value
          if (is.na(tsn)) {
            message("No TSN found. Returning NA.")
            return(NA)
          }
          
          # Call the ritis::taxon_authorship function with the extracted TSN
          if (verbose) {
            message("Getting authorship for tsn: ", tsn)
          }
          result <- ritis::taxon_authorship(as.numeric(tsn))
          
          # If successful, return authorship info
          return(result$authorship)
          
        # Catch errors (e.g., network issue, API issues) and retry
        }, error = function(e) {
          if (verbose) {
            message("Error in taxon_authorship: ", e$message)
            message("Attempt ", attempt, " of ", max_attempts, ". Retrying...")
          }
          
          # Increment attempt count and try again
          attempt <<- attempt + 1
        })
      }
      # Ultimately, if unsuccessful, return NA value
      message("No response in maximum attempts. Returning NA")
      return(NA)
    }
    message('Getting ITIS authorship data...')
    message('This may take a moment...')
    
    # Add authorship to table using map_chr (inital get_itis_authorship call)
    table <- table %>%
      mutate(table, authorship = map_chr(taxonID, get_itis_authorship, .progress = TRUE),
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