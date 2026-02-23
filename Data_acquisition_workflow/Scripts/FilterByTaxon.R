# Function to filter a taxonomic table to a list of desired taxonomic names of a given rank
filter_by_taxon <- function(table = NULL, names = NULL, rank = NULL) {
  filteredTable <- NULL
  taxaList <- c("kingdom", "phylum", "class", "order", "family",
                "subfamily", "tribe", "subtribe", "genus", 
                "subgenus", "species", "infraspecies")
  
  # Catch missing arguments with messaging
  # If no table provided
  if (is.null(table)) {
    stop("Please provide a taxonomic table")
  }
  # If no desired taxa provided
  if (is.null(names)) {
    stop("Please provide a vector of scientific names (of the same rank)")
  }
  # If the user did not provide exactly one rank, or it is not found in approved list of ranks
  if ((!(tolower(rank) %in% taxaList)) | (is.null(rank)) | (length(rank) != 1)) {
    stop(paste0("Please provide a single rank from within this list: ",
                paste(taxaList, collapse = ", ")))
  }
  
  rank <- tolower(rank)

  tryCatch({
    table <- table %>%
      # Create canonical_accepted column using id, canonical, and authorship columns
      left_join(
        .,
        dplyr::select(table, all_of(c("id", "canonical", "authorship", rank))),
        by = c("accid" = "id"),
        suffix = c("", "_accepted_tmp")
      )
    # If rank is species, look to canonical and canonical_accepted columns
    if (rank == "species") {
      filteredTable <- filter(table,
                             (tolower(canonical) %in% tolower(names)) | 
                             (tolower(canonical_accepted_tmp) %in% tolower(names)))
    # If another rank is requested, filter to column of that name
    } else {

      filteredTable <- filter(table, 
                             (tolower(.data[[rank]]) %in% tolower(names)) |
                             (tolower(.data[[paste0(rank, "_accepted_tmp")]]) %in% tolower(names)))
    }
    
    # Drop _accepted columns before returning
    filteredTable <- filteredTable %>%
      dplyr::select(-dplyr::ends_with("_accepted_tmp"))
      
  }, error = function(e) {
    stop(paste0("Error: ", e))
  })
  return(filteredTable)
}