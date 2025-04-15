# Function to filter a taxonomic table to a list of species names or a list of genera
filter_by_taxon <- function(table = NULL, names = NULL, rank = NULL, includeSynonyms = TRUE) {
  filteredTable <- NULL
  taxaList <- c("kingdom", "phylum", "class", "order", "family",
                "subfamily", "tribe", "subtribe", "genus", 
                "subgenus", "species", "infraspecies")
  # Catch some missing arguments with custom messaging
  if (is.null(table)) {
    stop("Please provide a taxonomic table")
  }
  if (is.null(names)) {
    stop("Please provide a vector of scientific names (of the same rank)")
  }
  if ((!(tolower(rank) %in% taxaList)) | (is.null(rank)) | (length(rank) != 1)) {
    stop(paste0("Please provide a singlerank from within this list: ", taxaList))
  }
  tryCatch({
    table <- table %>%
      left_join(
        .,
        dplyr::select(table, all_of(c("id", "canonical", "authorship", taxaList))),
        by = c("accid" = "id"),
        suffix = c("", "_accepted")
      )
    if (tolower(rank) == "species") {
      filteredTable <- filter(table,
                             (tolower(canonical) %in% tolower(names)) | 
                             (tolower(canonical_accepted) %in% tolower(names))) # filter to rows related to those in sppList
    } else {
      rank_col <- tolower(rank)
      rank_accepted_col <- paste0(rank_col, "_accepted")

      filteredTable <- filter(table, 
                             (tolower(!!sym(rank_col)) %in% tolower(names)) |
                             (tolower(!!sym(rank_accepted_col)) %in% tolower(names)))
    }
      
  }, error = function(e) {
    stop(paste0("Error: ", e))
  })
  return(filteredTable)
}