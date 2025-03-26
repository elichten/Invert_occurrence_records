merge_taxonomy <- function(table, table_names) {
  # Define the columns you want to coalesce
  columns <- c("flags", "taxonomic_status", "source", "accid", "id", 
               "kingdom", "phylum", "class", "order", "family", "subfamily", 
               "tribe", "subtribe", "validName", "canonical", "canonical_withFlags", 
               "genus", "subgenus", "species", "infraspecies", "authorship", 
               "taxon_rank", "valid", "notes", "canonical_accepted", "authorship_accepted")
  
  # Dynamically coalesce columns and perform the join at the same time
  mergedTaxonomy <- table
  
  for (column in columns) {

    # Dynamically generate column names for each source
    full_column_names <- paste0(column, "_", table_names)
    
    # Dynamically check for the existence of the columns and coalesce them
    mergedTaxonomy <- mergedTaxonomy %>%
      mutate(
        !!column := purrr::reduce(
          purrr::map(full_column_names, ~ {
            if (.x %in% colnames(table)) {
              return(mergedTaxonomy[[.x]])
            } else {
              return(NA)  # If column doesn't exist, return NA
            }
          }),
          ~ ifelse(!is.na(.x), .x, .y)  # Coalesce the values (first non-NA wins)
        )
      )
  }
  
  # Finally, select the desired columns
  mergedTaxonomy <- mergedTaxonomy %>%
    dplyr::select(all_of(columns), canonical_accepted, authorship_accepted)
}