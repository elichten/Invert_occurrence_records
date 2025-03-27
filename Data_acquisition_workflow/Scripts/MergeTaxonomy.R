merge_taxonomy <- function(table, table_names) {
  # Define the columns you want to coalesce
  columns <- c("flags", "taxonomic_status", "source", "accid", "id", 
               "kingdom", "phylum", "class", "order", "family", "subfamily", 
               "tribe", "subtribe", "validName", "canonical", "canonical_withFlags", 
               "genus", "subgenus", "species", "infraspecies", "authorship", 
               "taxon_rank", "valid", "notes", "canonical_accepted", "authorship_accepted")
  
  mergedTaxonomy <- table
  
  for (column in columns) {
    # Create full column names for each source (e.g., "scientificName_gbif", "scientificName_itis")
    full_column_names <- paste0(column, "_", table_names)
    
    # Check if the columns exist in the table, if not, replace with NA
    existing_columns <- purrr::map(full_column_names, ~ if (.x %in% colnames(table)) .x else NA)
    
    # Remove NAs using purrr::discard to get valid column names
    existing_columns_char <- purrr::discard(existing_columns, is.na)
    
    # Dynamically generate the merged column by coalescing (only valid columns will be kept)
    mergedTaxonomy <- mergedTaxonomy %>%
      mutate(
        !!column := coalesce(!!!syms(existing_columns_char))  # Coalesce across the columns, handling missing columns as NA
      )
  }
  
  # Finally, select the columns we want to keep (including coalesced ones)
  mergedTaxonomy <- mergedTaxonomy %>%
    dplyr::select(all_of(columns), canonical_accepted, authorship_accepted)
  
  return(mergedTaxonomy)
}