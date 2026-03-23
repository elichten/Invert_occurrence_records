merge_taxonomy <- function(table, table_names) {
  # Define the columns you want to coalesce
  columns <- c("flags", "taxonomic_status", "source", "accid", "id", 
               "kingdom", "phylum", "class", "order", "family", "subfamily", 
               "tribe", "subtribe", "validName", "canonical", "canonical_withFlags", 
               "genus", "subgenus", "species", "infraspecies", "authorship", 
               "taxon_rank", "valid", "notes", "canonical_accepted", "authorship_accepted")
  
  mergedTaxonomy <- table
  
  # Perform the big merge
  for (column in columns) {
    # Create full column names for each source (e.g., "canonical_gbif", "canonical_itis")
    full_column_names <- paste0(column, "_", table_names)
    
    # Check if the columns exist in the table, if not, replace with NA
    existing_columns <- purrr::map(full_column_names, ~ ifelse(.x %in% colnames(table), .x, NA_character_))
    
    # Remove NAs using purrr::discard to get valid column names
    existing_columns_char <- purrr::discard(existing_columns, is.na)
    
    # Dynamically generate the merged column by coalescing (only valid columns will be kept)
    mergedTaxonomy <- mergedTaxonomy %>%
      mutate(
        !!sym(column) := coalesce(!!!syms(existing_columns_char))  # Coalesce across the columns, handling missing columns as NA
      )
  }
  
  # Finally, select the columns we want to keep (including coalesced ones)
  mergedTaxonomy <- mergedTaxonomy %>%
    dplyr::select(all_of(columns), canonical_accepted, authorship_accepted)
  
  # Check for duplicate canonical names where at least one is lacking authorship while another has it
  # In addition, filter only to duplicates coming from differing sources
  duplicateTable <- mergedTaxonomy %>%
    group_by(canonical) %>%
    filter(n() > 1, 
           n_distinct(source) > 1,
           any(!is.na(authorship)) & any(is.na(authorship))) %>%
    ungroup()
  
  duplicateCount <- duplicateTable %>%
    dplyr::count(canonical) %>%
    nrow(.)
  
  if (duplicateCount > 0) {
    repeat {
      choice <- menu(c("Yes", "No", "See duplicates table", "Exit"), 
                     title=paste0(duplicateCount, " potential taxonomic duplicates were found where one duplicate contains authorship information, while another does not. This could cause issues in the workflow later. Would you like to remove those duplicates that lack authorship?"))
      # If "Yes"
      if (choice == 1) {
        # Get duplicates found in duplicateTable that do not have authorship
        toRemove <- duplicateTable %>%
          filter(is.na(authorship))
        # Remove them
        mergedTaxonomy <- anti_join(mergedTaxonomy, 
                                    toRemove, 
                                    by = colnames(mergedTaxonomy))
        break
      }
      # If "No"
      else if (choice == 2) {
        # Do nothing 
        break
      }
      # If "See duplicates table"
      else if (choice == 3) {
        view(duplicateTable)
      } 
      # If "Exit", quit out of process
      else if (choice == 4) {
        stop("User cancelled operation.")
      }
    }
  }

  
  return(mergedTaxonomy)
}