# Script check table for required column names
check_col_names <- function(table, requiredColumns, desiredColumns = NULL) {
  
  # Filter to only required/desired names which are NOT present in table
  missingRequired <- setdiff(requiredColumns, colnames(table))
  missingDesired <- setdiff(desiredColumns, colnames(table))
  
  # Warn user about missing desired names
  if (length(missingDesired) != 0) {
    warning(paste('The following columns are not required, but can be helpful for this script:\n\t', 
                  paste(missingDesired, collapse=", ")))
  }
  
  # Throw error if table is missing required names
  if (length(missingRequired) != 0) {
    stop(paste("The provided table is missing the following required columns:\n\t", 
               paste(missingRequired, collapse=", ")))
  }
  return(table)
}

# Occurrence table
check_occ_table <- function(table) {
  requiredColumns <- c("scientificName",
                       "decimalLatitude",
                       "decimalLongitude",
                       "basisOfRecord",
                       "countryCode",
                       "dataSource",
                       "country",
                       "database_id",
                       "locality",
                       "occurrenceStatus",
                       "id",
                       "catalogNumber",
                       "occurrenceID",
                       "eventDate",
                       "recordedBy",
                       "datasetName",
                       "coordinateUncertaintyInMeters",
                       "year",
                       "month",
                       "day")
  
  desiredColumns <- c("otherCatalogNumbers",
                      "institutionCode",
                      "recordId",
                      "fieldNotes",
                      "locationRemarks",
                      "verbatimEventDate")
  
  check_col_names(table, requiredColumns, desiredColumns)
  return(table)
}


# Taxon table
check_taxon_table <- function(table) {
  
  # TODO: Is the column "valid" required?
  # TODO: Is the column "flags" required?
  # TODO: Is the column "canonical_withFlags" required?
  requiredColumns <- c("id",
                       "canonical",
                       "authorship",
                       "accid",
                       "taxonomic_status",
                       "validName",
                       "source",
                       "taxon_rank",
                       "kingdom",
                       "phylum",
                       "class",
                       "order",
                       "family",
                       "genus")
  
  check_col_names(table, requiredColumns)
  return(table)
}