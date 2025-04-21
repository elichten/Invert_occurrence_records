# Script check table for required column names
# TODO: This needs to error and not return anything if it breaks
check_col_names <- function(table, requiredColumns, desiredColumns = NULL) {
  
  # Filter to only required/desired names which are NOT present in table
  missingRequired <- setdiff(requiredColumns, colnames(table))
  missingDesired <- setdiff(desiredColumns, colnames(table))
  
  # Throw error if table is missing required names
  if (length(missingRequired) != 0) {
    cli::cli_alert_danger("{.strong The following required columns are missing from the data frame:}")
    
    # Show missing required columns with an error icon
    for (col in missingRequired) {
      cli::cli_alert_danger("    {col}")
    }
  }
  
  # Warn user about missing desired names
  if (length(missingDesired) != 0) {
    cli::cli_alert_warning("{.strong The following columns are not required, but can be helpful for this script:}")
    
    # Show missing desired columns with a warning icon
    for (col in missingDesired) {
      cli::cli_alert_warning("    {col}")
    }
  }
  
  # Exit if any missing required fields
  if (length(missingRequired) != 0) {
    stop("Missing required columns. Table not imported.")
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