# Script to check a taxon or observation table for required column names
check_col_names <- function(table, requiredColumns, desiredColumns = character()) {
  
  # Filter to only required/desired names which are NOT present in table
  missingRequired <- setdiff(requiredColumns, colnames(table))
  missingDesired <- setdiff(desiredColumns, colnames(table))
  
  # Warn about missing desired columns
  if (length(missingDesired) > 0) {
    cli::cli_alert_warning("{.strong The following columns are not required, but can be helpful:}")
    cli::cli_li(missingDesired)
  }
  
  # Report all missing required columns, then fail
  if (length(missingRequired) > 0) {
    cli::cli_abort(c(
      "{.strong The following required columns are missing from the table:}",
      x = missingRequired
    ))
  }
}


normalize_occ_table <- function(table, datasetName, dataSource) {
  
  # Columns with derived values
  if (!"database_id" %in% names(table)) {
    table <- table %>%
      mutate(database_id = paste0(
        datasetName, "_",
        replicate(nrow(table), uuid::UUIDgenerate(n=1, TRUE, "string"))
      ))
  }
  
  if (!"id" %in% names(table)) {
    message("Creating id column...")
    table <- table %>% mutate(id = database_id)
  }
  if (!"recordId" %in% names(table)) {
    message("Creating recordId column...")
    table <- table %>% mutate(recordId = database_id)
  }
  if (!"datasetName" %in% names(table)) {
    message("Creating datasetName column...")
    table <- table %>% mutate(datasetName = datasetName)
  }
  if (!"dataSource" %in% names(table)) {
    message("Creating dataSource column...")
    table <- table %>% mutate(dataSource = dataSource)
  }
  
  # Columns with default empty values
  defaults <- list(
    country = "",
    countryCode = "",
    fieldNotes = "",
    locationRemarks = "",
    verbatimEventDate = ""
  )
  
  for (nm in names(defaults)) {
    if (!nm %in% names(table)) {
      cli::cli_alert_info("Creating column {.field {nm}} with default value")
      table <- table %>% mutate(!!nm := defaults[[nm]])
    }
  }
  
  # Note: Some of these will never fail, as they are created with empty values
  # in the lines above. They'll remain in this list for documentation.
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
                       "day",
                       "fieldNotes",
                       "locationRemarks",
                       "verbatimEventDate")
  
  # Columns that BeeBDC functions would prefer, but that do not break the workflow
  desiredColumns <- c("otherCatalogNumbers",
                      "institutionCode",
                      "recordId")

  check_col_names(table, requiredColumns, desiredColumns)
  
  return(table)
}


# Taxon table
normalize_taxon_table <- function(table) {
  
  # Create "validName" column if doesn't exist
  if (!"validName" %in% names(table)) {
    message("Creating validName column using canonical and authorship columns...")
    table <- table %>%
      mutate(., validName = paste0(canonical, " ", ifelse(is.na(authorship), "", authorship)))
  }
  
  requiredColumns <- c("id",
                       "canonical",
                       "authorship",
                       "accid",
                       "taxonomic_status",
                       "source",
                       "taxon_rank",
                       "kingdom",
                       "phylum",
                       "class",
                       "order",
                       "family",
                       "genus",
                       "validName")
  
  # Check for required columns (this will stop execution if some are missing)
  check_col_names(table, requiredColumns)
  
  return(table)
}


# Checklist table (more of a basic check than normalization)
normalize_checklist <- function(table) {

  requiredColumns <- c("validName",
                       "iso_a3_eh",
                       "continent")
  
  # Check for required columns (this will stop execution if some are missing)
  check_col_names(table, requiredColumns)
  
  return(table)
}