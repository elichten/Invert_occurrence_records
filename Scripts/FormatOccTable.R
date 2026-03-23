format_occ_table <- function(table) {
  
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
                       "recordId")
  
  for (column in requiredColumns) {
    if (!column %in% names(table)) {
      message(paste0('"', column, '" ', "not found in table. Creating column with NA values."))
      table <- mutate(table, !!sym(column) := NA)
    }
  }
  
  return(table)
}