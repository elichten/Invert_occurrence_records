# Format a previously imported gbif table by adding unique database_id values,
# datasetID = "GBIF", etc, before filtering to BeeBDC-desired columns.

format_gbif_import <- function(gbifTable) {
  gbifTable <- gbifTable %>%
    mutate(database_id = paste0("GBIF_", 
                                replicate(n(), 
                                          uuid::UUIDgenerate(use.time = TRUE, 
                                                             output = "string"))
                                ),
           datasetID = "GBIF",
           dataSource = "GBIF",
           country = "",
           datasetName = datasetKey,
           id = gbifID) %>%
    filter_to_BeeBDC()
  
  return(gbifTable)
}