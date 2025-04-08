format_gbif_import <- function(gbifTable) {
  gbifTable <- gbifTable %>%
    mutate(database_id = paste0("GBIF_4bees_",seq_len(nrow(gbifTable))),
           datasetID = "GBIF_4bees",
           dataSource = "GBIF",
           country = "",
           datasetName = datasetKey,
           id = gbifTable$gbifID) %>%
    filter_to_BeeBDC()
  
  return(gbifTable)
}