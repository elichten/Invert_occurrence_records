# Format a previously imported gbif table by adding unique database_id values,
# datasetID = "GBIF", etc, before filtering to BeeBDC-desired columns.

format_gbif_import <- function(gbifTable) {
  gbifTable <- gbifTable %>%
    mutate(database_id = paste0("GBIF_", 
                                uuid::UUIDgenerate(n = n(), output = "string")),
           datasetID = "GBIF",
           dataSource = "GBIF",
           country = "",
           datasetName = datasetKey,
           id = gbifID) %>%
    filter_to_BeeBDC()
  
  return(gbifTable)
}

import_gbif_file <- function(fp, chunk_size=100000) {
  # Read in file with BeeBDC column types
  
  chunk_list <- list()
  
  message("Reading in occurrence file using BeeBDC column types...")
  read_delim_chunked(
    fp,
    delim = '\t',
    col_types = BeeBDC::ColTypeR()$cols,
    callback = SideEffectChunkCallback$new(function(chunk, pos) {
      chunk_list[[length(chunk_list) + 1]] <<- format_gbif_import(chunk)
    }),
    chunk_size = chunk_size
  )
  
  formattedTable <- data.table::rbindlist(chunk_list, use.names = TRUE, fill = TRUE)
  rm(chunk_list); gc()
  
  return(formattedTable)
}