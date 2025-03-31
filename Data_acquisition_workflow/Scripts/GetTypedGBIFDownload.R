# Script to wait for gbif download, and then import gbif table with 
# BeeBDC column typing using a GBIF occurrence download key
get_typed_gbif_download <- function(downloadKey) {
  
  # Wait for gbif download to be ready
  # TODO: The messaging from this function is unclear and confusing.
  occ_download_wait(downloadKey)
  
  outPath = file.path(DataPath, "Additional_Datasets")

  tryCatch(
    {
      # Get gbif download
      zipFp <- occ_download_get(key=downloadKey, path=outPath)
      
      # Unzip and erase old .zip
      message("Unzipping download file...")
      fp <- unzip_and_clean(file.path(zipFp))
      
      # Read in file with BeeBDC column types
      message("Reading in occurrence file using BeeBDC column types...")
      typedTable <- suppressWarnings(
            read_delim(fp, col_types=c(BeeBDC::ColTypeR()$cols))
      )
    }, error = function(e) {
      stop(e)
    }
  )

  
  return(typedTable)
}