# Script to wait for gbif download, and then import gbif table with 
# BeeBDC column typing using a GBIF occurrence download key.

# This script can handle DWCA or simple_csv download types, and will only
# extract the occurrences.txt file from a DWCA download
get_typed_gbif_download <- function(downloadKey) {
  
  # Wait for gbif download to be ready
  # TODO: The messaging from this function is unclear and confusing.
  occ_download_wait(downloadKey)
  
  outPath = file.path(DataPath, "Additional_Datasets")

  tryCatch(
    {
      fp <- file.path(outPath, downloadKey)

      zipFp <- occ_download_get(key=downloadKey, path=outPath, overwrite=TRUE)
      
      # Check if DWCA or simple_csv
      isDWCA <- "occurrence.txt" %in% (unzip(file.path(zipFp), list = TRUE)$Name)
      # If not DWCA, unzip .csv and rename
      if (!isDWCA) {
        temp_fp <- unzip_and_clean(zipFp)
        file.rename(from=temp_fp, to=file.path(fp, paste0(downloadKey, "_SIMPLE.csv")))
        fp <- file.path(fp, paste0(downloadKey, "_SIMPLE.CSV"))
        # Else, only retrieve occurrence.txt, and rename to 
      } else if (isDWCA) {
        temp_fp <- unzip_and_clean(zipFp, files="occurrence.txt")
        message(temp_fp, "HERE IT IS")
        file.rename(from=temp_fp, to=file.path(fp, paste0(downloadKey, "_DWCA.csv")))
        fp <- file.path(fp, paste0(downloadKey, "_DWCA.CSV"))
      }

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