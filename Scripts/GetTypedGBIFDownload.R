# Script to wait for gbif download, then unzip and find occurrence table
# using a GBIF occurrence download key. This script is designed to work 
# with the invert_occurrence_records workflow, and assumes access to 
# User_Data directory, created in this workflow.

# This script can handle DWCA or simple_csv download types, and will only
# extract the occurrences.txt file from a DWCA download
get_typed_gbif_download <- function(downloadKey) {
  
  # Wait for gbif download to be ready
  occ_download_wait(downloadKey)

  outPath = UserDataPath
  
  tryCatch(
    {
      fp <- file.path(outPath, downloadKey)

      message("Getting GBIF download...")
      zipFp <- occ_download_get(key=downloadKey, path=outPath, overwrite=TRUE)
      
      # Check if DWCA or simple_csv
      isDWCA <- "occurrence.txt" %in% (unzip(file.path(zipFp), list = TRUE)$Name)
      # If not DWCA, unzip .csv and rename
      if (!isDWCA) {
        temp_fp <- unzip_and_clean(zipFp)
        file.rename(from=temp_fp, to=file.path(fp, paste0(downloadKey, "_SIMPLE.csv")))
        fp <- file.path(fp, paste0(downloadKey, "_SIMPLE.csv"))
        # Else, only retrieve occurrence.txt, and rename to 
      } else if (isDWCA) {
        temp_fp <- unzip_and_clean(zipFp, files="occurrence.txt")
        file.rename(from=temp_fp, to=file.path(fp, paste0(downloadKey, "_DWCA.csv")))
        fp <- file.path(fp, paste0(downloadKey, "_DWCA.csv"))
      }
      return(fp)
    }, error = function(e) {
      stop(paste0("Error: ", e))
    }
  )

  
  return(typedTable)
}