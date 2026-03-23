# This script takes a filepath to a .zip file, unzips the contents in place,
# and deletes the original .zip file.
unzip_and_clean <- function(fp, files = NULL) {
  # Check if the file exists
  if (!file.exists(fp)) {
    stop("Error: The specified file does not exist.")
  }
  
  # Check if the file is a .zip
  if (tools::file_ext(fp) != "zip") {
    stop("Error: The specified file is not a .zip file.")
  }
  
  # Define the destination folder (same as the zip file directory)
  dest_folder <- dirname(fp)
  
  # Unzip the file
  unzip_path <- 
    tryCatch({
      zipName <- tools::file_path_sans_ext(basename(fp))
      unzip(fp, exdir = file.path(dest_folder, zipName), files, overwrite = TRUE)
      }, error = function(e) {
        stop("Error during unzipping: ", e$message)
      }
    )
  
  # Check if unzipping was successful (check if files were extracted)
  if (length(unzip_path) == 0) {
    stop("Error: No files extracted from the zip archive.")
  }
  
  # Delete the original .zip file if unzipping was successful
  file.remove(fp)
  
  # Return the path to the unzipped file(s)
  return(unzip_path)
}