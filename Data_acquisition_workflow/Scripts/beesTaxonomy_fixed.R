beesTaxonomyFixed <- function (URL = "https://open.flinders.edu.au/ndownloader/files/47089969", 
          ...) 
{
  destfile <- taxonomy <- attempt <- nAttempts <- error_funcFile <- error_func <- NULL
  nAttempts = 5
  
  error_func <- function(e) {
    message(paste("Download attempt failed..."))
  }
  error_funcFile <- function(e) {
    message(paste("Could not read download..."))
  }

  taxonomy <- NULL
  attempt <- 1
  output_path = file.path(tempdir(), "beesTaxonomy.Rda")
  
  suppressWarnings(while (is.null(taxonomy) && attempt <= nAttempts) {
    if (attempt < nAttempts) {
        tryCatch(utils::download.file(URL, 
                                      destfile = output_path,
                                      mode = "wb"),
                 error = error_func, 
                 warning = error_func)
        tryCatch(taxonomy <- readRDS(output_path),
                 error = error_funcFile,
                 warning = error_funcFile)
    }
    if (attempt < nAttempts) {
      if (attempt > 1) {
        Sys.sleep(1)
        print(paste("Attempt: ", attempt, " of ", nAttempts - 
                      1))
      }
    }
    attempt <- attempt + 1
  })
  if (is.null(taxonomy)) {
    message(" - Taxonomy download failed. Please check your internet connection.")
  }
  return(taxonomy)
}