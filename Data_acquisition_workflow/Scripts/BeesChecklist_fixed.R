beesChecklist_fixed <- function(URL = "https://figshare.com/ndownloader/files/47092720", 
                                ...) 
{
  destfile <- checklist <- attempt <- nAttempts <- error_funcFile <- error_func <- NULL
  nAttempts = 5
  
  error_func <- function(e) {
    message(paste("Download attempt failed..."))
  }
  error_funcFile <- function(e) {
    message(paste("Could not read download..."))
  }
  
  checklist <- NULL
  attempt <- 1
  output_path = file.path(tempdir(), "beesChecklist.Rda")
  
  suppressWarnings(while (is.null(checklist) && attempt <= nAttempts) {
    if (attempt < nAttempts) {
      # Fix: mode needs to be set to "wb" for Windows machines
      tryCatch(utils::download.file(URL, 
                                    destfile = output_path,
                                    mode = "wb"),
               error = error_func, 
               warning = error_func)
      tryCatch(checklist <- readRDS(output_path),
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
  if (is.null(checklist)) {
    message(" - Checklist download failed. Please check your internet connection.")
  }
  return(checklist)
}