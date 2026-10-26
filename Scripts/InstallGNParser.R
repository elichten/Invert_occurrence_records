# This script installs gnparser to a user folder, dependant on OS.
# It was taken from this guide from the BDC folks: https://github.com/brunobrr/bdc/blob/master/vignettes/help/installing_gnparser.Rmd
# See the referenced link for troubleshooting


install_gn_parser <- function() {
  # Operating system ("win", "mac" or "linux") and processor ("x86" or "arm")
  os <- switch(Sys.info()[["sysname"]],
               Windows = "win",
               Darwin = "mac",
               "linux"
  )
  arch <- if (R.version$arch %in% c("aarch64", "arm64")) "arm" else "x86"
  
  # Latest version of gnparser, e.g. "v1.15.1"
  version <- jsonlite::fromJSON(
    "https://api.github.com/repos/gnames/gnparser/releases/latest"
  )$tag_name
  
  # Downloads and extracts gnparser into a temporary folder
  extension <- if (os == "win") ".zip" else ".tar.gz"
  url <- paste0(
    "https://github.com/gnames/gnparser/releases/download/", version,
    "/gnparser-", version, "-", os, "-", arch, extension
  )
  archive <- file.path(tempdir(), basename(url))
  download.file(url, archive, mode = "wb")
  if (os == "win") {
    unzip(archive, exdir = tempdir())
  } else {
    untar(archive, exdir = tempdir())
  }
  
  # Copies gnparser to the folder where bdc looks for it
  folder <- switch(os,
                   win = file.path(Sys.getenv("APPDATA"), "gnparser"),
                   mac = "~/Library/Application Support/gnparser",
                   linux = "~/bin"
  )
  program <- if (os == "win") "gnparser.exe" else "gnparser"
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(tempdir(), program), folder, overwrite = TRUE)
  Sys.chmod(file.path(folder, program), mode = "755")
  
  # Shows the installed version: if you see a version number, it worked
  rgnparser::gn_version()
}
