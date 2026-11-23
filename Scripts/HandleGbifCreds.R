# Check getPass is installed ----
if (!requireNamespace("getPass", quietly = TRUE)) {
  stop(
    "Package 'getPass' is required for GBIF credential entry. ",
    "Please install it using install.packages('getPass').",
    call. = FALSE
  )
}

# Function to set GBIF credentials in user-dialogue
set_gbif_creds <- function(creds) {
  # Iterate through credentials listed above, requesting user input
  # Set each in environment
  for (credential in creds) {
    user_input <- if (credential == "GBIF_PWD") {
      getPass::getPass(msg = sprintf("Enter %s: ", credential))
    } else {
      readline(prompt = sprintf("Enter %s: ", credential))
    }
    # getPass returns NULL on cancel
    if (is.null(user_input)) stop("Credential entry cancelled.", call. = FALSE)
    do.call(Sys.setenv, setNames(list(user_input), credential))
  }
}

# Check if GBIF environment variables are already set
# If credentials already in environment, ask to use them, else call set_gbif_creds()
check_gbif_creds <- function() {
  # List of necessary GBIF credentials
  GBIF_creds <- list("GBIF_USER", "GBIF_PWD", "GBIF_EMAIL")
  if (all(nzchar(Sys.getenv(GBIF_creds)))) {
    user_choice <- menu(c("Yes", "No"), title = sprintf("Use credentials for %s?", Sys.getenv("GBIF_USER")))
    # If yes, keep current env credentials
    if (user_choice == 1) {
      print(sprintf("Using credentials for %s", Sys.getenv("GBIF_USER")))
      # Else, ask for new credentials
    } else if (user_choice == 2) {
      set_gbif_creds(GBIF_creds)
    }
    # If credentials not set, ask for credentials
  } else {
    set_gbif_creds(GBIF_creds)
  }
}
