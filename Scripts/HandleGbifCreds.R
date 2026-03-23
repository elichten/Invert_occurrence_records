# Function to set GBIF credentials in user-dialogue
set_gbif_creds <- function(creds) {
  # Iterate through credentials listed above, requesting user input
  # Set each in environment
  for (credential in creds) {
    user_input <- readline(prompt=sprintf("Enter %s: ", credential))
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
