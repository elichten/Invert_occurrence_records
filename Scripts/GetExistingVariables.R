# Generalized function to filter existing variables and return list of existing names
get_existing_variables <- function(variableList) {
  
  # Check if the input is a character vector
  if (!is.character(variableList)) {
    stop("The input must be a character vector of possible environment variables.")
  }
  # Filter the variables that exist in the global environment
  existing_vars <- variableList[sapply(variableList, function(var) exists(var, envir = .GlobalEnv))]
  
  return(existing_vars)
}