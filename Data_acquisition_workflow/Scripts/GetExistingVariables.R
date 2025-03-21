# Generalized function to filter existing variables and return the actual objects
getExistingVariables <- function(variableList) {
  
  # Check if the input is a character vector
  if (!is.character(variableList)) {
    stop("The input must be a character vector of possible environment variables.")
  }
  # Filter the variables that exist in the global environment
  existing_vars <- variableList[sapply(variableList, function(var) exists(var, envir = .GlobalEnv))]
  
  # Return a list of the actual objects
  objects <- lapply(existing_vars, function(var) get(var, envir = .GlobalEnv))
  
  return(objects)
}