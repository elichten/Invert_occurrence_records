# This script is intended for the invert_occurrence_records workflow
# It facilitates importing custom occurrence record tables into a centralized 
# list of tables, to later be combined. It returns this centralized list

# If a datasetName is provided which already exists in the provided mainList,
# the new table will be combined with the preexisting dataset

# This function accepts either a table or a filepath argument
add_occ_records <- function(table = NULL, filepath = NULL, datasetName = "custom_dataset", mainList = list(), dataSource = "other", gbifKey = NULL) {
  
  # If user provided both filepath and table (or neither), prompt them for just one
  if ((is.null(filepath) && is.null(table)) || (!is.null(filepath) && !is.null(table))) {
    stop("Please provide either a filepath or a table (but not both!).")
  # If only filepath detected, initiate custom file import process
  } else if (!is.null(filepath)) {
    # Read in the table using the provided filepath
    table <- suppressWarnings(
      readr::read_delim(filepath, col_types = BeeBDC::ColTypeR())
    )
  }
  
  # Check required and desired columns
  normalizedTable <- normalize_occ_table(table, datasetName, dataSource)
  
  # If provided mainList doesn't exist, create one
  mainList <- tryCatch(mainList, error = function(e) list())
  
  # Add amended table to mainList
  if (!datasetName %in% names(mainList)) {
    mainList[[datasetName]] <- normalizedTable
  # If table of the same name already exists in mainList, combine the new and old tables
  } else {
    mainList[[datasetName]] <- bind_rows(mainList[[datasetName]], 
                                            normalizedTable)
  }
  
  return(mainList)
}