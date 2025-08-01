## A function for importing occurrence data tables into a list of tables.
## This is to be used with the BeeBDC workflow, and facilitates importing
# one or more custom occurrence record tables.
## This function will accept either a table or a filepath argument, but not both
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
  
  # TODO: Can we generate these AFTER the check_occ_table() call?
  # Add database_id column with <datasetName>_<uuid> format
  tableWithIds <- table %>%
    mutate(database_id = paste0(!!datasetName,
                                "_",
                                replicate(n(), 
                                          uuid::UUIDgenerate(use.time = TRUE, 
                                                             output = "string"))
    ))
  
  # Check a few columns that can be easily created if not provided
  # TODO: Are there any more we could add?
  columnNames <- names(table)
  
  if (!"id" %in% columnNames) {
    message("Creating id column...")
    tableWithIds <- tableWithIds %>%
      mutate(id = database_id)
  }
  if (!"recordId" %in% columnNames) {
    message("Creating recordId column...")
    tableWithIds <- tableWithIds %>%
      mutate(recordId = database_id)
  }
  if (!"country" %in% columnNames) {
    message("Creating empty country column...")
    tableWithIds <- tableWithIds %>%
      mutate(country = "")
  }
  # dataSource argument defaults to "other"
  if (!"dataSource" %in% columnNames){
    message("Creating dataSource column...")
    tableWithIds <- tableWithIds %>%
      mutate(dataSource = !!dataSource)
  }
  
  # Check required and desired columns
  check_occ_table(tableWithIds)
  
  # If provided mainList doesn't exist, create one
  mainList <- tryCatch(mainList, error = function(e) list())
  
  # Add amended table to mainList
  if (!datasetName %in% names(mainList)) {
    mainList[[datasetName]] <- tableWithIds
  # If table of the same name already exists in mainList, combine the new and old tables
  } else {
    mainList[[datasetName]] <- bind_rows(mainList[[datasetName]], 
                                            tableWithIds)
  }
  
  return(mainList)
}