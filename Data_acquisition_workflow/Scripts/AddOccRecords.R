## A function for importing occurrence data tables into a list of tables.
## This is to be used with the BeeBDC workflow, and facilitates importing
# one or more custom occurrence record tables.  
add_occ_records <- function(filepath, datasetName = "custom_dataset", mainList = list(), dataSource = "other") {
  
  # Read in the table using the provided filepath
  table <- suppressWarnings(
              readr::read_delim(filepath, col_types = BeeBDC::ColTypeR())
            )
  
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
  
  # TODO: Could this default to NA?
  if (!"id" %in% columnNames) {
    message("Creating id column...")
    tableWithIds <- tableWithIds %>%
      mutate(id = database_id)
  }
  # TODO: Could this default to NA?
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