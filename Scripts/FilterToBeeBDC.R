# Function to filter a table to columns which match BeeBDC::ColTypeR() columns
filter_to_BeeBDC <- function(table) {
  tableCols <- names(table)
  beeBDCCols <- names(BeeBDC::ColTypeR()$cols)
  sharedCols <- intersect(tableCols, beeBDCCols)
  
  if (length(sharedCols) == 0) {
    warning("No columns match BeeBDC::ColTypeR(). Returning empty table.")
    return(table[ , 0])
  }
  
  table <- table %>%
    dplyr::select(all_of(sharedCols))
  
  return(table)
}