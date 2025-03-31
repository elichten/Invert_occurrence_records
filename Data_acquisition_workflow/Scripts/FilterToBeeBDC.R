# Function to filter a table to columns which match BeeBDC::ColTypeR() columns
filter_to_BeeBDC <- function(table) {
  tableCols <- names(table)
  beeBDCCols <- names(BeeBDC::ColTypeR()$cols)
  sharedCols <- intersect(tableCols, beeBDCCols)
  
  table <- table %>%
    dplyr::select(all_of(sharedCols))
  
  return(table)
}