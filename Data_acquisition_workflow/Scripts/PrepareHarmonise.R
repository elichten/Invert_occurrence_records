# Function which check table for two special column names needed to use BeeBDC::harmoniseR
# and adds them to the provided table if needed
prepare_harmonise <- function(table) {
  
  # Define which columns we're checking for
  columns_to_check <- c("flags", "canonical_withFlags")
  
  corrected_table <- table
  
  # Check if columns exist in table
  for (column in columns_to_check) {
    if (!column %in% colnames(table)) {
      corrected_table <- corrected_table %>%
        mutate(!!sym(column) := "")
      message(paste0(column, " column is missing. Adding to table with empty values."))
    }
  }
  
  return(corrected_table)
  
}