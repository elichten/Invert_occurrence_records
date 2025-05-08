# This function was created by James Dorey on the 26th of May 2022. It will attempt to find dates
# that dont occur in the EventDate column and restore them to avoid losing those occurrences in 
# filtering. 
# For questions, ask James Dorey at jbdorey[at]me.com

#' Find dates in other columns
#' 
#' A function made to search other columns for dates and add them to the eventDate column. 
#' The function searches the columns locality, fieldNotes, locationRemarks, and verbatimEventDate 
#' for the relevant information.
#'

dateFindR_fixed2 <- function(data = NULL,
                      maxYear = lubridate::year(Sys.Date()),
                      minYear = 1700) {
  # locally bind variables to the function
  eventDate<-database_id<-.<-verbatimEventDate<-fieldNotes<-locationRemarks<-ymd_vEV<-
    ymd_fieldNotes<-ymd_locationRemarks<-locality<-dmy_vEV<-dmy_locality<-dmy_fieldNotes<-
    dmy_locationRemarks<-mdy_vEV<-mdy_locality<-mdy_fieldNotes<-mdy_locationRemarks<-my_vEV<-
    my_locality<-my_fieldNotes<-my_locationRemarks<-amb_vEV<-amb_locality<-amb_fieldNotes<-
    amb_locationRemarks<-year <- endTime <- startTime <- originalDateCount <- 
    eventDate_in <- day <- NULL
  
  # load required packages
  requireNamespace("dplyr")
  requireNamespace("lubridate")
  requireNamespace("mgsub")
  
  timeStart <- Sys.time()
  
  #### 0.0 prep ####
  writeLines(" - Preparing data...")
  # Get a count of how many eventDate rows are full
  originalDateCount <- sum(complete.cases(data$eventDate))
  # Create a new running dataset
  noDATEa <- data %>%
    # Save the original eventDate column
    dplyr::mutate(eventDate_in = eventDate,
                  .before = eventDate) %>%
    dplyr::mutate(eventDate = eventDate %>% 
                    lubridate::parse_date_time(eventDate_in, 
                                               orders = c("ymd", "ymdHMS","dmy","mdy"),
                                               truncated = 5,
                                               quiet = TRUE,
                                               tz = "UTC",
                                               locale = Sys.getlocale("LC_TIME"))) %>%
    dplyr::mutate(eventDate = dplyr::if_else(is.na(eventDate),
                                             lubridate::ymd_hms(eventDate_in, quiet = TRUE,
                                                                truncated = 5),
                                             eventDate)) %>%
    dplyr::mutate(dateSuccess = dplyr::if_else(is.na(eventDate),
                                               FALSE,
                                               TRUE)) 
  
  # Save this dataset to be merged at the end...
  ymd_hms_0 <- noDATEa %>%
    dplyr::filter(complete.cases(eventDate)) %>%
    dplyr::select(database_id, eventDate) %>%
    setNames(object = ., c("database_id", "date"))
  
  #### 1.0 easyDates ####
  # Retrieve dates that are much easier to recover...
  writeLines(" - Extracting dates from year, month, day columns...")
  ##### 1.1 year month day ####
  # Filter down to the records that again have no eventDate
  noDATEa <- noDATEa %>%
    dplyr::filter(is.na(eventDate))
  # Some records have date information in the dmy columns that can easily be retrieved
  noDATEa <- noDATEa %>%
    dplyr::mutate(eventDate = dplyr::if_else(is.na(as.character(eventDate)),
                                             lubridate::ymd(stringr::str_c(year, month, day,
                                                                           sep = "-"),
                                                            quiet = TRUE, truncated = 2),
                                             eventDate))
  
  # Save this dataset to be merged at the end...
  dmy_1 <- noDATEa %>%
    dplyr::filter(complete.cases(eventDate)) %>%
    dplyr::select(database_id, eventDate) %>%
    setNames(object = ., c("database_id", "date"))
  
  # Filter down to the records that again have no eventDate
  noDATEa <- noDATEa %>%
    dplyr::filter(is.na(eventDate))
  if("occurrenceYear" %in% colnames(noDATEa)){
    # Copy across the occurrenceYear column into the eventDate column
    noDATEa$eventDate <- noDATEa$occurrenceYear}
  # Save this dataset to be merged at the end...
  occYr_2 <- noDATEa %>%
    dplyr::filter(complete.cases(eventDate))%>%
    dplyr::select(database_id, eventDate) %>%
    setNames(object = ., c("database_id", "date"))
  
  ##### 1.1 Sept ####
  # Because some people write "Sept" which cannot be read by lubridate, it needs to be 
  # replaced in these columns
  noDATEa$locality <- noDATEa$locality %>%
    stringr::str_replace(pattern = "[Ss]ept[\\s-/]",
                         replacement = "Sep ")
  noDATEa$fieldNotes <- noDATEa$fieldNotes %>%
    stringr::str_replace(pattern = "[Ss]ept[\\s-/]",
                         replacement = "Sep ")
  noDATEa$locationRemarks <- noDATEa$locationRemarks %>%
    stringr::str_replace(pattern = "[Ss]ept[\\s-/]",
                         replacement = "Sep ")
  noDATEa$verbatimEventDate <- noDATEa$verbatimEventDate %>%
    stringr::str_replace(pattern = "[Ss]ept[\\s-/]",
                         replacement = "Sep ")
  
  
  #### 2.0 unAmb. str. dates ####
  writeLines(paste(
    " - Extracting dates from fieldNotes, locationRemarks, and verbatimEventDate ",
    "columns in unambiguous ymd, dmy, mdy, and my formats...", sep = ""))
  # Filter down to the records that again have no eventDate
  noDATEa <- noDATEa %>%
    dplyr::filter(is.na(eventDate))
  
  monthStrings <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct","Nov", "Dec",
                    "jan", "feb", "mar", "apr", "may", "jun",
                    "jul", "aug", "sep", "oct","nov", "dec",
                    "January", "February", "March", "April",
                    "May", "June","July","August",
                    "September","October","November","December",
                    "january", "february", "march", "april",
                    "may", "june","july","august",
                    "september","october","november","december",
                    "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                    "JUL", "AUG", "SEP", "OCT","NOV", "DEC",
                    "JANUARY", "FEBRUARY", "MARCH", "APRIL",
                    "MAY", "JUNE","JULY","AUGUST",
                    "SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER",
                    "sept")
  romanNumerals <- c("i","ii","iii","iv","v","vi","vii","viii","ix","x","xi","xii",
                     "I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII")
  numeralConversion <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                         "Jul", "Aug", "Sep", "Oct","Nov", "Dec",
                         "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                         "Jul", "Aug", "Sep", "Oct","Nov", "Dec")
  
  ##### 2.1 ymd ####
  # CHANGE: General changes to have the regex is written.
  # enter ymd strings...
  ymd_strings <- c(
    # Numeric: YYYY-MM-DD with day > 12
    "[0-9]{4}[-/\\. ]+(0[1-9]|1[0-2])[-/\\. ]+(1[3-9]|2[0-9]|3[01])",
    # Text month
    paste0("[0-9]{4}[-/\\. ]+(?:", monthStrings, ")[-/\\. ]+[0-9]{1,2}\\b"),
    # Roman month
    paste0("[0-9]{4}[-/\\. ]+(?:", romanNumerals, ")[-/\\. ]+[0-9]{1,2}\\b"),
    # Textual month with day range, e.g., "2020 May 14–16"
    paste0("[0-9]{4}[-/\\. ]+(?:", monthStrings, ")[ ]?[0-9]+[ ]*[-\\u2013][ ]*[0-9]+\\b")
  )
  
  # Extract the matching strings
  ymd_unambiguous <- noDATEa %>%
    dplyr::mutate(
      ymd_vEV = stringr::str_extract(verbatimEventDate,
                                     pattern = paste(ymd_strings, 
                                                     collapse = "|", sep = "")),
      ymd_fieldNotes = stringr::str_extract(fieldNotes,
                                            pattern = paste(ymd_strings, 
                                                            collapse = "|", sep = "")),
      ymd_locationRemarks = stringr::str_extract(locationRemarks,
                                                 pattern = paste(ymd_strings, 
                                                                 collapse = "|", sep = ""))
    ) %>% # END mutate
    dplyr::select(database_id, 
                  verbatimEventDate,
                  ymd_vEV,
                  ymd_fieldNotes, 
                  ymd_locationRemarks)  
  
  # CHANGE: These were lacking roman number substitutions.
  # FORMAT the ymd_vEV column
  ymd_unambiguous$ymd_vEV <- ymd_unambiguous$ymd_vEV %>%
    # Convert roman numerals to readable by lubridate
    mgsub::mgsub(
      pattern = paste("[-/\\. ]", romanNumerals, "[ -/]", sep = ""),
      replacement = numeralConversion) %>%
    # Remove 00 values to truncate the lubridate 
    stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>%
    lubridate::ymd(truncated = 2, quiet = TRUE) 
  #
  # FORMAT the ymd_fieldNotes column
  ymd_unambiguous$ymd_fieldNotes <- ymd_unambiguous$ymd_fieldNotes %>%
    mgsub::mgsub(
      pattern = paste("[-/\\. ]", romanNumerals, "[ -/]", sep = ""),
      replacement = numeralConversion) %>%
    # Remove 00 values to truncate the lubridate 
    stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>%
    lubridate::ymd(truncated = 2, quiet = TRUE) 
  #
  # FORMAT the ymd_locationRemarks column
  ymd_unambiguous$ymd_locationRemarks <- ymd_unambiguous$ymd_locationRemarks %>%
    mgsub::mgsub(
      pattern = paste("[-/\\. ]", romanNumerals, "[ -/]", sep = ""),
      replacement = numeralConversion) %>%
    # Remove 00 values to truncate the lubridate 
    stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>%
    lubridate::ymd(truncated = 2, quiet = TRUE) 
  #
  # Combine the columns
  ymd_keepers_21 <- ymd_unambiguous %>%
    dplyr::filter(complete.cases(ymd_vEV)|
                    complete.cases(ymd_locationRemarks) | complete.cases(ymd_fieldNotes)) %>%
    tidyr::unite(col = date,
                 ymd_vEV, ymd_locationRemarks, ymd_fieldNotes, na.rm = TRUE)
  # add ymd_keepers_21 at the end
  
  
  # CHANGE: When patterns contain an internal pipe ("|"), they'll need to be encapsulated in parentheses
  # CHANGE: General changes to have the regex is written.
  ##### 2.2 dmy ####
  dmy_strings <- c(
    # Numeric dmy, unambiguous: day > 12, allowing spaces as separator
    "([1][3-9]|2[0-9]|3[01])[-/\\. ]+([1-9]|0[1-9]|1[0-2])[-/\\. ]+[0-9]{4}",
    # Text month (with hyphen support)
    paste0("[0-9]{1,2}[-/\\.\\- ]+", monthStrings, "[-/\\.\\- ]+[0-9]{4}"),
    # Roman numeral month
    paste0("[0-9]{1,2}[-/\\. ]+", romanNumerals, "[-/\\. ]+[0-9]{4}"),
    # Textual month with day range, e.g. "14–16 May 2020"
    paste0("[0-9]+[-\\u2013][0-9]+[ ]*", monthStrings, "[-/\\. ]+[0-9]{4}")
  )
  
  # Extract the matching strings
  dmy_unambiguous <- noDATEa %>%
    # First, remove the strings matched prior
    dplyr::filter(!database_id %in% ymd_keepers_21$database_id) %>%
    dplyr::mutate(
      dmy_vEV = stringr::str_extract(verbatimEventDate,
                                     pattern = paste(dmy_strings, 
                                                     collapse = "|", sep = "")),
      dmy_locality = stringr::str_extract(locality,
                                          pattern = paste(dmy_strings, 
                                                          collapse = "|", sep = "")),
      dmy_fieldNotes = stringr::str_extract(fieldNotes,
                                            pattern = paste(dmy_strings, 
                                                            collapse = "|", sep = "")),
      dmy_locationRemarks = stringr::str_extract(locationRemarks,
                                                 pattern = paste(dmy_strings, 
                                                                 collapse = "|", sep = ""))
    ) %>% # END mutate
    dplyr::select(database_id, dmy_vEV, dmy_locality,
                  dmy_fieldNotes, dmy_locationRemarks, verbatimEventDate)  
  
  # FORMAT the dmy_vEV column
  dmy_unambiguous$dmy_vEV <- dmy_unambiguous$dmy_vEV %>%
    # Convert roman numerals to readable by lubridate
    mgsub::mgsub(
      pattern = paste("[-/\\. ]", romanNumerals, "[ -/]", sep = ""),
      replacement = numeralConversion) %>%
    # Remove 00 values to truncate the lubridate 
    stringr::str_replace(pattern = "/00$|/00/00$|^00", replacement = "") %>%
    lubridate::dmy(truncated = 2, quiet = TRUE) 
  #
  # FORMAT the dmy_locality column
  dmy_unambiguous$dmy_locality <- dmy_unambiguous$dmy_locality %>%
    # Convert roman numerals to readable by lubridate
    mgsub::mgsub(
      pattern = paste("[-/\\. ]", romanNumerals, "[ -/]", sep = ""),
      replacement = numeralConversion) %>%
    # Remove 00 values to truncate the lubridate 
    stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>%
    lubridate::dmy(truncated = 2, quiet = TRUE) 
  #
  # FORMAT the dmy_fieldNotes column
  dmy_unambiguous$dmy_fieldNotes <- dmy_unambiguous$dmy_fieldNotes %>%
    # Convert roman numerals to readable by lubridate
    mgsub::mgsub(
      pattern = paste("[-/\\. ]", romanNumerals, "[ -/]", sep = ""),
      replacement = numeralConversion) %>%
    # Remove 00 values to truncate the lubridate 
    stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>%
    lubridate::dmy(truncated = 2, quiet = TRUE) 
  #
  # FORMAT the dmy_locationRemarks column
  dmy_unambiguous$dmy_locationRemarks <- dmy_unambiguous$dmy_locationRemarks %>%
    # Convert roman numerals to readable by lubridate
    mgsub::mgsub(
      pattern = paste("[-/\\. ]", romanNumerals, "[ -/]", sep = ""),
      replacement = numeralConversion) %>%
    # Remove 00 values to truncate the lubridate 
    stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>%
    lubridate::dmy(truncated = 2, quiet = TRUE) 
  #
  # Combine the columns
  dmy_keepers_22 <- dmy_unambiguous %>%
    dplyr::filter(complete.cases(dmy_vEV) |
                    complete.cases(dmy_locality) | 
                    complete.cases(dmy_locationRemarks) | 
                    complete.cases(dmy_fieldNotes)) %>%
    tidyr::unite(col = date, 
                 dmy_vEV, dmy_locality, dmy_locationRemarks, dmy_fieldNotes, 
                 na.rm = TRUE)
  # add dmy_keepers_22 at the end
  
  # CHANGE: When patterns contain an internal pipe ("|"), they'll need to be encapsulated in parentheses
  # CHANGE: General changes to have the regex is written.
  ##### 2.3 mdy ####
  mdy_strings <- c(
    # Numeric month with day > 12, allowing both single and double digit months
    "(?<![0-9])(?:0?[1-9]|1[0-2])[-/\\. ]+(?:1[3-9]|2[0-9]|3[01])[-/\\. ]+[0-9]{4}(?![0-9])",
    # Text month with day and 4-digit year
    paste0("(?:", monthStrings, ")[-/\\. ]+[0-9]{1,2}[-/\\. ]+[0-9]{4}\\b"),
    # Roman numeral month (e.g., "IV 5 2020")
    paste0("\\b(?:", romanNumerals, ")\\b[-/\\. ]+[0-9]{1,2}[-/\\. ]+[0-9]{4}"),
    # Textual day ranges (e.g., "May 5–7 2020")
    paste0("(?:", monthStrings, ")[ ]?[0-9]+[ ]*[-\\u2013][ ]*[0-9]+[-/\\. ]+[0-9]{4}\\b")
  )
  
  # Get the IDs to remove...
  id2remove_23 <- c(ymd_keepers_21$database_id, dmy_keepers_22$database_id)
  
  # Extract the matching strings to three columns
  mdy_unambiguous <- noDATEa %>%
    # First, remove the strings matched prior
    dplyr::filter(!database_id %in% id2remove_23) %>%
    dplyr::mutate(
      mdy_vEV = stringr::str_extract(verbatimEventDate,
                                     pattern = paste(mdy_strings, 
                                                     collapse = "|", sep = "")),
      mdy_locality = stringr::str_extract(locality,
                                          pattern = paste(mdy_strings, 
                                                          collapse = "|", sep = "")),
      mdy_fieldNotes = stringr::str_extract(fieldNotes,
                                            pattern = paste(mdy_strings, 
                                                            collapse = "|", sep = "")),
      mdy_locationRemarks = stringr::str_extract(locationRemarks,
                                                 pattern = paste(mdy_strings, 
                                                                 collapse = "|", sep = ""))
    ) %>% # END mutate
    # select a subset of columns
    dplyr::select(database_id, mdy_vEV, mdy_locality,
                  mdy_fieldNotes, mdy_locationRemarks, verbatimEventDate)  
  
  # FORMAT the mdy_vEV column
  mdy_unambiguous$mdy_vEV <- mdy_unambiguous$mdy_vEV %>%
    # Convert roman numerals to readable by lubridate
    mgsub::mgsub(
      pattern = paste("^",romanNumerals, "( |\\.|-)", sep = ""),
      replacement = numeralConversion) %>%
    lubridate::mdy(truncated = 2, quiet = TRUE) 
  #
  # FORMAT the mdy_locality column
  mdy_unambiguous$mdy_locality <- mdy_unambiguous$mdy_locality %>%
    # Convert roman numerals to readable by lubridate
    mgsub::mgsub(
      pattern = paste("^",romanNumerals, "( |\\.|-)", sep = ""),
      replacement = numeralConversion) %>%
    stringr::str_replace( pattern = "^The ", replacement = "") %>%
    lubridate::mdy(truncated = 2, quiet = TRUE) 
  #
  # FORMAT the mdy_fieldNotes column
  mdy_unambiguous$mdy_fieldNotes <- mdy_unambiguous$mdy_fieldNotes %>%
    # Convert roman numerals to readable by lubridate
    mgsub::mgsub(
      pattern = paste("^",romanNumerals, "( |\\.|-)", sep = ""),
      replacement = numeralConversion) %>%
    lubridate::mdy(truncated = 2, quiet = TRUE) 
  #
  # FORMAT the mdy_locationRemarks column
  mdy_unambiguous$mdy_locationRemarks <- mdy_unambiguous$mdy_locationRemarks %>%
    # Convert roman numerals to readable by lubridate
    mgsub::mgsub(
      pattern = paste("^",romanNumerals, "( |\\.|-)", sep = ""),
      replacement = numeralConversion) %>%
    lubridate::mdy(truncated = 2, quiet = TRUE) 
  #
  # Combine the columns
  mdy_keepers_23 <- mdy_unambiguous %>%
    dplyr::filter( complete.cases(mdy_vEV) |
                     complete.cases(mdy_locality) | 
                     complete.cases(mdy_locationRemarks) | 
                     complete.cases(mdy_fieldNotes)) %>%
    tidyr::unite(col = date, 
                 mdy_vEV, mdy_locality, mdy_locationRemarks, mdy_fieldNotes, 
                 na.rm = TRUE, sep = "")
  # KEEP mdy_keepers_23 at the end
  
  
  # CHANGE: Being a lot more conservative here
  # CHANGE: General changes to have the regex is written.
  ##### 2.4 my ####
  my_strings <- c(
    # MM/YYYY format (No day after or before)
    "(?<=\\s)(0[1-9]|1[0-2])/[0-9]{4}(?=\\s)",
    # Text month and 4-digit year, no numbers before
    paste0("(?<!\\d\\s?)\\b(?:", monthStrings, ")([-/\\.\\s])(?:[1-9][0-9]{3})\\b(?!\\s?\\d)"),
    # Roman numeral month and 4-digit year
    paste0("(?<!\\d\\s?)\\b(?:", romanNumerals, ")([-/\\.\\s])(?:[1-9][0-9]{3})\\b(?!\\s?\\d)")
  )
  
  # Get the IDs to remove...
  id2remove_24 <- c(mdy_keepers_23$database_id, id2remove_23)
  
  # Extract the matching strings to three columns
  my_unambiguous <- noDATEa %>%
    # First, remove the strings matched prior
    dplyr::filter(!database_id %in% id2remove_24) %>%
    dplyr::mutate(
      my_vEV = stringr::str_extract(verbatimEventDate,
                                    pattern = paste(my_strings, 
                                                    collapse = "|", sep = "")),
      my_locality = stringr::str_extract(locality,
                                         pattern = paste(my_strings, 
                                                         collapse = "|", sep = "")),
      my_fieldNotes = stringr::str_extract(fieldNotes,
                                           pattern = paste(my_strings, 
                                                           collapse = "|", sep = "")),
      my_locationRemarks = stringr::str_extract(locationRemarks,
                                                pattern = paste(my_strings, 
                                                                collapse = "|", sep = ""))
    ) %>% # END mutate
    # select a subset of columns
    dplyr::select(database_id, my_vEV, my_locality,
                  my_fieldNotes, my_locationRemarks, verbatimEventDate) 
  
  # FORMAT the my_vEV column
  my_unambiguous$my_vEV <- my_unambiguous$my_vEV %>%
    # Convert roman numerals to readable by lubridate
    mgsub::mgsub(
      pattern = paste("^",romanNumerals, sep = ""),
      replacement = numeralConversion) %>%
    # format
    lubridate::my( quiet = TRUE) 
  #
  # FORMAT the my_locality column
  my_unambiguous$my_locality <- my_unambiguous$my_locality %>%
    # Convert roman numerals to readable by lubridate
    mgsub::mgsub(
      pattern = paste("^",romanNumerals, sep = ""),
      replacement = numeralConversion) %>%
    # format
    lubridate::my(quiet = TRUE) 
  #
  # FORMAT the my_fieldNotes column
  my_unambiguous$my_fieldNotes <- my_unambiguous$my_fieldNotes %>%
    # Convert roman numerals to readable by lubridate
    mgsub::mgsub(
      pattern = paste("^",romanNumerals, sep = ""),
      replacement = numeralConversion) %>%
    # format
    lubridate::my(quiet = TRUE) 
  #
  # FORMAT the my_locationRemarks column
  my_unambiguous$my_locationRemarks <- my_unambiguous$my_locationRemarks %>%
    # Convert roman numerals to readable by lubridate
    mgsub::mgsub(
      pattern = paste("^",romanNumerals, sep = ""),
      replacement = numeralConversion) %>%
    # format
    lubridate::my(quiet = TRUE) 
  #
  
  # Combine the columns
  my_keepers_24 <- my_unambiguous %>%
    dplyr::filter(complete.cases(my_vEV) |
                    complete.cases(my_locality) | 
                    complete.cases(my_locationRemarks) | 
                    complete.cases(my_fieldNotes)) %>%
    tidyr::unite(col = date, 
                 my_vEV, my_locality, my_locationRemarks, my_fieldNotes, 
                 na.rm = TRUE, sep = "-") 
  # Remove double-ups
  my_keepers_24$date <- stringr::str_replace(my_keepers_24$date,
                                             pattern = "-[0-9]+-[0-9]+-[0-9]+", 
                                             replacement = "")
  

  # CHANGE: This assumes ambiguous dates are dmy? That feels like it undoes all
  # of the previous thought put into this function
  
 #  #### 3.0 Amb. str. dates ####
 #  writeLines(paste(
 #    " - Extracting year from fieldNotes, locationRemarks, and verbatimEventDate ",
 #    "columns in ambiguous formats...", sep = ""))
 #  ambiguousDateStrings <- c(
 #    # dmy or mdy; 10 02 1946
 #    "[0-9]{1,2}[\\s-/ ]+[0-9]{1,2}[\\s-/ ]+[0-9]{4}",
 #    "[0-9]{2}[\\s-/ ]+[0-9]{2}[\\s-/ ]+[0-9]{4}",
 #    "[0-9]{1,2}[\\s-/ ]+[0-9]{1,2}[\\s-/ ]+[0-9]{2}",
 #    "[0-9]{2}[\\s-/ ]+[0-9]{2}[\\s-/ ]+[0-9]{2}"
 #  )
 #  
 #  # Get the IDs to remove...
 #  id2remove_30 <- c(my_keepers_24$database_id, id2remove_24)
 #  
 #  # Extract the matching strings to three columns
 #  ambiguousNames <- noDATEa %>%
 #    # First, remove the strings matched prior
 #    dplyr::filter(!database_id %in% id2remove_30) %>%
 #    dplyr::mutate(
 #      amb_vEV = stringr::str_extract(verbatimEventDate,
 #                                     pattern = paste(ambiguousDateStrings, 
 #                                                     collapse = "|", sep = "")),
 #      amb_locality = stringr::str_extract(locality,
 #                                          pattern = paste(ambiguousDateStrings, 
 #                                                          collapse = "|", sep = "")),
 #      amb_fieldNotes = stringr::str_extract(fieldNotes,
 #                                            pattern = paste(ambiguousDateStrings, 
 #                                                            collapse = "|", sep = "")),
 #      amb_locationRemarks = stringr::str_extract(locationRemarks,
 #                                                 pattern = paste(ambiguousDateStrings, 
 #                                                                 collapse = "|", sep = ""))
 #    ) %>% # END mutate
 #    # select a subset of columns
 #    dplyr::select(database_id, amb_vEV, amb_locality,
 #                  amb_fieldNotes, amb_locationRemarks) 
 #  
 #  # FORMAT the amb_vEV column
 #  ambiguousNames$amb_vEV <- ambiguousNames$amb_vEV %>%
 #    # Remove 00 values to truncate the lubridate 
 #    stringr::str_replace(pattern = "00[-/\\. ]00[-/\\. ]", replacement = "01-01-") %>%
 #    stringr::str_replace(pattern = "00[-/\\. ]", replacement = "01-") %>%
 #    stringr::str_replace(pattern = "00-00-|[-/\\. ]00$|[-/\\. ]00[-/\\. ]00$|^00|^00[-/\\. ]00[-/\\. ]",
 #                         replacement = "") %>%
 #    lubridate::dmy(truncated = 2, quiet = TRUE) 
 #  # FORMAT the amb_locality column
 #  #
 #  ambiguousNames$amb_locality <- ambiguousNames$amb_locality %>%
 #    # Remove 00 values to truncate the lubridate 
 #    stringr::str_replace(pattern = "00[-/\\. ]00[-/\\. ]", replacement = "01-01-") %>%
 #    stringr::str_replace(pattern = "00[-/\\. ]", replacement = "01-") %>%
 #    stringr::str_replace(pattern = "00-00-|[-/\\. ]00$|[-/\\. ]00[-/\\. ]00$|^00|^00[-/\\. ]00[-/\\. ]",
 #                         replacement = "") %>%
 #    lubridate::dmy(truncated = 2, quiet = TRUE) 
 #  # FORMAT the amb_fieldNotes column
 #  ambiguousNames$amb_fieldNotes <- ambiguousNames$amb_fieldNotes %>%
 #    # Remove 00 values to truncate the lubridate 
 #    stringr::str_replace(pattern = "00[-/\\. ]00[-/\\. ]", replacement = "01-01-") %>%
 #    stringr::str_replace(pattern = "00[-/\\. ]", replacement = "01-") %>%
 #    stringr::str_replace(pattern = "00-00-|[-/\\. ]00$|[-/\\. ]00[-/\\. ]00$|^00|^00[-/\\. ]00[-/\\. ]",
 #                         replacement = "") %>%
 #    lubridate::dmy(truncated = 2, quiet = TRUE) 
 #  #
 #  # FORMAT the amb_locationRemarks column
 #  ambiguousNames$amb_locationRemarks <- ambiguousNames$amb_locationRemarks %>%
 #    # Remove 00 values to truncate the lubridate 
 #    stringr::str_replace(pattern = "00[-/\\. ]00[-/\\. ]", replacement = "01-01-") %>%
 #    stringr::str_replace(pattern = "00[-/\\. ]", replacement = "01-") %>%
 #    stringr::str_replace(pattern = "00-00-|[-/\\. ]00$|[-/\\. ]00[-/\\. ]00$|^00|^00[-/\\. ]00[-/\\. ]",
 #                         replacement = "") %>%
 #    lubridate::dmy(truncated = 2, quiet = TRUE) 
 #  
 #  
 #  
 #  # Combine the columns
 #  amb_keepers_30 <- ambiguousNames %>%
 #    dplyr::filter(complete.cases(amb_vEV)|
 #                    complete.cases(amb_locality) | 
 #                    complete.cases(amb_locationRemarks) | 
 #                    complete.cases(amb_fieldNotes)) %>%
 #    tidyr::unite(col = date, amb_vEV,  
 #                 amb_locality, amb_locationRemarks, amb_fieldNotes, 
 #                 na.rm = TRUE)
 #  # KEEP amb_keepers_30 at the end
  
  #### 4.0 Format+combine ####
  writeLines(paste(
    " - Formating and combining the new data..", sep = ""))
  ##### 4.1 formatting... ####
  # Extract only the date from occYr_2
  occYr_2$date <- as.character(occYr_2$date) %>% lubridate::ymd_hms() %>% lubridate::date()
  # Set as date format...
  ymd_keepers_21$date <- lubridate::ymd(ymd_keepers_21$date)
  dmy_keepers_22$date <- lubridate::ymd(dmy_keepers_22$date)
  mdy_keepers_23$date <- lubridate::ymd(mdy_keepers_23$date)
  my_keepers_24$date <- lubridate::ymd(my_keepers_24$date)
  # merge these data...
  
  saveTheDates <- dplyr::bind_rows(ymd_hms_0, dmy_1, occYr_2, 
                                   ymd_keepers_21, dmy_keepers_22, mdy_keepers_23) %>%
    dplyr::select(database_id, date) 
  
  
  ##### 4.2 Full dates ####
  # Join these dates to the original rows...
  datesOut_full <- data %>% 
    dplyr::right_join(saveTheDates, 
                      by = "database_id")
  # Fill the eventDate column
  datesOut_full$eventDate <- lubridate::ymd(datesOut_full$date)
  # Fill the year, month, and day columns 
  datesOut_full$year <- lubridate::year(datesOut_full$date)
  datesOut_full$month <- lubridate::month(datesOut_full$date)
  datesOut_full$day <- lubridate::day(datesOut_full$date)
  # Remove records with non-sensical years
  datesOut_full <- datesOut_full %>%
    # remove FUTURE dates
    dplyr::filter(!year > maxYear) %>%
    # Remove PAST dates 
    dplyr::filter(!year < minYear) %>%
    # remove the date column
    dplyr::select(!date)
  
  
  ##### 4.3 No day ####
  # Join these dates to the original rows...
  datesOut_noDay <- data %>% 
    dplyr::right_join(my_keepers_24, 
                      by = "database_id")
  # Fill the eventDate column
  datesOut_noDay$eventDate <- lubridate::ymd(datesOut_noDay$date, quiet = TRUE)
  # Fill the year, month, and day columns 
  datesOut_noDay$year <- lubridate::year(datesOut_noDay$date)
  datesOut_noDay$month <- lubridate::month(datesOut_noDay$date)
  # Remove records with non-sensical years
  datesOut_noDay <- datesOut_noDay %>%
    # remove FUTURE dates
    dplyr::filter(!year > maxYear) %>%
    # Remove PAST dates 
    dplyr::filter(!year < minYear) %>%
    # remove the date column
    dplyr::select(!date)
  
  
  # amb_keepers_30 does not represent dates with no month...
  
  ##### 4.4 No month ####
  # Join these dates to the original rows...
  # datesOut_noMonth <- data %>% 
  #   dplyr::right_join(amb_keepers_30, 
  #                     by = "database_id")
  # # Fill the eventDate column
  # datesOut_noMonth$eventDate <- lubridate::ymd(datesOut_noMonth$date, quiet = TRUE)
  # # Fill the year, month, and day columns 
  # datesOut_noMonth$year <- lubridate::year(datesOut_noMonth$date)
  # # Remove records with non-sensical years
  # datesOut_noMonth <- datesOut_noMonth %>%
  #   # remove FUTURE dates
  #   dplyr::filter(!year > maxYear) %>%
  #   # Remove PAST dates 
  #   dplyr::filter(!year < minYear) %>%
  #   # remove the date column
  #   dplyr::select(!date)

  
  #### 5.0 Merge ####
  writeLines(paste(
    " - Merging all data, nearly there...", sep = ""))
  # Get all of the changed rows together
  datesMerged <- dplyr::bind_rows(
    datesOut_full, datesOut_noDay)
  
  # Format the original eventDate column into a new sheet - datesOut
  datesOut <- data
  datesOut$eventDate <- lubridate::ymd_hms(datesOut$eventDate,
                                           truncated = 5, quiet = TRUE)
  # Replace these in the original dataset
  datesOut <- datesOut %>% 
    # Remove the dates that are to be replaced
    dplyr::filter(!database_id %in% datesMerged$database_id) 
  
  # Extract year, month, and day where possible
  # year
  datesOut$year <- ifelse(is.na(datesOut$year),
                          lubridate::year(datesOut$eventDate),
                          datesOut$year)
  # month
  datesOut$month <- ifelse(is.na(datesOut$month),
                           lubridate::month(datesOut$eventDate),
                           datesOut$month)
  # day
  datesOut$day <- ifelse(is.na(datesOut$day),
                         lubridate::day(datesOut$eventDate),
                         datesOut$day)
  
  # Remove the months and days where the year is incorrect.
  datesOut$month <- ifelse(datesOut$year > maxYear | datesOut$year < minYear,
                           NA,
                           datesOut$month)
  datesOut$day <- ifelse(datesOut$year > maxYear | datesOut$year < minYear,
                         NA,
                         datesOut$day)
  # Remove non-sensical years now
  datesOut$year <- ifelse(datesOut$year > maxYear | datesOut$year < minYear,
                          NA,
                          datesOut$year)
  # Now check and replace the eventDate column if it's out of range
  datesOut$eventDate <- ifelse(lubridate::year(datesOut$eventDate) > maxYear | 
                                 lubridate::year(datesOut$eventDate) < minYear,
                               NA,
                               as.character(datesOut$eventDate))
  
  
  # For simplicity's sake, return the date columns as character...
  datesOut$eventDate  <- as.character(datesOut$eventDate)
  datesMerged$eventDate  <- as.character(datesMerged$eventDate)
  
  # MERGE all datsets
  dates_complete <- data %>%
    dplyr::mutate(eventDate = as.character(eventDate)) %>%
    # REMOVE the meddled-with rows
    dplyr::filter(!database_id %in% c( datesMerged$database_id, datesOut$database_id)) %>%
    # Merge the new rows back in
    dplyr::bind_rows(datesMerged, datesOut)
  
  # Return to date format <3 
  dates_complete$eventDate <- lubridate::ymd_hms(dates_complete$eventDate,
                                                 truncated = 5, quiet = TRUE)
  
  # Plot the dates
  # graphics::hist(dates_complete$eventDate, breaks = 100,
  #      main = "Histogram of eventDate output")
  
  timeEnd <- Sys.time()
  
  # Return user output
  writeLines(
    paste(
      " - Finished. \n",
      "We now have ",
      format((sum(complete.cases(dates_complete$eventDate)) - originalDateCount),
             big.mark = ","), 
      " more full eventDate cells than in the input data.\n",
      "We modified dates in \n",
      format(nrow(datesMerged), big.mark = ","), " occurrences.\n",
      " - As it stands, there are ", 
      format( sum(complete.cases(dates_complete$eventDate)), big.mark = ","),
      " complete eventDates and ", 
      format( sum(is.na(dates_complete$eventDate)), big.mark = ","), 
      " missing dates.\n", 
      " - There are also ", 
      format( sum(complete.cases(dates_complete$year)), big.mark = ","), 
      " complete year occurrences to filter from. This is up from an initial count of ",
      format( sum(complete.cases(data$year)), big.mark = ","),
      " At this rate, you will stand to lose ",
      format( sum(is.na(dates_complete$year)), big.mark = ","), 
      " occurrences on the basis of missing",
      " year",
      " - Operation time: ", (timeEnd - timeStart)," ",
      units(round(timeEnd - timeStart, digits = 2)),
      sep = "")
  )
  return(dates_complete)
}