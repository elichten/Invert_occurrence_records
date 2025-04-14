# TODO: This SEVERELY needs some commenting
dateFindR_fixed <- function (data = NULL, maxYear = lubridate::year(Sys.Date()), 
  minYear = 1700) 
{
  eventDate <- database_id <- . <- verbatimEventDate <- fieldNotes <- locationRemarks <- ymd_vEV <- ymd_fieldNotes <- ymd_locationRemarks <- locality <- dmy_vEV <- dmy_locality <- dmy_fieldNotes <- dmy_locationRemarks <- mdy_vEV <- mdy_locality <- mdy_fieldNotes <- mdy_locationRemarks <- my_vEV <- my_locality <- my_fieldNotes <- my_locationRemarks <- amb_vEV <- amb_locality <- amb_fieldNotes <- amb_locationRemarks <- year <- endTime <- startTime <- originalDateCount <- eventDate_in <- day <- NULL
  requireNamespace("dplyr")
  requireNamespace("lubridate")
  requireNamespace("mgsub")
  timeStart <- Sys.time()
  writeLines(" - Preparing data...")
  originalDateCount <- sum(complete.cases(data$eventDate))
  noDATEa <- data %>% 
    dplyr::mutate(eventDate_in = eventDate, .before = eventDate) %>% 
    dplyr::mutate(
      eventDate = lubridate::parse_date_time(eventDate_in, 
                                             orders = c("ymd","ymdHMS","dmy","mdy"), 
                                             truncated = 5, 
                                             quiet = TRUE, 
                                             tz = "UTC", 
                                             locale = Sys.getlocale("LC_TIME"))) %>% 
    dplyr::mutate(eventDate = dplyr::if_else(is.na(eventDate), 
                                             lubridate::ymd_hms(eventDate_in, 
                                                                quiet = TRUE, 
                                                                truncated = 5), 
                                             eventDate)) %>% 
    dplyr::mutate(dateSuccess = dplyr::if_else(is.na(eventDate), 
                                               FALSE, 
                                               TRUE))
  ymd_hms_0 <- noDATEa %>% dplyr::filter(complete.cases(eventDate)) %>% 
    dplyr::select(database_id, eventDate) %>% 
      setNames(object = ., c("database_id", "date"))
  writeLines(" - Extracting dates from year, month, day columns...")
  noDATEa <- noDATEa %>% dplyr::filter(is.na(eventDate))
  noDATEa <- noDATEa %>% dplyr::mutate(eventDate = dplyr::if_else(is.na(as.character(eventDate)), 
    lubridate::ymd(stringr::str_c(year, month, day, sep = "-"), 
      quiet = TRUE, truncated = 2), eventDate))
  dmy_1 <- noDATEa %>% dplyr::filter(complete.cases(eventDate)) %>% 
    dplyr::select(database_id, eventDate) %>% setNames(object = ., 
    c("database_id", "date"))
  noDATEa <- noDATEa %>% dplyr::filter(is.na(eventDate))
  if ("occurrenceYear" %in% colnames(noDATEa)) {
    noDATEa$eventDate <- noDATEa$occurrenceYear
  }
  occYr_2 <- noDATEa %>% dplyr::filter(complete.cases(eventDate)) %>% 
    dplyr::select(database_id, eventDate) %>% setNames(object = ., 
    c("database_id", "date"))
  noDATEa$locality <- noDATEa$locality %>% stringr::str_replace(pattern = "[Ss]ept[\\s-/]", 
    replacement = "Sep ")
  noDATEa$fieldNotes <- noDATEa$fieldNotes %>% stringr::str_replace(pattern = "[Ss]ept[\\s-/]", 
    replacement = "Sep ")
  noDATEa$locationRemarks <- noDATEa$locationRemarks %>% stringr::str_replace(pattern = "[Ss]ept[\\s-/]", 
    replacement = "Sep ")
  noDATEa$verbatimEventDate <- noDATEa$verbatimEventDate %>% 
    stringr::str_replace(pattern = "[Ss]ept[\\s-/]", replacement = "Sep ")
  writeLines(paste(" - Extracting dates from fieldNotes, locationRemarks, and verbatimEventDate ", 
    "columns in unambiguous ymd, dmy, mdy, and my formats...", 
    sep = ""))
  noDATEa <- noDATEa %>% dplyr::filter(is.na(eventDate))
  monthStrings <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "jan", "feb", 
    "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", 
    "nov", "dec", "January", "February", "March", "April", 
    "May", "June", "July", "August", "September", "October", 
    "November", "December", "january", "february", "march", 
    "april", "may", "june", "july", "august", "september", 
    "october", "november", "december", "JAN", "FEB", "MAR", 
    "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", 
    "DEC", "JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", 
    "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", 
    "DECEMBER", "sept")
  romanNumerals <- c("i", "ii", "iii", "iv", "v", "vi", "vii", 
    "viii", "ix", "x", "xi", "xii", "I", "II", "III", "IV", 
    "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII")
  numeralConversion <- c("Jan", "Feb", "Mar", "Apr", "May", 
    "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", 
    "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", 
    "Oct", "Nov", "Dec")
  ymd_strings <- c(
    "[0-9]{4}[\\s-/\\. ]+[0-9]{2}[\\s-/\\. ]+[0-9]{2}", 
    paste("[0-9]{4}", monthStrings, "[0-9]{2}", collapse = "|", sep = "[\\s-/\\. ]+"), 
    paste("[0-9]{4}", romanNumerals, "[0-9]{2}", collapse = "|", sep = "[\\s-/\\. ]+"))
  ymd_unambiguous <- noDATEa %>% 
    dplyr::mutate(
      ymd_vEV = stringr::str_extract(verbatimEventDate, 
                                     pattern = paste(ymd_strings, 
                                                     collapse = "|", 
                                                     sep = "")), 
      ymd_fieldNotes = stringr::str_extract(fieldNotes, 
                                            pattern = paste(ymd_strings, 
                                                            collapse = "|", 
                                                            sep = "")), 
      ymd_locationRemarks = stringr::str_extract(locationRemarks, 
                                                 pattern = paste(ymd_strings, 
                                                                 collapse = "|", 
                                                                 sep = ""))) %>% 
    dplyr::select(database_id, ymd_vEV, ymd_fieldNotes, ymd_locationRemarks)
  ymd_unambiguous$ymd_vEV <- ymd_unambiguous$ymd_vEV %>% 
    stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>% 
    lubridate::ymd(truncated = 2, quiet = TRUE)
  ymd_unambiguous$ymd_fieldNotes <- ymd_unambiguous$ymd_fieldNotes %>% 
    stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>% 
    lubridate::ymd(truncated = 2, quiet = TRUE)
  ymd_unambiguous$ymd_locationRemarks <- ymd_unambiguous$ymd_locationRemarks %>% 
    stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>% 
    lubridate::ymd(truncated = 2, quiet = TRUE)
  ymd_keepers_21 <- ymd_unambiguous %>% dplyr::filter(complete.cases(ymd_vEV) | 
    complete.cases(ymd_locationRemarks) | complete.cases(ymd_fieldNotes)) %>% 
    tidyr::unite(col = date, ymd_vEV, ymd_locationRemarks, 
      ymd_fieldNotes, na.rm = TRUE)
  # Note: When patterns contain an internal pipe ("|"), they'll need to be encapsulated in parentheses
  dmy_strings <- c(
    paste("[0-9]{1,2}[\\s-/\\. ]+", monthStrings, "[\\s-/\\. ]+[0-9]{4}", collapse = "|", sep = ""), 
    paste("[0-9]{1,2}[\\s-/\\. ]+", monthStrings, "[\\s-/\\. ]+[0-9]{2}", collapse = "|", sep = ""), 
    paste("[0-9]{1,2}[\\s-/\\. ]+", romanNumerals, "[\\s-/\\. ]+[0-9]{4}", collapse = "|", sep = ""), 
    paste("[0-9]{1,2}[\\s-/\\. ]+", romanNumerals, "[\\s-/\\. ]+[0-9]{2}", collapse = "|", sep = ""), 
    "(([1][3-9]|[2-3][0-9])[\\s-/\\. ]+([1-9]|1[0-2])[\\s-/\\. ]+[0-9]{4})", 
    "(([1][3-9]|[2-3][0-9])[\\s-/\\. ]+([1-9]|1[0-2])[\\s-/\\. ]+[0-9]{2})")
  dmy_unambiguous <- noDATEa %>% dplyr::filter(!database_id %in% 
    ymd_keepers_21$database_id) %>% dplyr::mutate(dmy_vEV = stringr::str_extract(verbatimEventDate, 
    pattern = paste(dmy_strings, collapse = "|", sep = "")), 
    dmy_locality = stringr::str_extract(locality, pattern = paste(dmy_strings, 
      collapse = "|", sep = "")), dmy_fieldNotes = stringr::str_extract(fieldNotes, 
      pattern = paste(dmy_strings, collapse = "|", sep = "")), 
    dmy_locationRemarks = stringr::str_extract(locationRemarks, 
      pattern = paste(dmy_strings, collapse = "|", sep = ""))) %>% 
    dplyr::select(database_id, dmy_vEV, dmy_locality, dmy_fieldNotes, 
      dmy_locationRemarks)
  dmy_unambiguous$dmy_vEV <- dmy_unambiguous$dmy_vEV %>% mgsub::mgsub(pattern = paste("[-/ ]", 
    romanNumerals, "[ -/]", sep = ""), replacement = numeralConversion) %>% 
    stringr::str_replace(pattern = "/00$|/00/00$|^00", replacement = "") %>% 
    lubridate::dmy(truncated = 2, quiet = TRUE)
  dmy_unambiguous$dmy_locality <- dmy_unambiguous$dmy_locality %>% 
    mgsub::mgsub(pattern = paste("[-/ ]", romanNumerals, 
      "[ -/]", sep = ""), replacement = numeralConversion) %>% 
    stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>% 
    lubridate::dmy(truncated = 2, quiet = TRUE)
  dmy_unambiguous$dmy_fieldNotes <- dmy_unambiguous$dmy_fieldNotes %>% 
    mgsub::mgsub(pattern = paste("[-/ ]", romanNumerals, 
      "[ -/]", sep = ""), replacement = numeralConversion) %>% 
    stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>% 
    lubridate::dmy(truncated = 2, quiet = TRUE)
  dmy_unambiguous$dmy_locationRemarks <- dmy_unambiguous$dmy_locationRemarks %>% 
    mgsub::mgsub(pattern = paste("[-/ ]", romanNumerals, 
      "[ -/]", sep = ""), replacement = numeralConversion) %>% 
    stringr::str_replace(pattern = "/00$|/00/00$", replacement = "") %>% 
    lubridate::dmy(truncated = 2, quiet = TRUE)
  dmy_keepers_22 <- dmy_unambiguous %>% 
    dplyr::filter(complete.cases(dmy_vEV) | 
                  complete.cases(dmy_locality) | 
                  complete.cases(dmy_locationRemarks) | 
                  complete.cases(dmy_fieldNotes)) %>% 
    tidyr::unite(col = date, dmy_vEV, dmy_locality, dmy_locationRemarks, dmy_fieldNotes, na.rm = TRUE)
  # Note: When patterns contain an internal pipe ("|"), they'll need to be encapsulated in parentheses
  mdy_strings <- c(
    paste(monthStrings, "[\\s-/ ]+[0-9]{1,2}[\\s-/, ]+[0-9]{4}", collapse = "|", sep = ""), 
    paste(monthStrings, "[\\s-/ ]+[0-9]{1,2}[\\s-/, ]+[0-9]{2}", collapse = "|", sep = ""), 
    paste(monthStrings, "[0-9]+[-\\u2013][0-9]+[\\s-/ ]+[0-9]{4}", collapse = "|", sep = ""), 
    paste(monthStrings, "[0-9]+[-\\u2013][0-9]+[\\s-/ ]+[0-9]{2}", collapse = "|", sep = ""), 
    paste(romanNumerals, "[\\s-/\\. ]+[0-9]{1,2}[\\s-/ ]+[0-9]{4}", collapse = "|", sep = ""), 
    paste(romanNumerals, "[\\s-/\\. ]+[0-9]{1,2}[\\s-/ ]+[0-9]{2}", collapse = "|", sep = ""), 
    "(1[0-2])[\\s- /]+([2-3][0-9])[\\s- /]+[0-9]{4}", 
    "(((1[0-2])|[1-9])[\\s-/\\. ]+([1][3-9])[\\s-/\\. ]+[0-9]{4})", 
    "(((1[0-2])|[1-9])[\\s-/\\. ]+([2-3][0-9])[\\s-/\\. ]+[0-9]{4})", 
    "(((1[0-2])|^[1-9])[\\s-/\\. ]+([1][3-9])[\\s-/\\. ]+[0-9]{4})", 
    "(((1[0-2])|^[1-9])[\\s-/\\. ]+([2-3][0-9])[\\s-/\\. ]+[0-9]{4})", 
    "(((1[0-2])|[1-9])[\\s-/\\. ]+([1][3-9])[\\s-/\\. ]+[0-9]{2})", 
    "(((1[0-2])|[1-9])[\\s-/\\. ]+([2-3][0-9])[\\s-/\\. ]+[0-9]{2})", 
    "(((1[0-2])|^[1-9])[\\s-/\\. ]+([1][3-9])[\\s-/\\. ]+[0-9]{2})", 
    "(((1[0-2])|^[1-9])[\\s-/\\. ]+([2-3][0-9])[\\s-/\\. ]+[0-9]{2})", 
    "((1[0-2])[\\s-/\\. ]+([2-3][0-9])[\\s-/\\. ]+[0-9]{2})")
  id2remove_23 <- c(ymd_keepers_21$database_id, dmy_keepers_22$database_id)
  mdy_unambiguous <- noDATEa %>% dplyr::filter(!database_id %in% 
    id2remove_23) %>% dplyr::mutate(mdy_vEV = stringr::str_extract(verbatimEventDate, 
    pattern = paste(mdy_strings, collapse = "|", sep = "")), 
    mdy_locality = stringr::str_extract(locality, pattern = paste(mdy_strings, 
      collapse = "|", sep = "")), mdy_fieldNotes = stringr::str_extract(fieldNotes, 
      pattern = paste(mdy_strings, collapse = "|", sep = "")), 
    mdy_locationRemarks = stringr::str_extract(locationRemarks, 
      pattern = paste(mdy_strings, collapse = "|", sep = ""))) %>% 
    dplyr::select(database_id, mdy_vEV, mdy_locality, mdy_fieldNotes, 
      mdy_locationRemarks)
  mdy_unambiguous$mdy_vEV <- mdy_unambiguous$mdy_vEV %>% mgsub::mgsub(pattern = paste("^", 
    romanNumerals, "( |\\.|-)", sep = ""), replacement = numeralConversion) %>% 
    lubridate::mdy(truncated = 2, quiet = TRUE)
  mdy_unambiguous$mdy_locality <- mdy_unambiguous$mdy_locality %>% 
    mgsub::mgsub(pattern = paste("^", romanNumerals, "( |\\.|-)", 
      sep = ""), replacement = numeralConversion) %>% 
    stringr::str_replace(pattern = "^The ", replacement = "") %>% 
    lubridate::mdy(truncated = 2, quiet = TRUE)
  mdy_unambiguous$mdy_fieldNotes <- mdy_unambiguous$mdy_fieldNotes %>% 
    mgsub::mgsub(pattern = paste("^", romanNumerals, "( |\\.|-)", 
      sep = ""), replacement = numeralConversion) %>% 
    lubridate::mdy(truncated = 2, quiet = TRUE)
  mdy_unambiguous$mdy_locationRemarks <- mdy_unambiguous$mdy_locationRemarks %>% 
    mgsub::mgsub(pattern = paste("^", romanNumerals, "( |\\.|-)", 
      sep = ""), replacement = numeralConversion) %>% 
    lubridate::mdy(truncated = 2, quiet = TRUE)
  mdy_keepers_23 <- mdy_unambiguous %>% 
    dplyr::filter(complete.cases(mdy_vEV) | 
      complete.cases(mdy_locality) | 
      complete.cases(mdy_locationRemarks) | 
      complete.cases(mdy_fieldNotes)) %>% 
    tidyr::unite(col = date, 
                 mdy_vEV, 
                 mdy_locality, 
                 mdy_locationRemarks, 
                 mdy_fieldNotes, 
                 na.rm = TRUE, 
                 sep = "")
  # Note: When patterns contain an internal pipe ("|"), they'll need to be encapsulated in parentheses
  my_strings <- c(
    paste(romanNumerals, "[\\s-/ \\.]+[0-9]{4}", collapse = "|", sep = ""), 
    paste(monthStrings, "[\\s-/ \\.]+[0-9]{4}", collapse = "|", sep = ""), 
    paste(monthStrings, "[\\s-/ \\.]+[0-9]{2}", collapse = "|", sep = ""), 
    paste(monthStrings, "[\\s-/ ]+[0-9]{4}", collapse = "|", sep = ""), 
    paste(monthStrings, "[\\s-/ ]+[0-9]{2}", collapse = "|", sep = ""), 
    paste("[\\s- /]+[0-9]{4}", monthStrings, collapse = "|", sep = ""), 
    paste("[\\s- /]+[0-9]{2}", monthStrings, collapse = "|", sep = ""), 
    "(([1-9]|1[0-2])[\\s- /]+[0-9]{4})")
  id2remove_24 <- c(mdy_keepers_23$database_id, id2remove_23)
  my_unambiguous <- noDATEa %>% dplyr::filter(!database_id %in% 
    id2remove_24) %>% dplyr::mutate(my_vEV = stringr::str_extract(verbatimEventDate, 
    pattern = paste(my_strings, collapse = "|", sep = "")), 
    my_locality = stringr::str_extract(locality, pattern = paste(my_strings, 
      collapse = "|", sep = "")), my_fieldNotes = stringr::str_extract(fieldNotes, 
      pattern = paste(my_strings, collapse = "|", sep = "")), 
    my_locationRemarks = stringr::str_extract(locationRemarks, 
      pattern = paste(my_strings, collapse = "|", sep = ""))) %>% 
    dplyr::select(database_id, my_vEV, my_locality, my_fieldNotes, 
      my_locationRemarks)
  my_unambiguous$my_vEV <- my_unambiguous$my_vEV %>% mgsub::mgsub(pattern = paste("^", 
    romanNumerals, sep = ""), replacement = numeralConversion) %>% 
    lubridate::my(quiet = TRUE)
  my_unambiguous$my_locality <- my_unambiguous$my_locality %>% 
    mgsub::mgsub(pattern = paste("^", romanNumerals, sep = ""), 
      replacement = numeralConversion) %>% lubridate::my(quiet = TRUE)
  my_unambiguous$my_fieldNotes <- my_unambiguous$my_fieldNotes %>% 
    mgsub::mgsub(pattern = paste("^", romanNumerals, sep = ""), 
      replacement = numeralConversion) %>% lubridate::my(quiet = TRUE)
  my_unambiguous$my_locationRemarks <- my_unambiguous$my_locationRemarks %>% 
    mgsub::mgsub(pattern = paste("^", romanNumerals, sep = ""), 
      replacement = numeralConversion) %>% lubridate::my(quiet = TRUE)
  my_keepers_24 <- my_unambiguous %>% dplyr::filter(complete.cases(my_vEV) | 
    complete.cases(my_locality) | complete.cases(my_locationRemarks) | 
    complete.cases(my_fieldNotes)) %>% tidyr::unite(col = date, 
    my_vEV, my_locality, my_locationRemarks, my_fieldNotes, 
    na.rm = TRUE, sep = "-")
  my_keepers_24$date <- stringr::str_replace(my_keepers_24$date, 
    pattern = "-[0-9]+-[0-9]+-[0-9]+", replacement = "")
  writeLines(paste(" - Extracting year from fieldNotes, locationRemarks, and verbatimEventDate ", 
    "columns in ambiguous formats...", sep = ""))
  ambiguousDateStrings <- c(
    "[0-9]{1,2}[\\s-/ ]+[0-9]{1,2}[\\s-/ ]+[0-9]{4}", 
    "[0-9]{2}[\\s-/ ]+[0-9]{2}[\\s-/ ]+[0-9]{4}", 
    "[0-9]{1,2}[\\s-/ ]+[0-9]{1,2}[\\s-/ ]+[0-9]{2}", 
    "[0-9]{2}[\\s-/ ]+[0-9]{2}[\\s-/ ]+[0-9]{2}")
  id2remove_30 <- c(my_keepers_24$database_id, id2remove_24)
  ambiguousNames <- noDATEa %>% dplyr::filter(!database_id %in% 
    id2remove_30) %>% dplyr::mutate(amb_vEV = stringr::str_extract(verbatimEventDate, 
    pattern = paste(ambiguousDateStrings, collapse = "|", sep = "")), 
    amb_locality = stringr::str_extract(locality, pattern = paste(ambiguousDateStrings, collapse = "|", sep = "")), 
    amb_fieldNotes = stringr::str_extract(fieldNotes, pattern = paste(ambiguousDateStrings, collapse = "|", sep = "")), 
    amb_locationRemarks = stringr::str_extract(locationRemarks, pattern = paste(ambiguousDateStrings, collapse = "|", sep = ""))) %>% 
    dplyr::select(database_id, amb_vEV, amb_locality, amb_fieldNotes, amb_locationRemarks)
  ambiguousNames$amb_vEV <- ambiguousNames$amb_vEV %>% 
    stringr::str_replace(pattern = "00[-/ ]00[-/ ]", replacement = "01-01-") %>% 
    stringr::str_replace(pattern = "00[-/ ]", replacement = "01-") %>% 
    stringr::str_replace(pattern = "00-00-|[-/ ]00$|[-/ ]00[-/ ]00$|^00|^00[-/ ]00[-/ ]", replacement = "") %>% 
    lubridate::dmy(truncated = 2, quiet = TRUE)
  ambiguousNames$amb_locality <- ambiguousNames$amb_locality %>% 
    stringr::str_replace(pattern = "00[-/ ]00[-/ ]", replacement = "01-01-") %>% 
    stringr::str_replace(pattern = "00[-/ ]", replacement = "01-") %>% 
    stringr::str_replace(pattern = "00-00-|[-/ ]00$|[-/ ]00[-/ ]00$|^00|^00[-/ ]00[-/ ]", replacement = "") %>% 
    lubridate::dmy(truncated = 2, quiet = TRUE)
  ambiguousNames$amb_fieldNotes <- ambiguousNames$amb_fieldNotes %>% 
    stringr::str_replace(pattern = "00[-/ ]00[-/ ]", replacement = "01-01-") %>% 
    stringr::str_replace(pattern = "00[-/ ]", replacement = "01-") %>% 
    stringr::str_replace(pattern = "00-00-|[-/ ]00$|[-/ ]00[-/ ]00$|^00|^00[-/ ]00[-/ ]", replacement = "") %>% 
    lubridate::dmy(truncated = 2, quiet = TRUE)
  ambiguousNames$amb_locationRemarks <- ambiguousNames$amb_locationRemarks %>% 
    stringr::str_replace(pattern = "00[-/ ]00[-/ ]", replacement = "01-01-") %>% 
    stringr::str_replace(pattern = "00[-/ ]", replacement = "01-") %>% 
    stringr::str_replace(pattern = "00-00-|[-/ ]00$|[-/ ]00[-/ ]00$|^00|^00[-/ ]00[-/ ]", replacement = "") %>% 
    lubridate::dmy(truncated = 2,  quiet = TRUE)
  amb_keepers_30 <- ambiguousNames %>% 
    dplyr::filter(complete.cases(amb_vEV) | 
    complete.cases(amb_locality) | 
    complete.cases(amb_locationRemarks) | 
    complete.cases(amb_fieldNotes)) %>% 
    tidyr::unite(col = date, 
                 amb_vEV, 
                 amb_locality, 
                 amb_locationRemarks, 
                 amb_fieldNotes, 
                 na.rm = TRUE)
  writeLines(paste(" - Formatting and combining the new data..", sep = ""))
  occYr_2$date <- as.character(occYr_2$date) %>% 
    lubridate::ymd_hms() %>% 
    lubridate::date()
  ymd_keepers_21$date <- lubridate::ymd(ymd_keepers_21$date)
  dmy_keepers_22$date <- lubridate::ymd(dmy_keepers_22$date)
  mdy_keepers_23$date <- lubridate::ymd(mdy_keepers_23$date)
  my_keepers_24$date <- lubridate::ymd(my_keepers_24$date)
  saveTheDates <- dplyr::bind_rows(ymd_hms_0, 
                                   dmy_1, 
                                   occYr_2, 
                                   ymd_keepers_21, 
                                   dmy_keepers_22, 
                                   mdy_keepers_23) %>% 
                  dplyr::select(database_id, date)
  datesOut_full <- data %>% dplyr::right_join(saveTheDates, by = "database_id")
  # TODO: Here is the problem. The ymd_hms format from the ymd_hms0 table does not get parsed.
  datesOut_full$eventDate <- lubridate::parse_date_time(datesOut_full$date, 
                                                         orders = c("ymd", 
                                                                    "ymdHMS", 
                                                                    "dmy", 
                                                                    "mdy"), 
                                                         truncated = 5, 
                                                         quiet = TRUE, 
                                                         tz = "UTC", 
                                                         locale = Sys.getlocale("LC_TIME"))
  datesOut_full$year <- lubridate::year(datesOut_full$date)
  datesOut_full$month <- lubridate::month(datesOut_full$date)
  datesOut_full$day <- lubridate::day(datesOut_full$date)
  datesOut_full <- datesOut_full %>%
    dplyr::filter(!year > maxYear) %>% 
    dplyr::filter(!year < minYear) %>% 
    dplyr::select(!date)
  datesOut_noDay <- data %>% 
    dplyr::right_join(my_keepers_24, by = "database_id")
  datesOut_noDay$eventDate <- lubridate::ymd(datesOut_noDay$date, quiet = TRUE)
  datesOut_noDay$year <- lubridate::year(datesOut_noDay$date)
  datesOut_noDay$month <- lubridate::month(datesOut_noDay$date)
  datesOut_noDay <- datesOut_noDay %>% 
    dplyr::filter(!year > maxYear) %>% 
    dplyr::filter(!year < minYear) %>% 
    dplyr::select(!date)
  datesOut_noMonth <- data %>% 
    dplyr::right_join(amb_keepers_30, by = "database_id")
  datesOut_noMonth$eventDate <- lubridate::ymd(datesOut_noMonth$date, quiet = TRUE)
  datesOut_noMonth$year <- lubridate::year(datesOut_noMonth$date)
  datesOut_noMonth <- datesOut_noMonth %>% 
    dplyr::filter(!year > maxYear) %>% 
    dplyr::filter(!year < minYear) %>% 
    dplyr::select(!date)
  writeLines(paste(" - Merging all data, nearly there...", sep = ""))
  datesMerged <- dplyr::bind_rows(datesOut_full, datesOut_noDay, datesOut_noMonth)
  datesOut <- data
  datesOut$eventDate <- lubridate::ymd_hms(datesOut$eventDate, truncated = 5, quiet = TRUE)
  datesOut <- datesOut %>% 
    dplyr::filter(!database_id %in% 
    datesMerged$database_id)
  datesOut$year <- ifelse(is.na(datesOut$year), lubridate::year(datesOut$eventDate), datesOut$year)
  datesOut$month <- ifelse(is.na(datesOut$month), lubridate::month(datesOut$eventDate), datesOut$month)
  datesOut$day <- ifelse(is.na(datesOut$day), lubridate::day(datesOut$eventDate), datesOut$day)
  datesOut$month <- ifelse(datesOut$year > maxYear | datesOut$year <  minYear, 
                           NA, 
                           datesOut$month)
  datesOut$day <- ifelse(datesOut$year > maxYear | datesOut$year < minYear, 
                         NA, 
                         datesOut$day)
  datesOut$year <- ifelse(datesOut$year > maxYear | datesOut$year < minYear, 
                          NA, 
                          datesOut$year)
  datesOut$eventDate <- ifelse(lubridate::year(datesOut$eventDate) > 
    maxYear | lubridate::year(datesOut$eventDate) < minYear, 
    NA, as.character(datesOut$eventDate))
  datesOut$eventDate <- as.character(datesOut$eventDate)
  datesMerged$eventDate <- as.character(datesMerged$eventDate)
  dates_complete <- data %>% dplyr::mutate(eventDate = as.character(eventDate)) %>% 
    dplyr::filter(!database_id %in% c(datesMerged$database_id, 
      datesOut$database_id)) %>% dplyr::bind_rows(datesMerged, 
    datesOut)
  dates_complete$eventDate <- lubridate::ymd_hms(dates_complete$eventDate, 
    truncated = 5, quiet = TRUE)
  timeEnd <- Sys.time()
  writeLines(paste(" - Finished. \n", "We now have ", format((sum(complete.cases(dates_complete$eventDate)) - 
    originalDateCount), big.mark = ","), " more full eventDate cells than in the input data.\n", 
    "We modified dates in \n", format(nrow(datesMerged), 
      big.mark = ","), " occurrences.\n", " - As it stands, there are ", 
    format(sum(complete.cases(dates_complete$eventDate)), 
      big.mark = ","), " complete eventDates and ", format(sum(is.na(dates_complete$eventDate)), 
      big.mark = ","), " missing dates.\n", " - There are also ", 
    format(sum(complete.cases(dates_complete$year)), big.mark = ","), 
    " complete year occurrences to filter from. This is up from an initial count of ", 
    format(sum(complete.cases(data$year)), big.mark = ","), 
    " At this rate, you will stand to lose ", format(sum(is.na(dates_complete$year)), 
      big.mark = ","), " occurrences on the basis of missing", 
    " year", " - Operation time: ", (timeEnd - timeStart), 
    " ", units(round(timeEnd - timeStart, digits = 2)), 
    sep = ""))
  return(dates_complete)
}