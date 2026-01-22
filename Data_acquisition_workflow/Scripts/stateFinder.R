# This function attempts to impute a stateProvince value from lat/long coordinates if the current stateProvince value is 'NA'. It has been adapted from BeeBDC's jbd_CfC_chunker() function. Note that the matching process here is conservative - there are sometimes some discrepancies with the iso_a2 codes from rnaturalearth. When matching is uncertain (i.e., the iso_a2 code doesn't match up, or an occurrence's lat/long coordinates are right on the border of two states), an 'NA' value is retained rather than attempting a guess. 

# author: Avery E. Pearson
# last updated: 22 January 2026

stateFinder <- function (data = NULL, lat = "decimalLatitude", lon = "decimalLongitude", 
          stateProvince = "stateProvince", stepSize = 1e+06, chunkStart = 1, scale = "large", 
          path = tempdir(), mc.cores = 1) 
{
  BeeBDC_order <- . <- .data <- id_temp <- name <- geometry <- inData <- stateProvince_OG <- NULL
  startTime <- Sys.time()
  nChunks = ceiling(nrow(data)/stepSize)
  nrowMax <- nrow(data)
  chunkEnd = (chunkStart + stepSize) - 1
  message(paste(" - Running chunker with:", "\n", "stepSize = ", 
                format(stepSize, big.mark = ",", scientific = FALSE), 
                "\n", "chunkStart = ", format(chunkStart, big.mark = ",", 
                                              scientific = FALSE), "\n", "chunkEnd = ", format(chunkEnd, 
                                                                                               big.mark = ",", scientific = FALSE), sep = ""))
  funCoordStateProvince <- function(inData) {
    suppressWarnings({
     # check_require_cran("rnaturalearth")
      if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
        stop(paste0("Package `", rnaturalearth, "` needed for this function to work! Please, install it with: `install.packages(\"", pkg, "\")`"), call. = FALSE)
      } else {
        require('rnaturalearth', character.only = TRUE)
      }
    })
    inData$id_temp <- 1:nrow(inData)
    minimum_colnames <- c(lat, lon)
    if (!all(minimum_colnames %in% colnames(inData))) {
      stop("These columns names were not found in your database: ", 
           paste(minimum_colnames[!minimum_colnames %in% 
                                    colnames(inData)], collapse = ", "), call. = FALSE)
    }
    has_stateProvince <- any(colnames(inData) == stateProvince)
    if (!has_stateProvince) {
      inData$stateProvince <- NA
    }
    inData <- inData %>% dplyr::mutate(decimalLatitude = as.numeric(.data[[lat]]), 
                                       decimalLongitude = as.numeric(.data[[lon]]))
    
    unique_countries <- inData %>%
      filter(is.na(stateProvince)) %>%
      distinct(countryCode, country_suggested) %>%
      drop_na(countryCode) 
    
    statelist <- lapply(seq_len(nrow(unique_countries)), function(idx) {
      iso <- unique_countries$countryCode[idx]
      country_suggested <- unique_countries$country_suggested[idx]
      
      # Try ISO first
      res <- tryCatch(
        ne_states(iso_a2 = iso, returnclass = "sf"),
        error = function(e) NULL
      )
      
      # Fallback to country name if ISO fails
      if (is.null(res) || nrow(res) == 0) {
        res <- tryCatch(
          ne_states(country = country_suggested, returnclass = "sf"),
          error = function(e) NULL
        )
      }
      
      if (!is.null(res) && nrow(res) > 0) {
        st_make_valid(res)
      } else {
        NULL
      }
    })
    
    # Remove skipped entries
    statelist <- Filter(Negate(is.null), statelist)
    
    statemap <- dplyr::bind_rows(statelist)
    
    data_no_state <- inData %>% dplyr::filter(is.na(stateProvince) | 
                                                  stateProvince == "")
    if (nrow(data_no_state) == 0) {
      inData <- inData %>% dplyr::select(-id_temp)
    }
    else {
      suppressWarnings({
        data_no_state <- CoordinateCleaner::cc_val(x = data_no_state, 
                                                     lon = lon, lat = lat, verbose = FALSE) %>% 
          sf::st_as_sf(., coords = c("decimalLongitude", 
                                     "decimalLatitude"), remove = FALSE) %>% 
          sf::st_set_crs(., sf::st_crs(statemap))
      })
      
      ext_state <- st_join(
        data_no_state %>% 
          dplyr::select(id_temp, geometry),
        statemap %>% 
          dplyr::select(name),
        left = TRUE
      ) %>%
        group_by(id_temp) %>%
        mutate(
          n_matches = sum(!is.na(name)),
          name = if_else(n_matches == 1, name, NA_character_)
        ) %>%
        ungroup() %>%
        distinct(id_temp, .keep_all = TRUE) %>%
        dplyr::select(-n_matches)

      ext_state$geometry <- NULL
      res <- dplyr::left_join(data_no_state, ext_state, 
                              by = "id_temp") %>% dplyr::distinct(id_temp, 
                                                                  .keep_all = TRUE)
      id_replace <- res$id_temp
      inData[id_replace, "stateProvince"] <- res$name
      inData <- inData %>% dplyr::select(-id_temp)
    }
    return(dplyr::as_tibble(inData))
  }
  writeLines(paste(" - Starting parallel operation. Unlike the serial operation (mc.cores = 1)", 
                   ", a parallel operation will not provide running feedback. Please be patient", 
                   " as this function may take some time to complete. Each chunk will be run on", 
                   " a seperate thread so also be aware of RAM usage."))
  loop_check_pf = data %>% dplyr::mutate(BeeBDC_order = dplyr::row_number()) %>% 
    dplyr::group_by(BeeBDC_group = ceiling(BeeBDC_order/stepSize)) %>% 
    dplyr::group_split(.keep = TRUE) %>% parallel::mclapply(., 
                                                            funCoordStateProvince, mc.cores = mc.cores) %>% dplyr::bind_rows()
  stateList = dplyr::tibble(database_id = loop_check_pf$database_id, 
                              stateProvince = loop_check_pf$stateProvince, BeeBDC_order = loop_check_pf$BeeBDC_order) %>% 
    dplyr::arrange(BeeBDC_order) %>% dplyr::select(!tidyselect::any_of("BeeBDC_order"))
  colnames(stateList) <- c("database_id", "stateProvince")
  endTime <- Sys.time()
  message(paste(" - Completed in ", round(difftime(endTime, 
                                                   startTime), digits = 2), " ", units(round(endTime - 
                                                                                               startTime, digits = 2)), sep = ""))
  data <- data %>% dplyr::select(tidyselect::any_of(c("database_id", 
                                                      "stateProvince")))
  stateList <- stateList %>% tidyr::drop_na(stateProvince)
  summaryTable <- stateList %>% dplyr::left_join(data, by = "database_id", 
                                                   suffix = c("", "_OG")) %>% dplyr::mutate(changed = dplyr::if_else(is.na(stateProvince_OG), 
                                                                                                                     1, 0))
  writeLines(paste0(" - We have updated the stateProvince names of ", 
                    format(sum(summaryTable$changed), big.mark = ","), " occurrences that previously had no stateProvince name assigned."))
  return(stateList)
}
