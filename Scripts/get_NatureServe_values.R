get_NatureServe_values <- function(x, sep_distance = 1, boundary, precise_occurrences = TRUE, year_range = F, occupied_county = F) {
  # Load necessary packages
  require(tidyverse)
  require(tigris)
  require(sf)
  require(red)
  require(lwgeom)
  
  # Make sure we're using s2 (global calculations) for area calculations
  old_s2 <- sf::sf_use_s2()
  sf::sf_use_s2(TRUE)
  on.exit(sf::sf_use_s2(old_s2))
  
  # Make sure boundary is in degrees CRS (4326)
  boundary <- st_transform(boundary, 4326)
  
  x <- x %>%
    filter(!is.na(decimalLatitude) & !is.na(decimalLongitude))
  # Check if the dataframe is grouped
  is_grouped <- dplyr::is.grouped_df(x)
  
  # If grouped, split by group. Otherwise, treat the whole dataframe as one group
  grouped_data <- if (is_grouped) {
    dplyr::group_split(x)
  } else {
    list(x)
  }
  
  # Retrieve group keys if available
  group_keys_df <- if (is_grouped) {
    dplyr::group_keys(x)
  } else {
    NULL
  }
  
  # Process each group
  results <- map2(grouped_data, seq_along(grouped_data), function(species_data, idx) {
    # Create sf object
    species_sf <- species_data %>%
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
    filtered_sf <- species_sf %>%
      filter(lengths(st_intersects(., boundary)) > 0)
    
    # Calculate number of observations (# of unique specimen records)
    observations <- nrow(filtered_sf)

    message("Getting occurrence count.")
    if (precise_occurrences) {
      message("This operation is expensive with large datasets when using precise_occurrences = TRUE")
    }
    
    # Calculate number of occurrences
    occurrences <- count_occ_clusters(
      filtered_sf,
      sep_distance=sep_distance,
      precise_calculations=precise_occurrences
    )
    
    filtered_sf %>%
      st_union() %>%
      st_convex_hull() %>%
      st_intersection(boundary) %>%
      st_geometry_type()
    
    # WARNING: This 'hull' is not a useful metric on a global scale
    # Calculating over antimeridian causes 'flip' of polygon edges
    hull <- filtered_sf %>%
      st_union() %>%
      st_convex_hull() %>%
      st_intersection(boundary)
    
    # Calculate range extent (Convex Hull Intersected with Boundary) in km^2
    if (observations <= 2) {
      range_extent <- NA
      
    } else {
      range_extent <- filtered_sf %>%
        st_union() %>%
        st_convex_hull() %>%
        st_intersection(boundary) %>%
        st_area() %>%
        units::set_units(km^2)
    }
    
    # Calculate area of occupancy (AOO) in km^2
    coords <- filtered_sf %>%
      st_coordinates() %>%
      as.data.frame()
    
    if (nrow(coords) == 0) {
      area_of_occupancy <- NA
    } else {
      area_of_occupancy <- red::aoo(coords) %>%
        units::set_units(km^2)
    }
    
    # Determine year range of observations
    if (year_range) {
      year_range_val <- species_data %>%
        summarise(range_str = ifelse(
          min(year, na.rm = TRUE) == max(year, na.rm = TRUE),
          as.character(min(year, na.rm = TRUE)),
          paste0(min(year, na.rm = TRUE), '-', max(year, na.rm = TRUE)))
        )
      
    }
    # Determine county list of observations
    if (occupied_county) {
      occupied_county_val <- filtered_sf %>%
        dplyr::filter(!is.na(county)) %>%
        summarise(county_list = paste(unique(county), collapse = ", "))
    }
    # make sure range extent values are greater than or equal to area of occupancy values, per IUCN standards
    if (!is.na(range_extent) && as.numeric(range_extent) < as.numeric(area_of_occupancy)) {
      range_extent <- as.numeric(area_of_occupancy)
    } else {
      range_extent <- as.numeric(range_extent)
    }
    
    # Create result tibble
    result <- tibble(
      observations = observations,
      occurrences = occurrences,
      range_extent = range_extent,
      area_of_occupancy = as.numeric(area_of_occupancy),
      year_range = if (year_range) year_range_val$range_str else NULL,
      occupied_county = if (occupied_county) occupied_county_val$county_list else NULL
    )
    
    # If grouped, add group keys to result
    if (!is.null(group_keys_df)) {
      result <- bind_cols(group_keys_df[idx, ], result)
    }
    
    return(result)
  })
  
  # Combine results
  results_df <- bind_rows(results)
  
  # provide warning about NatureServe calculations if there are any NA values for range extent
  # THIS PART DOESN'T NEED TO BE IN EACH IF STATEMENT!!
  if(any(is.na(results_df$range_extent)))
  {
    warning('NA range extent values present for some taxa - not enough information to complete NatureServe analyses for these taxa')
  }
  
  return(results_df)
  
}