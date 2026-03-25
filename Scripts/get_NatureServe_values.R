get_NatureServe_values <- function(x, boundary, year_range = F, occupied_county = F) {
  # Load necessary packages
  require(tidyverse)
  require(tigris)
  require(sf)
  require(red)
  require(lwgeom)
  
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
    
    # Calculate number of observations (# of unique specimen records)
    observations <- species_sf %>%
      filter(lengths(st_intersects(., boundary)) > 0) %>%
      nrow()
    
    # AEP TEMP: Calculate number of occurrences (# of unique specimen records with distinct locations)
    # occurrence_grid <- species_sf %>%
    #   filter(lengths(st_intersects(., boundary)) > 0) %>%
    #   distinct(geometry) %>%
    #   st_make_grid(cellsize = sqrt(2e6),
    #                what = 'polygons') %>%
    #   st_sf(grid_id = 1:length(.)) %>%
    #   st_make_valid()
    # 
    # points_with_overlap <- st_join(species_sf, occurrence_grid, 
    #                             join = st_within) %>%
    #   group_by(grid_id) %>%
    #   summarise(n_points = n(), .groups = "drop") %>%
    #   filter(n_points > 1) %>%
    #   filter(grid_id %in% points_per_cell$grid_id)
    # 
    
    # AEP TEMP: IF there is any overlap between points, retain only the first record (or the one with finer resolution?)
    # then count the number of unique points
    # nrow()
    
    # TEMP OCCURRENCE CALCS - UPDATE
    occurrences <- species_sf %>%
      st_join(boundary, join = st_intersects) %>%
      distinct(geometry) %>% 
      nrow()
    
    # Calculate range extent (Convex Hull Intersected with Boundary) in km^2
    if (occurrences <= 2) {
      range_extent <- NA
      
    } else {
      range_extent <- species_sf %>%
        st_union() %>%
        st_convex_hull() %>%
        st_intersection(boundary) %>%
        st_area() %>%
        units::set_units(km^2)
    }
    
    # Calculate area of occupancy (AOO) in km^2
    coords <- species_sf %>%
      filter(lengths(st_intersects(., boundary)) > 0) %>%
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
          min(year) == max(year),
          as.character(min(year)),
          paste0(min(year), '-', max(year)))
        )
      
    }
    # Determine county list of observations
    if (occupied_county) {
      occupied_county_val <- species_sf %>%
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