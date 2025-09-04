flag_spatial_intersects <- function(data_table, fp, column_names) {
  
  # Get list of new columns and delete in original table if they already exist
  new_colnames <- paste0("INTERSECTS_", column_names)
  data_table <- data_table %>% dplyr::select(-dplyr::any_of(new_colnames))
  
  # Use GEOS to automatically handle duplicate vertex issues  
  sf::sf_use_s2(FALSE)
  
  # Read in .shp file
  shapefile <- read_sf(fp)
  
  # Convert observations table to sf object
  obs_sf <- st_as_sf(
    data_table, 
    coords = c("decimalLongitude", "decimalLatitude"), crs = 4326
  )
  
  # Reproject shapefile if needed
  if (st_crs(obs_sf) != st_crs(shapefile)) {
    shapefile <- st_transform(shapefile, crs = 4326)
  }
  
  # Get subset of .shp, and rename selected columns with INTERSECTS_ prefix
  shapefile_subset <- shapefile %>%
    dplyr::select(all_of(column_names)) %>%
    rename_with(~ paste0("INTERSECTS_", .x))
  
  # Spatial join (match points to polygons)
  joined <- st_join(obs_sf, shapefile_subset, join=st_intersects)

  aggregated <- joined %>%
    st_drop_geometry() %>%
    dplyr::group_by(recordId) %>%
    dplyr::summarize(
      across(
        all_of(new_colnames),
        ~ list(sort(unique(.x)))
      ),
      .groups = "drop"
    )
  
  result <- data_table %>%
    dplyr::left_join(aggregated, by = "recordId")
  
  return(result)
}