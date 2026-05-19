library(igraph)
library(dbscan)
library(sf)

#' S2 method derived from Megan Barkdull’s code for clustering localities into “sites” 
#' based on distance (used with permission)
#' 
#' @param species_sf An sf object. The occurrence data for a single species as an SF object.
#' @param sep_distance A number. The separation distance, in kilometers, by which to calculate occurrence clusters
#' Default = 1
#' @param precise_calculations A boolean. Whether or not to use aeqd-based clustering function (more precise, very time/RAM intensive) or the s2 function (less precise, very quick)
#' Default TRUE
#' 
#' @returns number_of_clusters A number. The count of occurrence clusters calculated by the function.

# Collapsing sites so that nearby localities are clustered into a single "site"
count_occ_clusters <- function(species_sf, sep_distance = 1, precise_calculations = TRUE) {

  # Safeguard against species with 0 observations
  if (nrow(species_sf) == 0) return(0)
  
  . <- snapshot_locations <- snapshot_locations_sf <- distance_matrix <- cutoff <- 
    distance_matrix_cutoff <- adjacency_network <- number_of_clusters <- NULL
  
  #### Define site clusters for Snapshot Sites, and count clusters:
  
  # Make sure species_sf is in degrees
  snapshot_locations <- st_transform(species_sf, 4326) %>%
    dplyr::distinct(geometry)
  
  if (precise_calculations == TRUE) {
    # Make sure we're using s2 (degrees)
    old_s2 <- sf::sf_use_s2()
    sf::sf_use_s2(TRUE)
    on.exit(sf::sf_use_s2(old_s2))
    
    # Define site clusters for Snapshot Sites, and count clusters:
    # This is where RAM and time are massively affected — we're calculating between all points
    distance_matrix <- st_distance(snapshot_locations)
    
    adjacency_matrix <- matrix(as.numeric(as.numeric(distance_matrix)) < (sep_distance * 1000),
                               nrow = nrow(distance_matrix))
    
    adjacencyNetwork <- graph_from_adjacency_matrix(adjacency_matrix)
    
    # By doing this, igraph has assigned each site to a group (a component); associate the component ID with each site:
    snapshot_locations$group <- factor(components(adjacencyNetwork)$membership)
    
    # Count clusters
    number_of_clusters <- length(unique(as.character(snapshot_locations$group)))
    
  } else if (precise_calculations == FALSE) {
    # Get centroid of coords for projection
    centroid <- st_coordinates(st_centroid(st_union(species_sf)))
    aeqd_crs <- sprintf("+proj=aeqd +lat_0=%f +lon_0=%f +datum=WGS84 +units=m",
                        centroid[2], centroid[1])
    
    # Project species_sf using Azimuthal Equidistant CRS around centroid
    species_projected <- st_transform(species_sf, crs=st_crs(aeqd_crs))
    
    # Get coordinates matrix
    coords <- st_coordinates(species_projected)
    
    # Cluster using dbscan—each point belonging to a cluster
    clusters <- dbscan(coords, eps=sep_distance * 1000, minPts=1)
    
    # Count unique clusters
    number_of_clusters <- length(unique(clusters$cluster[clusters$cluster > 0]))
  }
  
  return(number_of_clusters)
}