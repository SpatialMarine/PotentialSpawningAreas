# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 2.3. Calculate distances from features to study points
#-------------------------------------------------------------------------------
# MEJORAR NO PASAR POR TIERRA

library(geosphere)
library(sf)
library(gdistance)
library(sp)
library(raster)

data <- read.csv2("temp/env_data.csv", sep = ";")

# use same temporal resolution (day) and numeric for lon and lat
data$date <- as.Date(data$date) #if your time scale has not hours
data$lon <- as.numeric(data$lon)
data$lat <- as.numeric(data$lat)
range(data$date)
range(data$lon)
range(data$lat)

# Make an ocean mask so you calculate distance only without accounting for the land
bathy <- raster("input/gebco/gebco_bathy.tif")
print(bathy)

# transform 0 to NA
bathy[bathy >= 0] <- NA
print(bathy)

# Eliminate Atlantic data (xmin, xmax, ymin, ymax)
b <- as(extent(-6, 0, 42, 46), 'SpatialPolygons')
crs(b) <- crs(bathy)
# Use the mask function to apply the mask to bathy
bathy <- mask(bathy, b, inverse = TRUE)
plot(bathy_d)

# create ocean mask using the bathymetry
mask <- bathy/bathy

# change to a coarser resolution
mask_ag <- aggregate(mask, fact = 10)
plot(mask_ag)

# create surface
tr1 <- transition(mask_ag, transitionFunction=mean, directions=16)
tr1C <- geoCorrection(tr1)
print(tr1C)

point1<- c(3.2041567, 41.82413)
dist2col <- accCost(tr1C, point1)
dist2col[is.infinite(dist2col)] <- NA
plot(dist2col)




# SOMETHING LIKE THIS?

# Function to calculate minimum distance to seamounts considering land mask
min_distance_to_point_feature <- function(location, feature, land_mask) {
  # Get coordinates of the location
  loc_coords <- st_coordinates(location)
  # Find nearest feature in cropped_mounts to the location
  nearest_feature <- st_nearest_feature(location, feature)
  # Get coordinates of the nearest feature
  feature_coords <- st_coordinates(nearest_feature)
  
  # Create transition matrix from land mask (considering 16 directions)
  tr1 <- transition(land_mask, transitionFunction = mean, directions = 16)
  tr1C <- geoCorrection(tr1)
  
  # Calculate accumulated cost distance to nearest feature location
  dist2col <- accCost(tr1C, loc_coords)
  
  # If the nearest feature is over land, find the nearest point on land and use it as the endpoint
  if (raster::extract(land_mask, feature_coords) == 1) {
    nearest_land_point <- st_nearest_points(location, land_mask)
    feature_coords <- st_coordinates(nearest_land_point)
  }
  
  # Calculate geodesic distance between location and adjusted feature coordinates
  distance <- distGeo(loc_coords, feature_coords)
  
  # Use accumulated cost distance if it is finite and less than geodesic distance
  if (!is.na(dist2col) && dist2col < distance) {
    distance <- dist2col
  }
  
  return(distance)
}

# Load your study locations (already defined as study_locations)
# Load cropped_mounts and mask_ag
# cropped_mounts <- st_read("path_to_cropped_mounts.shp")
# mask_ag <- raster("path_to_mask_ag.tif")

# Calculate distances for each study location
distances <- sapply(1:nrow(study_locations), function(i) {
  min_distance_to_point_feature(study_locations[i, ], cropped_mounts, mask_ag)
})


















# 1) From point feature (.shp) -------------------------------------------------
# 1.1) Sea Mounts
mounts <- st_read("input/YessonEtAl2019-Seamounts-V2/YessonEtAl2019-Seamounts-V2.shp")
mounts

# crop to the Mediterranean extent
mediterranean_bbox <- st_bbox(c(xmin = -6.25, xmax = 36.25, ymin = 30.0, ymax = 46.0), crs = st_crs(4326))
mediterranean_extent <- st_as_sfc(mediterranean_bbox)
cropped_mounts <- st_intersection(mounts, mediterranean_extent)
#plot(st_geometry(cropped_mounts))
cropped_mounts

# Function to calculate minimum distance to seamounts
min_distance_to_point_feature <- function(location, feature, land_mask) {
  #location: a single study location represented as an sf object.
  #seamounts: a collection of features also represented as an sf object.
  # Get coordinates of the location
  loc_coords <- st_coordinates(location)
  # Get coordinates of all feature
  feature_coords <- st_coordinates(feature)
  # Calculate distances from the location to all feature
  distances <- distGeo(loc_coords, feature_coords)
  # calculates geodesic distance (the shortest distance over the earth's surface) between the study location and each feature
  # thus, distances, is a vector where each element represents the distance from the study location to a corresponding feature.
  # Return the minimum distance - smallest value in the distances vector, which represents the shortest distance from the study location to any feature
  return(min(distances))
}

# Create a data frame with your study locations and convert it to an sf object
study_locations <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
#plot(st_geometry(study_locations))
study_locations

# Calculate distances for each study location
distances <- sapply(1:nrow(study_locations), function(i) {
  min_distance_to_point_feature(study_locations[i, ], cropped_mounts)
})

# Add distances to the data frame
data$distance_to_seamount <- distances / 1000  # Convert to kilometers


# 2) From multipolygon feature (.shp) ------------------------------------------
# 2.1) Canyons
canyons <- st_read("input/global_seafloor_features/Canyons.shp")
canyons
# Clean the geometries
canyons_valid <- st_make_valid(canyons)
canyons_valid

# crop to the Mediterranean extent
mediterranean_bbox <- st_bbox(c(xmin = -6.25, xmax = 36.25, ymin = 30.0, ymax = 46.0), crs = st_crs(4326))
mediterranean_extent <- st_as_sfc(mediterranean_bbox)
cropped_canyons <- st_intersection(canyons_valid, mediterranean_extent)
cropped_canyons

# Convert all geometries in cropped_canyons to MULTIPOLYGON
cropped_canyons <- st_cast(cropped_canyons, "MULTIPOLYGON")
#plot(st_geometry(cropped_canyons))

# Function to calculate minimum distance to features
min_distance_to_poly_feature <- function(location, features) {
  # Get coordinates of the location
  loc_coords <- st_coordinates(location)
  loc_coords <- loc_coords[, c("X", "Y")]  # Extract X and Y coordinates
  
  # Initialize minimum distance
  min_distance <- Inf
  
  # Loop through each feature to find the minimum distance
  for (i in seq_len(nrow(features))) {
    # Get the coordinates of the current feature
    feature_coords <- st_coordinates(features[i, ])
    feature_coords <- feature_coords[, c("X", "Y")]  # Extract X and Y coordinates
    
    # Calculate the distance from the location to the current feature
    distances <- distGeo(loc_coords, feature_coords)
    
    # Update the minimum distance
    min_distance <- min(min_distance, min(distances, na.rm = TRUE))
  }
  
  # Return the minimum distance
  return(min_distance)
}

# Create a data frame with your study locations and convert it to an sf object
study_locations <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
study_locations

# Calculate distances for each study location
distances <- sapply(1:nrow(study_locations), function(i) {
  min_distance_to_poly_feature(study_locations[i, ], cropped_canyons)
})

beep()

# Add distances to the data frame
data$distance_to_canyons <- distances / 1000  # Convert to kilometers


# 1.5) Fans
fans <- st_read("input/global_seafloor_features/fans.shp")
fans

# Clean the geometries
fans_valid <- st_make_valid(fans)
fans_valid

# crop to the Mediterranean extent
mediterranean_bbox <- st_bbox(c(xmin = -6.25, xmax = 36.25, ymin = 30.0, ymax = 46.0), crs = st_crs(4326))
mediterranean_extent <- st_as_sfc(mediterranean_bbox)
cropped_fans <- st_intersection(fans_valid, mediterranean_extent)
cropped_fans

# Convert all geometries in cropped_canyons to MULTIPOLYGON
cropped_fans <- st_cast(cropped_fans, "MULTIPOLYGON")
#plot(st_geometry(cropped_fans))

# Calculate distances for each study location
distances <- sapply(1:nrow(study_locations), function(i) {
  min_distance_to_feature(study_locations[i, ], cropped_fans)
})

beep()

# Add distances to the data frame
data$distance_to_fans <- distances / 1000  # Convert to kilometers

# Save dataframe
write.csv2(data, "temp/data_2D_dist.csv", row.names = FALSE)
