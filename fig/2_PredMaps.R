# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 2. Fit prediction Map
#-------------------------------------------------------------------------------
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)


# 1. Set data repository and load rasters---------------------------------------

# 1.1. Bathymetry
bathy<- raster("input/gebco/Bathy.tif")
print(bathy)
# plot(bathy)
# Convert bathy raster to data frame
bathy_df <- as.data.frame(bathy, xy = TRUE)
#Colour for bathymetry:
# Create a mask if you dont want to plot all the bathymetricla range
#bathy_bb <-  bathy_df$Bathy <= 5 #only upper break
bathy_filtered <- bathy_df %>%
  filter(Bathy >= -800 & Bathy <= 5)
# Apply the mask
print(bathy_filtered)



# 1.2. Landmask
mask <- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
print(mask)
mask <- st_transform(mask, crs = 4326)
# crop it:
e <- c(-3, 5, 35, 43)
e <- extent(e)
bbox <- st_as_sfc(st_bbox(e))
# Set the CRS of bbox to match the mask
st_crs(bbox) <- st_crs(mask) 
#Crop the mask using the bounding box
mask <- st_intersection(mask, bbox)
print(mask)


# 1.3. Bathymetric contour
Bathy_cont<- st_read("input/emodnet/Bathy/contour1/Contours_2022.shp")
Bathy_cont$Elevation <- as.numeric(Bathy_cont$Elevation)
print(Bathy_cont)
#Select the bathymetrical lines that you want to plot:
Bathy_cont1 <- Bathy_cont %>%
  filter(Elevation %in% c(50, 200, 800))
# Set the CRS for the raster
st_crs(Bathy_cont1) <- st_crs(mask)
print(Bathy_cont1)

# 1.4. GSAs
GSA <- st_read("input/GSAs/GSAs_simplified.shp")
print(GSA)
# Filter the sf object to keep only the features where SECT_COD is "GSA06"
GSA_filtered <- GSA %>%
  filter(SECT_COD == "GSA06")
print(GSA_filtered)

# 1.5. Predicted habitat
mod_code <- "brt"
genus <- "Raja" #"Raja" #"Scyliorhinus"
type <- "_Nkm2" #"_Nk2" #"_PA"
season <- "2021"
path <- paste0("output/", mod_code, "/", paste0(genus, type), "/predict_boost/2021/", season, "_pred_median.tif")
habitat <- raster(path)
class(habitat)

# Convert raster to data frame
habitat_df <- as.data.frame(habitat, xy = TRUE)
colnames(habitat_df) <- c("x", "y", "habitat")
summary(habitat_df)

# Ensure CRS matches for all spatial data
st_crs(mask) <- 4326
st_crs(GSA_filtered) <- st_crs(mask)
st_crs(Bathy_cont1) <- st_crs(mask)


# 2. Crop habitat to GSA06 area ------------------------------------------------
# Convert habitat_df to an sf object (using original x, y coordinates)
habitat_sf <- st_as_sf(habitat_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
#plot(habitat_sf)

# Ensure CRS compatibility between habitat_sf and GSA_filtered
if (st_crs(habitat_sf) != st_crs(GSA_filtered)) {
  habitat_sf <- st_transform(habitat_sf, crs = st_crs(GSA_filtered))
}

# Perform the intersection
habitat_clipped_sf <- st_intersection(habitat_sf, GSA_filtered)

# Convert the clipped sf object back to a data frame (with original coordinates)
# Ensure that we retain the original x and y coordinates
habitat_clipped_df <- as.data.frame(habitat_clipped_sf) %>%
  dplyr::select(x, y, habitat)  # Keep the original x, y, and habitat columns

# Check for any remaining NAs and clean up the data
habitat_clipped_df <- habitat_clipped_df %>%
  filter(!is.na(habitat))

# Verify the result
summary(habitat_clipped_df)




# 3. Crop habitat to bathymetric range 800 m -----------------------------------
bathy_sf <- st_as_sf(bathy_filtered, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
print(bathy_sf)
#plot(bathy_sf)

# Check CRS and align if necessary
if (st_crs(habitat_clipped_sf) != st_crs(bathy_sf)) {
  habitat_clipped_sf <- st_transform(habitat_clipped_sf, crs = st_crs(bathy_sf))
}
# Perform spatial intersection to crop habitat_clipped_sf by bathy_sf
# Ensure bathy_sf is also an sf object with proper geometry
habitat_cropped_sf <- st_intersection(habitat_clipped_sf, bathy_sf)

# Convert the cropped result back to a data frame, if needed
habitat_cropped_df <- as.data.frame(habitat_cropped_sf) %>%
  select(x, y, habitat)  # Keep only relevant columns

# Inspect the result
summary(habitat_cropped_df)
str(habitat_cropped_df)

write.csv(habitat_cropped_df, "habitat_cropped.csv", row.names = FALSE)


# 3. Make zoomed in map---------------------------------------------------------
# Define the plot
p <- ggplot() +
  # Plot habitat raster
  geom_tile(data = habitat_cropped_df, aes(x = x, y = y, fill = habitat)) +
  scale_fill_viridis_c(option = "viridis", name = "Habitat") +
  
  # Plot bathymetric contours
  geom_sf(data = Bathy_cont1, color = "black", size = 0.5) +
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80", color = "grey60") +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Set spatial bounds (adjust these to fit your data)
  coord_sf(xlim = c(-1, 4), ylim = c(36.5, 42.5), expand = TRUE) +
  
  # Add scale bar (optional)
  annotation_scale(location = "bl", width_hint = 0.2) + 
  
  # Theme settings
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal") +
  
  # Title and labels
  ggtitle(paste(genus, "   Model:", mod_code, "\n", season)) +
  xlab("Longitude") +
  ylab("Latitude")

# Print the plot
print(p)

p + guides(fill = guide_legend(title = "Legend Title"))

