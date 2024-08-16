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
Bathy_cont<- st_read("input/gebco/cont/gebco_contours4osm.shp")
print(Bathy_cont)

Bathy_cont$DEPTH <- as.numeric(Bathy_cont$DEPTH)
unique(Bathy_cont$DEPTH)

#Select the bathymetrical lines that you want to plot:
Bathy_cont1 <- Bathy_cont %>%
  filter(DEPTH %in% c(-100, -200, -750))
unique(Bathy_cont1$DEPTH)
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
print(habitat)

# Ensure CRS matches for all spatial data
st_crs(mask) <- 4326
st_crs(GSA_filtered) <- st_crs(mask)
st_crs(Bathy_cont1) <- st_crs(mask)




# 2. Crop habitat to bathy 800 m---------------------------------------------------
# Define the function to create the mask
create_mask <- function(raster_layer, min_value, max_value) {
  # Apply the condition to the raster
  mask <- calc(raster_layer, fun = function(x) {
    x[x >= min_value & x <= max_value] <- 1
    x[x < min_value | x > max_value] <- NA
    return(x)
  })
  return(mask)
}

# Create the mask with values from 0 to 800
bathy_mask <- create_mask(bathy, -750, 5)
#plot(bathy_mask)

# Resample bathy_mask to match the resolution of habitat
bathy_mask_resampled <- resample(bathy_mask, habitat, method = "bilinear")

# Apply the mask to the habitat raster
habitat_cropped <- mask(habitat, bathy_mask_resampled)
plot(habitat_cropped)

# Convert raster to data frame
habitat_df <- as.data.frame(habitat_cropped, xy = TRUE)
colnames(habitat_df) <- c("x", "y", "habitat")
summary(habitat_df)




# 3. Crop habitat to GSA06 area ------------------------------------------------
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


# 4. Crop bathymetric contours to GSA06 ------------------------------------------
# Set the CRS of Bathy_cont1 to match GSA_filtered if needed
st_crs(Bathy_cont1) <- st_crs(GSA_filtered)
Bathy_cropped <- st_intersection(Bathy_cont1, GSA_filtered)




# 5. Make zoomed in map---------------------------------------------------------
# Define the plot
p <- ggplot() +
  # Plot habitat raster
  geom_tile(data = habitat_clipped_df, aes(x = x, y = y, fill = habitat)) +
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


p + guides(fill = guide_legend(title = "Legend Title"))

# export plot
outdir <- paste0(output_data, "/fig/Map")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", mod_code, "_", paste0(genus, type), "_habitat_Map.png")
ggsave(p_png, p, width=23, height=17, units="cm", dpi=300)
