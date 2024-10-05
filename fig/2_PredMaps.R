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

genus <- "Raja" #"Raja" #"Scyliorhinus"
family <- "LN_laplace_Final" #bernuilli #LN_laplace_sinO2
type <- "_NKm2" #"_NKm2" "_PA" "only_P
mod_code <- "brt"
dataset <- "ALL" #ALL, train
season <- "2021"

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
#print(mask)
mask <- st_transform(mask, crs = 4326)
# crop it:
e <- c(-3, 7, 35, 43)
e <- extent(e)
bbox <- st_as_sfc(st_bbox(e))
# Set the CRS of bbox to match the mask
st_crs(bbox) <- st_crs(mask) 
#Crop the mask using the bounding box
mask <- st_intersection(mask, bbox)
print(mask)


# 1.3. Bathymetric contour
#Bathy_cont<- st_read("input/gebco/cont/gebco_contours4osm.shp")
#print(Bathy_cont)
#
#Bathy_cont$DEPTH <- as.numeric(Bathy_cont$DEPTH)
#unique(Bathy_cont$DEPTH)
#
##Select the bathymetrical lines that you want to plot:
#Bathy_cont1 <- Bathy_cont %>%
#  filter(DEPTH %in% c(-750)) #-100, -200, 
#unique(Bathy_cont1$DEPTH)
## Set the CRS for the raster
#st_crs(Bathy_cont1) <- st_crs(mask)
#print(Bathy_cont1)

# 1.4. GSAs
GSA <- st_read("input/GSAs/GSAs_simplified.shp")
#print(GSA)
# Filter the sf object to keep only the features where SECT_COD is "GSA06"
GSA_filtered <- GSA %>%
  filter(SECT_COD == "GSA06")
print(GSA_filtered)


# 1.5. Predicted habitat
path <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_pred_median.tif")
#path <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/01/20210101_Scyliorhinus_brt_pred.tif")
habitat <- raster(path)
#habitat <- calc(habitat, function(x) 10^x)
print(habitat)


# Ensure CRS matches for all spatial data
st_crs(mask) <- 4326
st_crs(GSA_filtered) <- st_crs(mask)
#st_crs(Bathy_cont1) <- st_crs(mask)




# 2. Crop habitat to bathy 800 m---------------------------------------------------
# Filter the values between -50 and -600 and set values outside the range to NA
bathy_filtered <- calc(bathy, function(x) {
  x[x > -20 | x < -690] <- NA  # Set values outside the range to NA
  return(x)
})

# Assign a value of 1 to the remaining (non-NA) values
bathy_mask <- calc(bathy_filtered, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})


# Resample bathy_mask to match the resolution of habitat
bathy_mask_resampled <- resample(bathy_mask, habitat, method = "bilinear")

# Apply the mask to the habitat raster
habitat_cropped <- raster::mask(habitat, bathy_mask_resampled)
#plot(habitat_cropped)

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

# Revert the log1p() of the response variable
habitat_clipped_df$habitat <- expm1(habitat_clipped_df$habitat)

# Verify the result
summary(habitat_clipped_df)



# 4. Crop bathymetric contours to GSA06 ------------------------------------------
# Set the CRS of Bathy_cont1 to match GSA_filtered if needed
#st_crs(Bathy_cont1) <- st_crs(GSA_filtered)
#Bathy_cropped <- st_intersection(Bathy_cont1, GSA_filtered)
#str(Bathy_cropped)


# 5. Make HABITAT zoomed in map---------------------------------------------------------
# Define the plot
p <- ggplot() +
  # Plot habitat raster
  geom_tile(data = habitat_clipped_df, aes(x = x, y = y, fill = habitat)) +
  scale_fill_viridis_c(option = "viridis", name = "Ln(density) N/km2") +
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80", color = "grey60") +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Plot bathymetric contours
  #geom_sf(data = Bathy_cropped, color = "black", size = 0.1, alpha = 0.5) +
  
  # Set spatial bounds (adjust these to fit your data)
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Add scale bar (optional)
  #annotation_scale(location = "bl", width_hint = 0.2) + 
  
  # Theme settings
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1) 
  
  # Title and labels
  #ggtitle(paste(genus, "   Model:", mod_code, "\n", season)) +
  #xlab("Longitude") +
  #ylab("Latitude")

p

# export plot
outdir <- paste0(output_data, "/fig/Map/", paste0(genus, type, "_", family))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", mod_code, "_", paste0(genus, type, "_", family), "_EXP_habitat_Map_vars.png")
ggsave(p_png, p, width=20, height=20, units="cm", dpi=1800)








# 6. Load error map ------------------------------------------------------------
# 1.5. Predicted habitat

path <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_pred_CIR_median.tif")
error <- raster(path)
#error <- calc(error, expm1)
print(error)




# 7. Crop error to bathy 800 m------------------------------------------------
# Filter the values between -50 and -600 and set values outside the range to NA
bathy_filtered <- calc(bathy, function(x) {
  x[x > -20 | x < -690] <- NA  # Set values outside the range to NA
  return(x)
})

# Assign a value of 1 to the remaining (non-NA) values
bathy_mask <- calc(bathy_filtered, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})

# Resample bathy_mask to match the resolution of habitat
bathy_mask_resampled <- resample(bathy_mask, error, method = "bilinear")

# Apply the mask to the habitat raster
error_cropped <- mask(error, bathy_mask_resampled)
#plot(error_cropped)

# Convert raster to data frame
error_df <- as.data.frame(error_cropped, xy = TRUE)
colnames(error_df) <- c("x", "y", "error")
summary(error_df)




# Crop habitat to GSA06 area ------------------------------------------------
# Convert habitat_df to an sf object (using original x, y coordinates)
error_sf <- st_as_sf(error_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
#plot(error_sf)

# Ensure CRS compatibility between error_sf and GSA_filtered
if (st_crs(error_sf) != st_crs(GSA_filtered)) {
  error_sf <- st_transform(error_sf, crs = st_crs(GSA_filtered))
}

# Perform the intersection
error_clipped_sf <- st_intersection(error_sf, GSA_filtered)

# Convert the clipped sf object back to a data frame (with original coordinates)
# Ensure that we retain the original x and y coordinates
error_clipped_df <- as.data.frame(error_clipped_sf) %>%
  dplyr::select(x, y, error)  # Keep the original x, y, and error columns

# Check for any remaining NAs and clean up the data
error_clipped_df <- error_clipped_df %>%
  filter(!is.na(error))

# Revert the log1p() of the response variable
error_clipped_df$error <- expm1(error_clipped_df$error)

# Verify the result
summary(error_clipped_df)



# 5. Make 95% CI zoomed in map---------------------------------------------------------
#colors <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026", "#000000")
colors <- c("#FFFFB2", "#FECC5C", "#FDBF6F", "#F03B20", "#BD0026", "#8B0000")
# Create a function to generate a color palette
color_palette <- colorRampPalette(colors)
# Generate a gradient with a specified number of colors
num_colors <- 100  # Adjust this to the number of colors you need
gradient_colors <- color_palette(num_colors)


# Define the plot
p <- ggplot() +
  # Plot error raster
  geom_tile(data = error_clipped_df, aes(x = x, y = y, fill = error)) +
  scale_fill_gradientn(colors = gradient_colors, name = "95% CI") +
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80") +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Plot bathymetric contours
  #geom_sf(data = Bathy_cropped, color = "black", size = 0.1, alpha = 0.5) +
  
  # Set spatial bounds (adjust these to fit your data)
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Add scale bar (optional)
  annotation_scale(location = "bl", width_hint = 0.2) + 
  
  # Theme settings
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1) 

# Title and labels
#ggtitle(paste(genus, "   Model:", mod_code, "\n", season)) +
#xlab("Longitude") +
#ylab("Latitude")

p

# export plot
outdir <- paste0(output_data, "/fig/Map/", paste0(genus, type, "_", family))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", mod_code, "_", paste0(genus, type, "_", family), "_EXP_CIR_Map_scale_vars.png")
ggsave(p_png, p, width=20, height=20, units="cm", dpi=1800)






# 7. Load ERROR map ------------------------------------------------------------
# SD and SE

path <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_pred_SD_median.tif")
SD <- raster(path)
print(SD)
path <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_pred_SE_median.tif")
SE <- raster(path)
print(SE)



# 8. Crop SD and SE to bathy 800 m------------------------------------------------
# Filter the values between -50 and -600 and set values outside the range to NA
bathy_filtered <- calc(bathy, function(x) {
  x[x > -20 | x < -600] <- NA  # Set values outside the range to NA
  return(x)
})

# Assign a value of 1 to the remaining (non-NA) values
bathy_mask <- calc(bathy_filtered, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})

# Resample bathy_mask to match the resolution of habitat
bathy_mask_resampled <- resample(bathy_mask, SD, method = "bilinear")

# Apply the mask to the habitat raster
SD_cropped <- mask(SD, bathy_mask_resampled)
SE_cropped <- mask(SE, bathy_mask_resampled)

# Convert raster to data frame
SD_df <- as.data.frame(SD_cropped, xy = TRUE)
colnames(SD_df) <- c("x", "y", "SD")
summary(SD_df)

SE_df <- as.data.frame(SE_cropped, xy = TRUE)
colnames(SE_df) <- c("x", "y", "SE")
summary(SE_df)


# 3. Crop habitat to GSA06 area ------------------------------------------------
# Convert habitat_df to an sf object (using original x, y coordinates)
SD_sf <- st_as_sf(SD_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
SE_sf <- st_as_sf(SE_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)

# Ensure CRS compatibility between error_sf and GSA_filtered
if (st_crs(SD_sf) != st_crs(GSA_filtered)) {
  SD_sf <- st_transform(SD_sf, crs = st_crs(GSA_filtered))
}
if (st_crs(SE_sf) != st_crs(GSA_filtered)) {
  SE_sf <- st_transform(SE_sf, crs = st_crs(GSA_filtered))
}

# Perform the intersection
SD_clipped_sf <- st_intersection(SD_sf, GSA_filtered)
SE_clipped_sf <- st_intersection(SE_sf, GSA_filtered)

# Convert the clipped sf object back to a data frame (with original coordinates)
# Ensure that we retain the original x and y coordinates
SD_clipped_df <- as.data.frame(SD_clipped_sf) %>%
  dplyr::select(x, y, SD)  # Keep the original x, y, and error columns
SE_clipped_df <- as.data.frame(SE_clipped_sf) %>%
  dplyr::select(x, y, SE)  # Keep the original x, y, and error columns

# Check for any remaining NAs and clean up the data
SD_clipped_df <- SD_clipped_df %>%
  filter(!is.na(SD))
SE_clipped_df <- SE_clipped_df %>%
  filter(!is.na(SE))

# Verify the result
summary(SD_clipped_df)
summary(SE_clipped_df)

# 5. Make SD zoomed in map---------------------------------------------------------
#colors <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026", "#000000")
colors <- c("#FFFFB2", "#FECC5C", "#FDBF6F", "#F03B20", "#BD0026", "#8B0000")
# Create a function to generate a color palette
color_palette <- colorRampPalette(colors)
# Generate a gradient with a specified number of colors
num_colors <- 100  # Adjust this to the number of colors you need
gradient_colors <- color_palette(num_colors)


# Define the plot
p <- ggplot() +
  # Plot error raster
  geom_tile(data = SD_clipped_df, aes(x = x, y = y, fill = SD)) +
  scale_fill_gradientn(colors = gradient_colors, name = "SD") +
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80") +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Plot bathymetric contours
  #geom_sf(data = Bathy_cropped, color = "black", size = 0.1, alpha = 0.5) +
  
  # Set spatial bounds (adjust these to fit your data)
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Add scale bar (optional)
  annotation_scale(location = "bl", width_hint = 0.2) + 
  
  # Theme settings
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1) 

# Title and labels
#ggtitle(paste(genus, "   Model:", mod_code, "\n", season)) +
#xlab("Longitude") +
#ylab("Latitude")

p

# export plot
outdir <- paste0(output_data, "/fig/Map/", paste0(genus, type, "_", family))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", mod_code, "_", paste0(genus, type, "_", family), "_SD_Map_scale_vars.png")
ggsave(p_png, p, width=20, height=20, units="cm", dpi=1800)



# Define the plot
p <- ggplot() +
  # Plot error raster
  geom_tile(data = SE_clipped_df, aes(x = x, y = y, fill = SE)) +
  scale_fill_gradientn(colors = gradient_colors, name = "SE") +
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80") +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Plot bathymetric contours
  #geom_sf(data = Bathy_cropped, color = "black", size = 0.1, alpha = 0.5) +
  
  # Set spatial bounds (adjust these to fit your data)
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Add scale bar (optional)
  annotation_scale(location = "bl", width_hint = 0.2) + 
  
  # Theme settings
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1) 

# Title and labels
#ggtitle(paste(genus, "   Model:", mod_code, "\n", season)) +
#xlab("Longitude") +
#ylab("Latitude")

p

# export plot
outdir <- paste0(output_data, "/fig/Map/", paste0(genus, type, "_", family))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", mod_code, "_", paste0(genus, type, "_", family), "_SE_Map_scale_vars.png")
ggsave(p_png, p, width=15, height=22, units="cm", dpi=1200)
