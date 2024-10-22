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
library(sp)
library(beepr)


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



# 1.3. GSAs
GSA <- st_read("input/GSAs/GSAs_simplified.shp")
#print(GSA)
# Filter the sf object to keep only the features where SECT_COD is "GSA06"
GSA_filtered <- GSA %>%
  filter(SECT_COD == "GSA06")
print(GSA_filtered)




# 2.Adjust fishing effort with potential spawning areas and MPAs-----------------
# RUN ONLY ONCE:
# 5.1. Clean PSAs:
#genus <- "Scyliorhinus" #Scyliorhinus, Raja
#
#path <- paste0("output/shapefiles/", genus, "/", genus, "MASK_habitat_raster.tif")
#PSAr <- raster(path)
#plot(PSAr)
#print(PSAr)
#
## Create contour lines for the raster
#polygon_layer <- rasterToPolygons(PSAr, dissolve = TRUE)
## Convert contour lines to spatial polygons (sf object)
#contour_sf <- st_as_sf(polygon_layer)
## Simplify to the outer boundary by merging polygons
#outer_boundary <- st_union(contour_sf)
## Plot the contours
#plot(outer_boundary)
#print(outer_boundary)
#
## Smoothing the polygon
#library(rmapshaper)  # Optional for simplification
#library(smoothr)     # For smoothing
#outer_boundary <- st_as_sf(outer_boundary)
#smoothed_boundary <- smooth(outer_boundary, method = "ksmooth", smoothness = 3)
#plot(smoothed_boundary)
#
## Define the output shapefile path
#output_shapefile <- paste0("output/shapefiles/", genus, "_outer_boundary.shp")
## Ensure the output directory exists
#if (!dir.exists(dirname(output_shapefile))) {
#  dir.create(dirname(output_shapefile), recursive = TRUE)
#}
## Save the outer boundary as a shapefile
#st_write(smoothed_boundary, output_shapefile, append = FALSE)


# 3. Load layers-------------------------------------------------------------
PSA_Raja <- st_read("output/shapefiles/Raja_outer_boundary.shp")
PSA_Sca <- st_read("output/shapefiles/Scyliorhinus_outer_boundary.shp")
plot(PSA_Sca)
plot(PSA_Raja)

# Load fishing effort:
fishingEffort <- raster("input/gfwr/summarydata/high_resolution/FishingEffort.tif")
fishingEffort

# Create a new raster with the desired resolution (0.042 degrees)
res <- 0.042
fishingEffort_resampled <- raster(extent(fishingEffort), resolution = res, crs = crs(fishingEffort))

# Resample using bilinear interpolation
fishingEffort <- resample(fishingEffort, fishingEffort_resampled, method = "bilinear")

# Crop to FE
# Define the extent for cropping (xmin, xmax, ymin, ymax)
#crop_extent <- extent(-1.5, 6.2, 35.5, 43)
# Crop the raster using the defined extent
#fishingEffort <- crop(fishingEffort, crop_extent)


# Convert the clipped sf object back to a data frame (with original coordinates)
# Ensure that we retain the original x and y coordinates
FE_df <- as.data.frame(fishingEffort, xy = TRUE)

# Convert habitat_df to an sf object (using original x, y coordinates)
fishingEffort_sf <- st_as_sf(FE_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
#plot(fishingEffort_sf)

# Ensure CRS compatibility between habitat_sf and GSA_filtered
if (st_crs(fishingEffort_sf) != st_crs(GSA_filtered)) {
  fishingEffort_sf <- st_transform(fishingEffort_sf, crs = st_crs(GSA_filtered))
}

# Perform the intersection
fishingEffort_clipped_sf <- st_intersection(fishingEffort_sf, GSA_filtered)
#plot(fishingEffort_clipped_sf)

# convert to df
fishingEffort_df <- as.data.frame(fishingEffort_clipped_sf, xy = TRUE)

# Replace NA values in the 'FishingEffort' column with 0
library(tidyr)
FE_withinGSA_df <- fishingEffort_clipped_sf %>%
  mutate(FishingEffort = replace_na(FishingEffort, 0))

# Revert the log1p() of the response variable
FE_withinGSA_df$ln_FishingEffort <- log1p(FE_withinGSA_df$FishingEffort)

# Verify the result
summary(FE_withinGSA_df)


# Load MPAs:
# run only once:
#MPA <- st_read("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_20240531_shp/ProtectedSeas_Navigator_20240531_shp.shp")
#head(MPA)
#MPA_merged <- st_make_valid(MPA_merged)
# Identify invalid geometries
#invalid_geometries <- !st_is_valid(MPA_merged)
#sum(invalid_geometries)
# Filter out invalid geometries
#MPA_clean <- MPA_merged[!invalid_geometries, ]
#beep()
#invalid_geometries <- !st_is_valid(MPA_clean)
#sum(invalid_geometries)
#beep()
#Save:
#st_write(MPA_clean, "input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Valid.shp", append=FALSE)
#beep()
#
#MPA <- st_read("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Valid.shp")
#head(MPA)
#atributes <- read.csv("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Attributes_20240531.csv")
#
## Merge atributes into MPA based on shared 'site_id' and 'SITE_ID'
#MPA_merged <- MPA %>%
#  left_join(atributes, by = c("SITE_ID" = "site_id"))
#print(MPA_merged)
#
## Check CRS of GSA_filtered and MPA
#print(st_crs(GSA_filtered))  # Check the CRS of GSA_filtered
#print(st_crs(MPA_merged))           # Check the CRS of MPA
#
#if (st_crs(MPA_merged) != st_crs(GSA_filtered)) {
#  MPA_merged <- st_transform(MPA_merged, st_crs(GSA_filtered))
#}
#
# Filter to remove unwanted
# Keep only MPAs that intersect with GSA_filtered
#MPA_cropped <- st_intersection(MPA_merged, GSA_filtered)
#beep()
##plot(MPA_cropped)
#
#ggplot() +
#  geom_sf(data = MPA_cropped, fill = "lightblue", color = "darkblue") +
#  theme_minimal() +
#  ggtitle("MPA Cropped Areas") +
#  theme(plot.title = element_text(hjust = 0.5))
#
##Save
#st_write(MPA_cropped, "input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Valid_Cropped.shp", append=FALSE)
#beep()

#Load cropped MPAs to study area:
MPA_cropped <- st_read("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Valid_Cropped.shp")

atributes <- read.csv("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Attributes_20240531.csv")
MPA_merged <- MPA_cropped %>%
  left_join(atributes, by = c("SITE_ID" = "site_id"))
print(MPA_merged)

# Filter the MPAs types: 
#fishing_mpas <- MPA_merged #if you want them all
unique_values <- unique(MPA_merged$removal_of_marine_life_is_prohibited)

# Loop over the unique values and filter the data for each value
for (value in unique_values) {
  # Create a dynamic variable name (e.g., MPA_1, MPA_2, etc.)
  var_name <- paste0("MPA_", value)
  
  # Filter the dataset based on the current value of removal_of_marine_life_is_prohibited
  filtered_data <- MPA_merged %>%
    filter(removal_of_marine_life_is_prohibited == value)
  
  # Assign the filtered data to the dynamically created variable
  assign(var_name, filtered_data)
  
  # Optionally, print or summarize the filtered dataset
  print(paste("Summary of", var_name))
  print(summary(filtered_data))
}

# remove not real MPAs:
summary(MPA_1)
MPA_1 <- subset(MPA_1, !(SITE_ID %in% c('AIDZA1', 'AIESP1', 'AIFRA1', 'AIITA1', 'AIHSMA4', 'AIESP3')))




# 4. Plot overlap between MPAs and PSAs ----------------------------------------
#colRamp <- colorRampPalette(c('#FFFAFA','#FFE4E1','#FFC1C1','#FF8C69','#CD5B45','#8B3E2F', '#2B130E'))(100)

MPA_colors <- c("#003DA5", "#4CAF50", "#FFEB3B", "#FF9800", "#F44336") 
#MPA_colors <- c("#66FF66", "#4CAF50", "#38A745", "#007300", "#004d00")
#MPA_colors <- c("#A3C1E0", "#66B3B3", "#4CAF50", "#388E3C", "#005C26")

#PSA_Rajac <- "darkred"
#PSA_Scac <- "#663399"

#PSA_Rajac <- "#663399" 
#PSA_Scac <- "#FF6F61"   

PSA_Rajac <- "steelblue"  # Dark Teal for PSA_Rajac
PSA_Scac <- "orange"   # Coral for PSA_Scac


# Define the plot
p <- ggplot() +
  # Plot habitat raster (fishing effort)
  #geom_tile(data = FE_withinGSA_df, aes(x = x, y = y, fill = ln_FishingEffort), alpha = 0.7) +
  #scale_fill_viridis_c(option = "viridis", name = "fishing effort") +
  
  # Plot MPAs with varying transparency and colors to indicate restriction levels
  geom_sf(data = MPA_1, fill = MPA_colors[1], color = MPA_colors[1], size = 1.2, alpha = 0.3) +
  geom_sf(data = MPA_2, fill = MPA_colors[2], color = MPA_colors[2], size = 1.2, alpha = 0.3) +
  geom_sf(data = MPA_3, fill = MPA_colors[3], color = MPA_colors[3], size = 1.2, alpha = 0.3) +
  geom_sf(data = MPA_4, fill = MPA_colors[4], color = MPA_colors[4], size = 1.2, alpha = 0.3) +
  geom_sf(data = MPA_5, fill = MPA_colors[5], color = MPA_colors[5], size = 1.2, alpha = 0.3) +
  
  # Plot PSAs with bold color and solid lines
  geom_sf(data = PSA_Raja, fill = PSA_Rajac, color = "black", size = 1, linetype = "solid", alpha = 0.6) +
  geom_sf(data = PSA_Sca, fill = PSA_Scac, color = "black", size = 1, linetype = "solid", alpha = 0.6) +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80") +
  
  # Apply color scale for fishing effort
  #scale_fill_gradientn(name = "fishingEffort", colors = colRamp, na.value = "transparent") +
  
  # Set spatial bounds
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Theme and styling
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1)

# Display the plot
p


# export plot
enviro <- "MPAs_PSAs" #slope, fishingEffort, 
outdir <- paste0(output_data, "/fig/Map/overlapFE_PSA")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", enviro, "_.jpeg")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=1800)


# 5. Plot overlap between FE and PSAs ------------------------------------------
#colRamp <- colorRampPalette(c('#FFFAFA','#FFE4E1','#FFC1C1','#FF8C69','#CD5B45','#8B3E2F', '#2B130E'))(100)
#colRamp  <- colorRampPalette(c('#E0F7FA', '#80DEEA', '#00ACC1', '#8E24AA', '#4A148C'))(100)

PSA_Rajac <- "steelblue"  # Dark Teal for PSA_Rajac
PSA_Scac <- "orange"   # Coral for PSA_Scac


# Define the plot
p <- ggplot() +
  # Plot habitat raster (fishing effort)
  geom_tile(data = FE_withinGSA_df, aes(x = x, y = y, fill = ln_FishingEffort), alpha = 1) +
  scale_fill_viridis_c(option = "viridis", name = "fishing effort") +
  
  # Plot PSAs with bold color and solid lines
  geom_sf(data = PSA_Raja, fill = PSA_Rajac, color = "darkblue", size = 5, linetype = "solid", alpha = 0.2) +
  geom_sf(data = PSA_Sca, fill = PSA_Scac, color = "darkorange", size = 5, linetype = "solid", alpha = 0.2) +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80") +
  
  # Apply color scale for fishing effort
  #scale_fill_gradientn(name = "fishingEffort", colors = colRamp, na.value = "transparent") +
  
  # Set spatial bounds
  coord_sf(xlim = c(-1, 5.8), ylim = c(36.5, 42.2), expand = TRUE) +
  
  # Theme and styling
  theme_bw() +
  #theme(panel.grid = element_blank(),
  #      legend.position = "right",
  #      legend.box = "vertical",
  #      aspect.ratio = 1)
  
  #If you dont want legend:
  theme(panel.grid = element_blank(),
      legend.position = "none",     # Remove legend
      aspect.ratio = 1)
# Display the plot
p

# export plot
enviro <- "FE_PSAs" #slope, fishingEffort, 
outdir <- paste0(output_data, "/fig/Map/overlapFE_PSA")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", enviro, "2_.jpeg")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=1800)




# 6. Check change in fishing effort in relation to bathymetry----------------------
# Extract bathymetry values at fishing effort locations


fishing_effort_bathy <- raster::extract(bathy, FE_withinGSA_df[, c("x", "y")])
# Add bathymetry values to fishing effort data
FE_withinGSA_df$bathy <- fishing_effort_bathy

# model relationship:
library(mgcv)
FE_withinGSA_df <- na.omit(FE_withinGSA_df) 
FE_withinGSA_df$bathy <- abs(FE_withinGSA_df$bathy)
summary(FE_withinGSA_df$bathy) 
hist(FE_withinGSA_df$ln_FishingEffort)
summary(FE_withinGSA_df$ln_FishingEffort)
ks.test(FE_withinGSA_df$ln_FishingEffort, "pnorm", mean = mean(FE_withinGSA_df$ln_FishingEffort), sd = sd(FE_withinGSA_df$ln_FishingEffort))

# GAM:
#gam_model <- gam(ln_FishingEffort ~ s(bathy), data = FE_withinGSA_df, family = gaussian(link = "identity"))
#summary(gam_model)  # Check model summary and significance
#
## Plot the GAM
#png("output/fig/Map/overlapFE_PSA/gam_bathymetry_effect.png", width = 800, height = 800, res = 120)
#plot(gam_model, se = TRUE, shade = TRUE, main = "Effect of Bathymetry on Fishing Effort")
#dev.off()

# BRT:
library(gbm)
brt_model <- gbm(ln_FishingEffort ~ bathy, data = FE_withinGSA_df, distribution = "laplace", n.trees = 1000, interaction.depth = 3)
# Plot the fitted BRT curve
png("output/fig/Map/overlapFE_PSA/brt_bathymetry_effect.png", width = 800, height = 800, res = 120)
plot(brt_model, i.var = "bathy")
dev.off()

