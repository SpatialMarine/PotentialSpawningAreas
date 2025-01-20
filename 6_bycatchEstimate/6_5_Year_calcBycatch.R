# ------------------------------------------------------------------------------

# Title: Predicting Potential Spawning Areas: a novel framework for elasmobranch conservation and spatial management 

#---------------------------------------------------------------------------------------------------
# 6.5. Estimate total bycatch for the overall year based on fishing effort and bycatch prediction maps
#---------------------------------------------------------------------------------------------------
# You need to combine the daily trawl fishing effort maps with the daily bycatch per unit of effort maps. 
library(raster)


# 1. Calculate total bycatch ---------------------------------------------------
# 1.1.Load stacks---------------------------------------------------------------
# Yearly overall fishing effort:
yFE <- raster("input/gfwr/rawdata/high_resolution/summarydata/2021_FE.tif")
plot(yFE)

# Yearly overall BPUE:
genus <- "Raja" 
family <- "LN_gaussian_Final2" 
type <- "_NKm2" 
mod_code <- "brt"
season <- "2021"

genus <- "Scyliorhinus" 
family <- "bernuilli_Final2" 
type <- "_PA" 
mod_code <- "brt"
dataset <- "ALL" 

path <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_hurdle_pred_median.tif")
yBPUE <- raster(path)
print(yBPUE)

# Crop yFE to the extent of yBPUE
yFE <- projectRaster(yFE, yBPUE)
yFE <- resample(yFE, yBPUE, method = "bilinear") 
yFE_cropped <- crop(yFE, yBPUE)
print(yFE_cropped)
plot(yFE_cropped)
print(yBPUE)

# Ensure both stacks have the same dimensions and alignment
if (!compareRaster(yFE_cropped, yBPUE)) {
  stop("Effort and BPU maps are not aligned. Check projection and resolution.")
}

# 1.2. Crop yBPUE to study area -----------------------------------------------------
bathy<- raster("input/gebco/Bathy.tif")
print(bathy)
# plot(bathy)
# Convert bathy raster to data frame
bathy_df <- as.data.frame(bathy, xy = TRUE)
bathy_filtered <- bathy_df %>%
  filter(Bathy >= -800 & Bathy <= 5)
# Apply the mask
print(bathy_filtered)

# Filter the values between -20 and -700 and set values outside the range to NA
bathy_filtered <- calc(bathy, function(x) {
  x[x > -20 | x < -700] <- NA  # Set values outside the range to NA
  return(x)
})

# Assign a value of 1 to the remaining (non-NA) values
bathy_mask <- calc(bathy_filtered, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})

# Resample bathy_mask to match the resolution of habitat
bathy_mask_resampled <- resample(bathy_mask, yBPUE, method = "bilinear")

# Apply the mask to the habitat raster
habitat_cropped <- raster::mask(yBPUE, bathy_mask_resampled)

# Convert raster to data frame
habitat_df <- as.data.frame(habitat_cropped, xy = TRUE)
colnames(habitat_df) <- c("x", "y", "habitat")
summary(habitat_df)

GSA <- st_read("input/GSAs/GSAs_simplified.shp")
# Filter the sf object to keep only the features where SECT_COD is "GSA06"
GSA_filtered <- GSA %>%
  filter(SECT_COD == "GSA06")
print(GSA_filtered)

# Convert habitat_df to an sf object (using original x, y coordinates)
habitat_sf <- st_as_sf(habitat_df, coords = c("x", "y"), crs = st_crs(mask), remove = FALSE)
#plot(habitat_sf)

# Ensure CRS compatibility between habitat_sf and GSA_filtered
if (st_crs(habitat_sf) != st_crs(GSA_filtered)) {
  habitat_sf <- st_transform(habitat_sf, crs = st_crs(GSA_filtered))
}

# Perform the intersection
habitat_clipped_sf <- st_intersection(habitat_sf, GSA_filtered)
# Convert the habitat column using expm1
habitat_clipped_sf$habitat <- expm1(habitat_clipped_sf$habitat)
print(habitat_clipped_sf)


# 2.Calculate total bycatch-----------------------------------------------------
# Formula: Total bycatch in grid cell (x,𝑦) on day𝑡 = fishing effort in grid cell (x,𝑦) on day𝑡x Bycatch per unit effort in grid cell (x,𝑦; B(t,x,y) =E(t,x,y) × BPUE(t,x,y) 
print(yFE_cropped)
print(habitat_clipped_sf)

# 2.1. Calculate total bycatch maps---------------------------------------------
# Extract fishing effort values from yFE_cropped for each point in habitat_clipped_sf
habitat_clipped_sf$fishing_effort <- extract(yFE_cropped, habitat_clipped_sf)
# Calculate egg cases captured per cell as habitat * fishing_effort
habitat_clipped_sf$total_captures <- habitat_clipped_sf$habitat * habitat_clipped_sf$fishing_effort
print(habitat_clipped_sf)

# 2.2. Sum across all grid cells for each day-----------------------------------
# Formula: Bt = ∑B(t,x,y)
# Sum up the total captures across all cells
total_egg_cases <- sum(habitat_clipped_sf$total_captures, na.rm = TRUE)
# Print the result
cat("Total bycatch for 2021:", total_egg_cases, "egg cases for", genus)
# Total bycatch for 2021: 162,558 egg cases for Raja
# Total bycatch for 2021: 145,378.8 egg cases for Scyliorhinus


# Convert the clipped sf object back to a data frame (with original coordinates)
# Ensure that we retain the original x and y coordinates
habitat_clipped_df <- as.data.frame(habitat_clipped_sf) %>%
  dplyr::select(x, y, habitat, fishing_effort, total_captures)  # Keep the original x, y, and habitat columns

# Check for any remaining NAs and clean up the data
habitat_clipped_df <- habitat_clipped_df %>%
  filter(!is.na(habitat))

# Verify the result
summary(habitat_clipped_df)

# 3. Plot total_egg_cases-------------------------------------------------------
# Define the plot
p <- ggplot() +
  # Plot habitat raster
  geom_tile(data = habitat_clipped_df, aes(x = x, y = y, fill = total_captures)) +
  scale_fill_viridis_c(option = "viridis", name = "N") +
  
  # Plot land mask
  geom_sf(data = mask, fill = "grey80", color = "grey60") +
  
  # Plot GSAs
  geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
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

p

# export plot
outdir <- paste0(output_data, "/fig/Map/", paste0(genus, type, "_", family))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/total_bycatch.png")
ggsave(p_png, p, width=20, height=20, units="cm", dpi=1800)

