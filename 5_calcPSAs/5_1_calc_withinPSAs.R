# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 4.9. Calculate parameters within and outised potensial spawining areas (PSAs)
#-------------------------------------------------------------------------------
library(terra)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(sp)

genus <- "Raja" #"Raja" #"Scyliorhinus"
mod_code <- "brt"
family <- "LN_laplace_Final" #bernuilli #LN_laplace_sinO2
type <- "_NKm2" #"_NKm2" "_PA" "only_P
season <- "2021"

# 1. Load areas:

# 1.1. PSAs:
path <- paste0("output/shapefiles/", genus, "/", genus, "hurdle_MASK_habitat_raster.tif")
PSA_maks <- raster(path)
plot(PSA_maks)
print(PSA_maks)

#1.2. All area:
# 1.5. Predicted habitat
path <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "hurdle_pred_median.tif")
#path <- paste0("output/shapefiles/", genus, "/", genus, "hurdle_habitat_raster.tif") 
habitat <- raster(path)
#habitat <- calc(habitat, function(x) 10^x)
print(habitat)
plot(habitat)



# 2. Crop habitat to PSAs ------------------------------------------------------
PSA_maks_resampled <- resample(PSA_maks, habitat, method = "bilinear")
cropped_habitat <- mask(habitat, PSA_maks_resampled, inverse = TRUE)
plot(cropped_habitat)

# Make a mask:
cropped_habitat[!is.na(cropped_habitat)] <- 1
plot(cropped_habitat)

#Save it:
output_path <- paste0("output/shapefiles/", genus, "/", genus, "hurdle_MASK_rest_habitat_raster.tif")  # Windows with double backslashes
writeRaster(cropped_habitat, filename = output_path, format = "GTiff", overwrite = TRUE)



# 3. Obtain data from within and outside the PSAs-------------------------------
# Load areas:
path <- paste0("output/shapefiles/", genus, "/", genus, "hurdle_MASK_rest_habitat_raster.tif")
nonPSA <- raster(path)
plot(nonPSA)

path <- paste0("output/shapefiles/", genus, "/", genus, "hurdle_MASK_habitat_raster.tif")
PSA <- raster(path)
plot(PSA)

print(PSA)
nonPSA <- resample(nonPSA, PSA, method = "bilinear")
print(nonPSA)

# Load fishing effort:
fishingEffort <- raster("input/gfwr/summarydata/high_resolution/FishingEffort.tif")
fishingEffort

# Create a new raster with the desired resolution (0.042 degrees)
res <- 0.042
fishingEffort_resampled <- raster(extent(fishingEffort), resolution = res, crs = crs(fishingEffort))

# Resample using bilinear interpolation
fishingEffort <- resample(fishingEffort, fishingEffort_resampled, method = "bilinear")

# 3.1. Crop fishing effort to within and to outside the PSAs- - - - - - - - - -
fishingEffort_cropped <- crop(fishingEffort, extent(PSA))
extent(fishingEffort_cropped) <- extent(PSA)
FE_withinPSA <- mask(fishingEffort_cropped, PSA)
plot(FE_withinPSA)

# 3.2. Crop fishing effort to within and to outside the PSAs- - - - - - - - - - 
fishingEffort_cropped <- crop(fishingEffort, extent(nonPSA))
extent(fishingEffort_cropped) <- extent(nonPSA)
FE_outsidePSA <- mask(fishingEffort_cropped, nonPSA)
plot(FE_outsidePSA)

# 3.3. Transfor to dataframes- - - - - - - - - - - - - - - - - - - - - - - - - - 
print(FE_withinPSA)
print(FE_outsidePSA)
print(fishingEffort)

# Convert to data frame
df_withinPSA <- as.data.frame(FE_withinPSA, xy = TRUE, na.rm = TRUE)  # Include xy coordinates and remove NAs
df_outsidePSA <- as.data.frame(FE_outsidePSA, xy = TRUE, na.rm = TRUE)  # Include xy coordinates and remove NAs
df_FE <- as.data.frame(fishingEffort, xy = TRUE, na.rm = TRUE)  # Include xy coordinates and remove NAs

df_withinPSA$type <- "within"
df_outsidePSA$type <- "outside"
df_outsidePSA$type <- "all"

df_withinPSA$logFE <- log1p(df_withinPSA$FishingEffort)
df_outsidePSA$logFE <- log1p(df_outsidePSA$FishingEffort)
df_FE$logFE <- log1p(df_FE$FishingEffort)


# Remove rows with zero values from df_withinPSA
df_withinPSA <- df_withinPSA %>%
  filter(FishingEffort > 0, logFE > 0)

# Remove rows with zero values from df_outsidePSA
df_outsidePSA <- df_outsidePSA %>%
  filter(FishingEffort > 0, logFE > 0)

# Remove rows with zero values from df_FE
df_FE <- df_FE %>%
  filter(FishingEffort > 0, logFE > 0)


# Print the first few rows of the data frames
print(head(df_withinPSA))
print(head(df_outsidePSA))
print(head(df_FE))
summary(df_withinPSA)
summary(df_outsidePSA)
summary(df_FE)

# 3.4. Violin plot showing fishing effort inside and outside the PSAs - - - - - 
library(smplot2)
#available colors:
#"blue", "crimson", "green", "purple", "orange", "skyblue", "pink", "limegreen", "lightpurple", "brown", "red", "lightbrown", "asparagus", "viridian", "darkred", "lightblue","wine", "yellow", "lightgreen"

p1 <- ggplot(data = df_FE, mapping = aes(x = type, y = logFE, fill = type)) +
  sm_raincloud(data = df_withinPSA, position = position_nudge(x = -1),
               show.legend = FALSE,
               point.params = list(size = 2, shape = 21,
                                   color = 'transparent', 
                                   show.legend = TRUE,
                                   alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = 1.04, seed = 10,
                                                                          jitter.width = 0.06))) +
  sm_raincloud(data = df_outsidePSA, which_side = 'left',
               show.legend = FALSE,
               position = position_nudge(x = 0),
               point.params = list(size = 2, shape = 21,
                                   show.legend = TRUE,
                                   color = 'transparent', alpha = 0,
                                   position = sdamr::position_jitternudge(nudge.x = -0.06, seed = 10,
                                                                          jitter.width = 0.06))) +
  # color groups
  scale_fill_manual(values = sm_color("purple", "yellow")) +
  # y lab
  ylab('FishingEffort') +
  # Positioning Y-axis values on the right side
  scale_y_continuous(position = "right") +
  # theme 
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(angle = 90),
        axis.line.y = element_blank(),   # Remove the Y-axis line
        axis.ticks.y = element_blank(),  # Remove Y-axis ticks
        panel.background = element_blank(),  # Make panel background transparent
        plot.background = element_blank())   # Make plot background transparent
        #panel.grid.major.y = element_blank(),  # Remove horizontal major grid lines
        #panel.grid.minor.y = element_blank())  # Remove horizontal minor grid lines



print(p1)


# export plot
outdir <- paste0(output_data, "/fig/raincloud")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", genus, "_FE_labels.png")
ggsave(p_png, p1, width=17, height=10, units="cm", dpi=300)




# 4. Statistically compare groups ----------------------------------------------
# Shapiro-Wilk normality test
shapiro_test_within <- shapiro.test(df_withinPSA$logFE)
shapiro_test_outside <- shapiro.test(df_outsidePSA$logFE)

# Print results
print(shapiro_test_within)
print(shapiro_test_outside)

# If both groups are normally distributed, use t-test
if (shapiro_test_within$p.value > 0.05 && shapiro_test_outside$p.value > 0.05) {
  t_test_result <- t.test(df_withinPSA$logFE, df_outsidePSA$logFE)
  print(t_test_result)
} else {
  # If not normally distributed, use Mann-Whitney U test
  wilcox_test_result <- wilcox.test(df_withinPSA$logFE, df_outsidePSA$logFE)
  print(wilcox_test_result)
}





# 5. Load MPAs shapefile -------------------------------------------------------
MPA <- st_read("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_20240531_shp/croppedMPAs.shp")
head(MPA)
atributes <- read.csv("input/MPAs/ProtectedSeas/ProtectedSeas_Navigator_Attributes_20240531.csv")

# Merge atributes into MPA based on shared 'site_id' and 'SITE_ID'
print(atributes)
print(MPA)
MPA_merged <- MPA %>%
  left_join(atributes, by = c("SITE_ID" = "site_id"))

# Eliminate nonMPAs
#fishing_mpas <- MPA_merged
fishing_mpas <- MPA_merged %>%
  filter(removal_of_marine_life_is_prohibited == 5) #!=
print(fishing_mpas)
#plot(fishing_mpas)
summary(fishing_mpas)

# Plot the Marine Protected Areas
ggplot(data = fishing_mpas) +
  geom_sf(fill = "skyblue", color = "darkblue") +  # Fill color and border color
  labs(title = "Distribution of Marine Protected Areas",
       subtitle = "Marine MPAs where MARINE == 1",
       caption = "Source: Protected Seas") +
  theme_minimal()  # Use a minimal theme


# 5.1. Calculate % of overlap of PSAs and nonPSA with MPAs :
library(exactextractr)
print(PSA)
st_crs(fishing_mpas)
crs(PSA)

# Reproject both raster and polygons to a common CRS, e.g., UTM
projected_crs <- st_crs(32633)  # Example: UTM Zone 33N, adjust based on your region

# Reproject the polygons
fishing_mpas_projected <- st_transform(fishing_mpas, projected_crs)

# Create a mask for the raster where values are not NA
PSA_masked <- PSA
PSA_masked[is.na(PSA_masked)] <- 0 # Set NA values to 0 or another irrelevant value
unique(values(PSA_masked)) 
plot(PSA_masked)

# Reproject the raster (assuming 'PSA_masked' is the raster)
PSA_masked_projected <- projectRaster(PSA_masked, crs = st_crs(fishing_mpas_projected)$proj4string)


# Calculate the total area of valid raster cells
# Get the total number of valid cells (i.e., where the value is 1, meaning the raster has data)
valid_cells <- sum(values(PSA_masked) == 1)
# Calculate the area of a single raster cell
cell_area <- prod(res(PSA_masked))  # res() gives resolution in x and y
# Total area of valid raster cells
total_valid_area <- valid_cells * cell_area

# Calculate the area of overlap between fishing_mpas polygons and valid raster cells
overlap_areas <- exact_extract(PSA_masked, fishing_mpas, function(values, coverage_fraction) {
  sum(coverage_fraction[values == 1])  # Only consider areas where values are 1 (valid)
})

# Total overlap area in square meters
total_overlap_area <- sum(overlap_areas) * cell_area

# Percentage of overlap
percent_overlap <- (total_overlap_area / total_valid_area) * 100

# Print the result
percent_overlap #2-5: 0.7135192 #1-5: 6.639697

MPA1= 20.92341
MPA2= 0.4037366
MPA3 = 0
MPA4 = 0.194591
MPA5 = 0

# Define the data as segments with different percentage ranges
data <- data.frame(
  segment = c("MPA1", "MPA2", "Remaining"),
  value = c(1.130753, 4.905335 - 1.130753, 100 - 4.905335)  # Calculate differences for segments
)

# Create the bar plot
ggplot(data, aes(x = "", y = value, fill = segment)) + 
  geom_bar(stat = "identity", width = 0.5) +  # Use stat = "identity" for exact values
  scale_fill_manual(values = c("Segment 1" = "blue", "Segment 2" = "orange", "Remaining" = "gray")) +
  coord_flip() +  # Flip the bar to make it horizontal
  labs(
    title = "Bar Plot with Color Breaks",
    x = "",
    y = "Percentage"
  ) +
  scale_y_continuous(limits = c(0, 100)) +  # Ensure the bar maxes out at 100%
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12)) +  # Customize x-axis text size
  theme(axis.text.y = element_text(size = 12))   # Customize y-axis text size

