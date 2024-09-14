# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 3. Abundance Map
#-------------------------------------------------------------------------------
library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)

genus <- "Scyliorhinus" #"Raja" #"Scyliorhinus"

# 1. Set data repository and load rasters---------------------------------------
file <- paste0(temp_data, "/folds_dataset/", genus, "_folds_dataset.csv")
data <- read.csv2(file)

#transform response variable:
#data_Sca$ln_N_km2 <- log1p(data_Sca$N_km2)


# 1.1. Bathymetry
bathy<- raster("input/gebco/Bathy.tif")
#extent(bathy) <- c(-3, 7, 35, 43)
#print(bathy)

# Convert bathy raster to data frame
bathy_df <- as.data.frame(bathy, xy = TRUE)
print(bathy_df)


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


# Ensure CRS matches for all spatial data
st_crs(mask) <- 4326
st_crs(GSA_filtered) <- st_crs(mask)
#st_crs(Bathy_cont1) <- st_crs(mask)



# 2. Crop bathymetric contours to GSA06 ----------------------------------------
# Set the CRS of Bathy_cont1 to match GSA_filtered if needed
#st_crs(Bathy_cont1) <- st_crs(GSA_filtered)
#Bathy_cropped <- st_intersection(Bathy_cont1, GSA_filtered)
#str(Bathy_cropped)


# 3. Colour for bathymetry -----------------------------------------------------
# Create a mask if you dont want to plot all the bathymetricla range
bathy_bb <-  bathy_df$Bathy <= 5 #if you want to put a below limit: bathy_df$Bathy >= -800 &
# Apply the mask
bathy_df <- bathy_df[bathy_bb, ]
print(bathy_df)

#Create colour ramp
#color_palette_bathy <- colorRampPalette(c("lightblue", "white")) 
color_palette_bathy <- colorRampPalette(rev(c('#ecf9ff','#BFEFFF','#97C8EB','#4682B4','#264e76','#162e46')))(100)
cuts_bathy <- cut(bathy_df$Bathy, breaks = 100)
color_indices_bathy <- as.numeric(cuts_bathy) * 100 / length(levels(cuts_bathy))
bathy_df$filling_color <- color_palette_bathy[color_indices_bathy]




# 4. Make map ------------------------------------------------------------------
dataP <- data %>%
  filter(presence_absence != 0)

dataA <- data %>%
  filter(presence_absence == 0)

# Sort data by N_km2
dataP <- dataP[order(-dataP$N_km2), ]




# Create a ggplot object
p <- ggplot() +
  geom_tile(data = bathy_df, aes(x = x, y = y, fill = filling_color)) +
  
  # land mask
  geom_sf(data = mask) +
  
  # Add absences with cross shape and black color
  geom_point(data = dataA, aes(x = lon, y = lat), shape = 4, color = "black", size = 2, alpha = 0.6) +
  
  # add presence points
  geom_jitter(data = dataP, aes(x = lon, y = lat, fill = ifelse(N_km2 == 0, NA, "#FFE4B2"), #"#8D6E63" for skates, "#FFE4B2" for catsharks
                               size = N_km2), shape = 21, color = "black", alpha = 0.6, stroke = 0.7, width = 0.02, height = 0.02) + 
  
  # Plot GSAs
  #geom_sf(data = GSA_filtered, fill = NA, color = "black", size = 0.8, linetype = "dashed") +
  
  #Set spatial bounds
  coord_sf(xlim = c(-1.5, 4.5), ylim = c(37, 42.2), expand = TRUE) +
  
  # Add scale bar
  #annotation_scale(location = "bl", width_hint = 0.2) +  
  
  # theme
  theme_bw() +
  # Directly map colors without scaling
  scale_fill_identity()+
  scale_size(range = c(1.5, 10)) +
  
  # Remove grids
  theme(panel.grid = element_blank(),
      legend.position = "right",
      legend.box = "vertical",
      aspect.ratio = 1) 
 #p

# export plot
outdir <- paste0(output_data, "/fig/Map/density")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", genus, "BIG_density_Map.png")
ggsave(p_png, p, width=20, height=20, units="cm", dpi=1800)

# export plot
#outdir <- paste0(output_data, "/fig/Map/density")
#if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
#p_png <- paste0(outdir, "/", genus, "SCALE_density_Map.png")
#ggsave(p_png, p, width=20, height=20, units="cm", dpi=300)


# Plot enviromental variables --------------------------------------------------
library(viridis)
raster <- raster("input/emodnet/slope/slope.tif")
raster <- raster("input/gfwr/summarydata/high_resolution/FishingEffort.tif")
raster <- raster("input/")
#plot(raster)
raster <- log1p(raster)

# Convert bathy raster to data frame
raster_df <- as.data.frame(raster, xy = TRUE)
head(raster_df)

#Create colour ramp
#color_palette_bathy <- colorRampPalette(c("lightblue", "white")) 
#color_palette_raster <- colorRampPalette(rev(c('#e8f5e9','#a5d6a7','#66bb6a','#43a047','#2e7d32','#1b5e20')))(100)
#cuts_raster <- cut(raster_df$FishingEffort, breaks = 100)
#color_indices_raster <- as.numeric(cuts_raster) * 100 / length(levels(cuts_raster))
#raster_df$filling_color <- color_palette_bathy[color_indices_raster]


# Create a ggplot object
p <- ggplot() +
  geom_tile(data = raster_df, aes(x = x, y = y, fill = layer)) +  # Use the 'layer' for fill
  
  # land mask (if you have it, otherwise remove this line)
  geom_sf(data = mask) +
  
  # Set spatial bounds
  coord_sf(xlim = c(-1.5, 4.5), ylim = c(37, 42.2), expand = TRUE) +
  
  # Add scale bar
  annotation_scale(location = "bl", width_hint = 0.2) +
  
  # Apply viridis color scale for fill
  scale_fill_viridis(name = "Values", option = "D", na.value = "transparent") +  # Viridis palette
  
  # theme
  theme_bw() +
  
  # Customize the plot
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        aspect.ratio = 1)

#p

# export plot
enviro <- "fishingEffort" #slope, fishingEffort, 
outdir <- paste0(output_data, "/fig/Map/enviro")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/", enviro, ".jpeg")
ggsave(p_png, p, width=10, height=10, units="cm", dpi=1800)









# 4. Make zoom out map ------------------------------------------------------
pacman::p_load(dplyr, data.table, rnaturalearth, rnaturalearthdata, 
               ggplot2, raster, terra, tidyr, stringr, gridExtra, 
               plotly, sf, ggshadow, ggforce, giscoR, install = FALSE)

# Custom global ortohraphic proyection from Western-Mediterranean
ortho_crs <-'+proj=ortho +lat_0=20 +lon_0=0.5 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs'

# world coastlines
world_poly <- gisco_get_coastallines(year = "2016", epsg = "4326", resolution = "10")

# global graticule
grid <- st_graticule()

# ocean mask 
ocean <- st_point(x = c(0,0)) |>
  st_buffer(dist = 6371000) |> # Planet earth radius (m)
  st_sfc(crs = ortho_crs)
# plot(ocean)

# Select visible area and project
world <- world_poly |>
  st_intersection(st_transform(ocean, 4326)) |>
  st_transform(crs = ortho_crs) # 
# plot(world)

# delete grid trough continents to create a clean grid
grid_crp <- st_difference(grid, st_union(world_poly))

# select visible area
grid_crp <- st_intersection(grid_crp, st_transform(ocean, 4326)) |>
  st_transform(crs = ortho_crs)
# plot(grid_crp)

# cover globe limit into df - datframe
ocean_df <- st_cast(ocean, "LINESTRING") |> st_coordinates() |> as.data.frame()

# build shadow 
ggplot() + 
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.05,
                shadowsize = 1.5) +
  coord_sf() +
  theme_void()

# add more shadows
g <- ggplot() +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.06,
                shadowsize = 1.5) +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.02,
                shadowsize = 1) +
  geom_glowpath(data = ocean_df, 
                aes(X, Y, group = "L1"),
                shadowcolor='grey90',
                colour = "white",
                alpha = .01,
                shadowalpha=0.01,
                shadowsize = .5)

# adding other layers to the base shadow
g2 <- g +
  
  # Add the ocean mask and fill with white
  geom_sf(data = ocean, fill = "white", color = NA) +
  # add grid
  geom_sf(data = grid_crp, 
          colour = "grey85", 
          linewidth = .15) +
  # add sea-turtle SSM points
  #geom_sf(data = ssm_sf, size = 0.15, color = "deepskyblue3") +
  # add 3D globe land
  geom_sf(data = world, 
          #fill = "#FFE4B2",
          colour = "grey35",
          linewidth = .2) +
  # theme
  theme_void()

print(g2)

# export plot
outdir <- paste0(output_data, "/fig/Map")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
p_png <- paste0(outdir, "/_global_Map.png")
ggsave(p_png, g2, width=17, height=17, units="cm", dpi=300)




