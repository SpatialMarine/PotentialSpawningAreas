# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 2.4. Extract 2D data from raster to points 
#-------------------------------------------------------------------------------
library(raster)
library(ncdf4)
library(dplyr)

data <- read.csv2("temp/pres_absData.csv", sep = ";")

# add mins and secs:
# Note: 11:00:00 if you use 12:00:00 CMEMS use the next day
data$date_time <- paste0(data$date, " 11:00:00")


# use same temporal resolution (day) and numeric for lon and lat
data$date <- as.Date(data$date) #if your time scale has not hours
data$lon <- as.numeric(data$lon)
data$lat <- as.numeric(data$lat)

range(data$date)
range(data$lon)
range(data$lat)
range(data$date_time)

# 1) Load and extract data from all static rasters -----------------------------

# 1.1) Bathymetry (depth) 
bathy <- raster("input/gebco/Bathy.tif")
bathy

data$bathy <- raster::extract(bathy, cbind(data$lon, data$lat)) 
data$bathy <- abs(data$bathy)
head(data)

# 1.2) Substrate 
subs <- raster("input/emodnet/substrate_folk5/substrate_folk5.tif")
subs

data$subs <- raster::extract(subs, cbind(data$lon, data$lat)) 
head(data)

# 1.3) Slope 
# Calculate terrain characteristic from bathymetry

# Only run once:
#slope <- terrain(bathy, opt=c("slope"), unit='degrees')
#writeRaster(slope, filename="input/emodnet/slope/slope.tif", overwrite=TRUE)  # save binary file for slope

slope <- raster("input/emodnet/slope/slope.tif")
slope

data$slope <- raster::extract(slope, cbind(data$lon, data$lat)) 
head(data)

# 1.4) Roughness 
# Difference between the maximum and the minimum value of a cell and its 8 surrounding cells

roughness <- raster("input/gebco/roughness/roughness.tif")
roughness

data$roughness <- raster::extract(roughness, cbind(data$lon, data$lat)) 
head(data)

# 1.5) Fishing effort
fishingEffort <- raster("input/gfwr/summarydata/high_resolution/FishingEffort.tif")
fishingEffort

data$fishingEffort <- raster::extract(fishingEffort, cbind(data$lon, data$lat)) 
data$fishingEffort <- abs(data$fishingEffort)
head(data)

# 1.6) Distance to canyons
distCanyons <- raster("input/distance_to/distance_canyons.tif")
distCanyons

data$distCanyons <- raster::extract(distCanyons, cbind(data$lon, data$lat)) 
data$distCanyons <- abs(data$distCanyons)
head(data)

# 1.7) Distance to mounts
distMounts <- raster("input/distance_to/distance_mounts.tif")
distMounts

data$distMounts <- raster::extract(distMounts, cbind(data$lon, data$lat)) 
data$distMounts <- abs(data$distMounts)
head(data)

# 1.8) Distance to fans
distFans <- raster("input/distance_to/distance_fans.tif")
distFans

data$distFans <- raster::extract(distFans, cbind(data$lon, data$lat)) 
data$distFans <- abs(data$distFans)
head(data)

# Save dataframe
write.csv2(data, "temp/env_data.csv", row.names = FALSE)
data <- read.csv2("temp/env_data.csv", sep = ";")


# 2) Extract data from dynamic 2D variables-------------------------------------
catalog <- read.csv2("input/Catalog_CMEMS.csv", sep=";")
head(catalog)
cat <- catalog %>%
  filter(var_name %in% "SBT_Reanalysis")


# The only 2D variable is SBT (med-cmcc-tem-rean-d)
# make a loop to (1) open each file ".nc" (2) configure time format and (3) extract data

# Repository to folder where netCDFs are:
repo <- paste0(input_data, "/cmems") 

# Iterate over each productid in 'cat' dataframe
for (pid in unique(cat$id_product)) {
  # Filter data corresponding to current productid
  # example for checking code:  id_product <- 1
  subset_data <- subset(cat, id_product == pid)

data <- cmems2d(lon=data$lon, lat=data$lat, date=data$date, productid=pid, repo=repo, data=data)
# Print or save any necessary output or results
print(paste("Processed productid:", pid))
}
head(data)

# Save dataframe
write.csv2(data, "temp/env_data2D.csv", row.names = FALSE)
