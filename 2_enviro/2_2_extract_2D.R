# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 2.2. Extract 2D data from raster to points 
#-------------------------------------------------------------------------------
library(raster)
library(ncdf4)

data <- read.csv2("temp/pres_absData.csv", sep = ";")

# add mins and secs:
# initialize an empty vector to store the new values
new_days <- vector()
# loop through each value in the dataframe
for (j in 1:nrow(data)) {
  # Concatenate the date with the time string
  new_value <- paste0(data$date[j], " 11:00:00")
  # Append the new value to the vector
  new_days <- c(new_days, new_value)
}
# Convert to POSIXct without the timezone
new_days_posix <- as.POSIXct(new_days, format="%Y-%m-%d %H:%M:%S", tz="UTC")
# Convert back to character strings without timezone information (format required for cm$subset function)
data$date_time <- format(new_days_posix, format="%Y-%m-%d %H:%M:%S")
head(data)

# use same temporal resolution (day) and numeric for lon and lat
data$date <- as.Date(data$date) #if your time scale has not hours
data$lon <- as.numeric(data$lon)
data$lat <- as.numeric(data$lat)
range(data$date)
range(data$lon)
range(data$lat)

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

# 2) Extract data from dynamic 2D variables-------------------------------------
catalog <- read.csv2("input/Catalog_CMEMS.csv", sep=";")
head(catalog)
cat <- catalog %>%
  filter(var_name %in% c("SBT_Reanalysis"))


# The only 2D variable is SBT (med-cmcc-tem-rean-d)
# make a loop to (1) open each file ".nc" (2) configure time format and (3) extract data

# Repository to folder where netCDFs are:
repo <- paste0(input_data, "/cmems") 

# Iterate over each productid in 'cat' dataframe
for (pid in unique(cat$id_product)) {
  # Filter data corresponding to current productid
  subset_data <- subset(cat, id_product == pid)

data <- cmems2d(lon=data$lon, lat=data$lat, date=data$date, productid=pid, repo=repo, data=data)
# Print or save any necessary output or results
print(paste("Processed productid:", pid))
}
head(data)

# Save dataframe
write.csv2(data, "temp/env_data.csv", row.names = FALSE)
