# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 2.2. Extract 2D data from raster to points 
#-------------------------------------------------------------------------------
library(raster)
library(ncdf4)

data <- read.csv2("temp/pres_absData.csv", sep = ";")

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
View(data)

# 2) Generate function to extract data  ------------------------------------------------------------

# set extract function
extractTSR <- function(x, y, t){
  
  # get time from raster
  xtime <- raster::getZ(x)
  
  # match point time with raster
  # returns index from multilayer
  idx <- match(t, xtime)
  
  # extract data for all points from all layers
  ex <- raster::extract(x, y)
  
  # for each data point, select the data for idx
  dat <- ex[cbind(1:length(t), idx)]
  return(dat)
}

# 3) Extract data from dynamic 2D variables-------------------------------------
catalog <- read.csv2("input/Catalog_CMEMS.csv", sep=";")
head(catalog)
cat <- catalog %>%
  filter(var_name %in% c("SBT_Reanalysis"))

# The only 2D variable is SBT (med-cmcc-tem-rean-d)
# make a loop to (1) open each file ".nc" (2) configure time format and (3) extract data

# Initialize an empty list to store extracted data
extracted_data_list <- list()

# Loop through each layer in the 'cat' data frame
for (i in 1:nrow(cat)) {
  # Set the directory path for the current layer
  nc_directory <- file.path("input/cmems", cat$service[i], cat$layer[i], cat$var_name[i])
  
  # Get a list of all .nc files in the directory and subdirectories
  nc_files <- list.files(nc_directory, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)
  
  # Initialize a list to store the extracted data
  extracted_data_list <- vector("list", length = nrow(data))
  
  # Loop through each file and process
  for (j in seq_along(nc_files)) {
    
    # Open the NetCDF file as a raster brick
    sbt_reanalysis <- brick(nc_files[j])
    
    # Configure the time format (assuming time is in minutes since 1900-01-01)
    time <- getZ(sbt_reanalysis)
    time_seconds <- time * 60  # Convert minutes to seconds
    days <- as.POSIXct(time_seconds, origin = "1900-01-01", tz = "UTC")
    
    # Extract data using the provided function
    extracted_data <- extractTSR(x = sbt_reanalysis, y = cbind(data$lon, data$lat), t = data$time)
    
    # Store extracted data along with corresponding dates
    extracted_data_list[[j]] <- data.frame(date = days, Value = extracted_data)
    
    # Print the progress
    print(paste("Processed file:", nc_files[j], "for", cat$layer[i]))
    
    # Close the file (raster package manages this automatically, but you can ensure resources are freed)
    rm(sbt_reanalysis)
    gc()  # Garbage collection to free up memory
  }
  
  # Combine extracted data from all files into a single data frame
  combined_data <- do.call(rbind, extracted_data_list)
  
  # Merge with the main dataset 'data' based on dates
  data <- merge(data, combined_data, by = "date", all.x = TRUE)
  
  # Rename the merged column appropriately
  colnames(data)[ncol(data)] <- cat$var_name[i]
}

# Save dataframe
write.csv2(data, "temp/env_data.csv", row.names = FALSE)
