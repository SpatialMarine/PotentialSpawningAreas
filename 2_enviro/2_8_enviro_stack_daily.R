# ------------------------------------------------------------------------------

# Title:


# This script generates a daily multiband raster to then make model predictions.

#-------------------------------------------------------------------------------
# 2.8. Stack environmental data for predict
#-------------------------------------------------------------------------------
library(sf)
library(doParallel)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
library(ncdf4)
library(stringr)

# 1. Set static data repository -------------------------------------------------------
# path to environmental static data
static_data <- paste0(input_data, "/summary_static/")

# path to output
outdir <- paste0(temp_data, "/stack_daily/") 
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)






# 2. Create oceanmask-----------------------------------------------------------
# Set raster resolution and extent
res <- 0.042
e <- extent(-6, 30, 40, 46)

# create base raster
m <- raster(e, res = res, crs = crs("+proj=longlat +datum=WGS84"))
m[] <- 1





# 3. Stack static environmental data-------------------------------------------
# import static maps
bathy <- raster(paste0(static_data, "/bathy.tif"))  # bathymetry
bathy <- bathy+0

slope <- raster(paste0(static_data, "/slope.tif"))  # slope
slope <- slope+0

distCanyons <- raster(paste0(static_data, "/distance_canyons.tif"))  # distance to shore
distCanyons <- distCanyons+0

distanceFans <- raster(paste0(static_data, "/distance_fans.tif"))  # distance to shore
distanceFans <- distanceFans+0

distanceMounts <- raster(paste0(static_data, "/distance_mounts.tif"))  # distance to shore
distanceMounts <- distanceMounts+0

FishingEffort <- raster(paste0(static_data, "/FishingEffort.tif"))  # distance to shore
FishingEffort <- FishingEffort+0

roughness <- raster(paste0(static_data, "/roughness.tif"))  # distance to shore
roughness <- roughness+0

# prepare function for stack
prepareGrid <- function(r, m, method, name){
  rc <- raster::crop(r, m)  # crop by extent
  rs <- raster::resample(r, m, method=method)  # resample
  rm <- raster::mask(rs, m)  # mask
  names(rm) <- name
  return(rm)
}

# create stack with static variables
bat <- prepareGrid(bathy, m, method="bilinear", name="bathy")
slp <- prepareGrid(slope, m, method="bilinear", name="slope")
distCans <- prepareGrid(distCanyons, m, method="bilinear", name="distCanyons")
distFans <- prepareGrid(distanceFans, m, method="bilinear", name="distanceFans")
distMounts <- prepareGrid(distanceMounts, m, method="bilinear", name="distanceMounts")
FishingEff <- prepareGrid(FishingEffort, m, method="bilinear", name="FishingEffort")
rough <- prepareGrid(roughness, m, method="bilinear", name="roughness")

stack_static <- stack(bat, slp, distCans, distFans, distMounts, FishingEff, rough)








# 3. Convert 3D (lat, lon, depth) variables in 2D (lat, lon)--------------------
# by selecting the deepest data

# Example usage
folder_path <- "input/cmems_predict/2021"
base_dirs <- list.dirs(path = folder_path, full.names = TRUE, recursive = FALSE)
output_dir <- "input/cmems_predict_3d/2021"

# Loop through each subfolder and apply the function
for (base_dir in base_dirs) {
  # Create an output directory corresponding to the input subfolder
  output_dir <- file.path(output_dir, basename(base_dir))
  
  # Apply the conversion function
  convert_4d_to_3d_daily(base_dir, output_dir)
}









# 4. Prepare dynamic variables for stack --------------------------------------------------

# Function to prepare and stack raster files for each day

prepareStackForDay <- function(day_folder, variables, res, e, output_folder) {
  
  # Define extent and resolution
  e <- extent(e)
  
  # Create an empty stack
  stack_dynamic <- stack()
  
  for (variable in variables) {
    
    # Construct the file pattern for the variable
    file_pattern <- paste0("*_", variable, ".nc")
    
    # List netCDF files for the given variable
    nc_files <- list.files(path = day_folder, pattern = file_pattern, full.names = TRUE)
    
    if (length(nc_files) == 0) {
      next
    }
    
    # Read each netCDF file and prepare the raster
    for (nc_file in nc_files) {
      
      # Open the netCDF file
      nc <- nc_open(nc_file)
      
      # Get dimensions
      lon <- ncvar_get(nc, "longitude")
      lat <- ncvar_get(nc, "latitude")
      time <- ncvar_get(nc, "time")

      
      # Print dimensions for debugging
      cat("Dimensions for", variable, "from", nc_file, ":\n")
      cat("Longitude:", length(lon), "\n")
      cat("Latitude:", length(lat), "\n")
      cat("Time:", length(time), "\n")
      
      
      # Extract the data for the first time step (assuming you want the first slice)
      data <- ncvar_get(nc, "bottomT", start = c(1, 1, 1), count = c(-1, -1, 1))
      
      # Convert the data matrix to a 2D array
      data_matrix <- t(data[,,1])  # Assuming `data` is ordered as (lon, lat, time)
      
      # Create a raster from the 2D data matrix
      r <- raster(nrows = nrow(data_matrix), ncols = ncol(data_matrix),
                  xmn = min(lon), xmx = max(lon),
                  ymn = min(lat), ymx = max(lat))
      
      # Set values for the raster
      r <- setValues(r, as.vector(data_matrix))
      
      # Set projection if not already set
      projection(r) <- "+proj=longlat +datum=WGS84"
      
      # Crop and resample the raster
      r <- crop(r, e)
      r <- resample(r, raster(extent = e, resolution = res), method = "bilinear")
      
      # Stack the raster
      stack_dynamic <- stack(stack_dynamic, r)
      
      # Close the netCDF file
      nc_close(nc)
    }
  }
  
  # Save the stack to file
  if (nlayers(stack_dynamic) > 0) {
    output_file <- file.path(output_folder, paste0("stack_", format(Sys.Date(), "%Y%m%d"), ".grd"))
    writeRaster(stack_dynamic, output_file, format = "raster", overwrite = TRUE)
  }
}





# prepare the function to process each dayly stack:
processDailyStacks <- function(base_folder, variables, res, e) {
  
  # Get list of all month folders
  month_folders <- list.dirs(base_folder, full.names = TRUE, recursive = FALSE)
  
  for (month_folder in month_folders) {
    
    # Get list of all day folders within the current month folder
    day_folders <- list.dirs(month_folder, full.names = TRUE, recursive = FALSE)
    
    for (day_folder in day_folders) {
      
      # Extract date from folder name
      date_folder <- basename(day_folder)
      
      # Define output folder for the stack
      output_folder <- day_folder
      if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
      }
      
      # Prepare and stack rasters
      prepareStackForDay(day_folder, variables, res, e, output_folder)
    }
  }
}




# Stack you documents:
# General path:
base_folder <- "input/cmems_predict/2021"

# Select the dynamic variables to extract (same names as catalog):
catalog <- read.csv2("input/Catalog_CMEMS.csv", sep=";")
catalog$variable
variables <- c("bottomT", "o2", "nppv", "ph", "nh4", "no3", "po4", "so", "uo", "vo")

# Set the resolution and extent:
res <- 0.042
e <- extent(-6, 30, 40, 46) 

# Process the stacks
processDailyStacks(base_folder, variables, res, e)

































# Create a dates dataframe:
# Set period
date_start <- as.Date("2021-01-01")
date_end <- as.Date("2021-12-31")

# Create dates
dates <- data.frame(date = seq.Date(date_start, date_end, by = "day"))
dates$date <- as.Date(dates$date)

# Add a new column with the year, month and day information
dates <- dates %>%
  mutate(Year = format(date, "%Y"),
         Month = format(date, "%m"),
         Day = format(date, "%d"))
head(dates)






# prepare function for stack:
prepareGridCatalogue <- function(var, date, catalog, m, method, name, input_data){
  # Load required libraries
  library(stringr)
  library(lubridate)
  library(raster)  # Ensure raster functions are available
  
  # Create an empty map with NA in case no file is found
  empty_m <- m
  empty_m[!is.na(empty_m)] <- NA
  
  # Locate product by variable name in the catalog
  c <- catalog[catalog$variable == var, ]
  
  # Handle case where the catalog is empty
  if (nrow(c) == 0) {
    warning(paste("No matching variable found in the catalog for variable:", var))
    return(empty_m)
  }
  
  # Prepare date information
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Locate folder path for the specific date
  product_folder <- file.path(input_data, "cmems_predict", YYYY, MM, DD)
  
  # List files matching the variable pattern in the folder
  folder_files <- list.files(product_folder, full.names = TRUE, pattern = var)
  
  # Check that there is exactly one file to retrieve
  if (length(folder_files) == 1){
    # Import the file
    r <- tryCatch({
      raster(folder_files[1], var = var)  # Open the NetCDF file using the correct variable
    }, error = function(e) {
      warning(paste("Error reading raster for", var, "on", date, ":", e))
      return(empty_m)
    })
    
    # Resample at a common grid
    pg <- prepareGrid(r, m, method = method, name = name)
  } else {
    # If no file or multiple files found, return the empty map
    if(length(folder_files) == 0){
      warning(paste("No file found for variable", var, "on", date))
    } else {
      warning(paste("Multiple files found for variable", var, "on", date, "- using the first one."))
    }
    pg <- empty_m
    names(pg) <- name
  }
  
  return(pg)
}



# Load necessary packages
library(foreach)
library(doParallel)
library(lubridate)
library(raster)
library(stringr)
library(dplyr)



# Register parallel backend
numCores <- detectCores() - 1  # Use one less than the number of cores
cl <- makeCluster(numCores)
registerDoParallel(cl)

# Progress tracking (optional)
progress <- txtProgressBar(min = 1, max = nrow(dates), style = 3)

# Process data in parallel
foreach(i = 1:nrow(dates), .packages = c("lubridate", "raster", "stringr", "dplyr"), .combine='c') %dopar% {
  # Set day i
  date <- dates$date[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  print(date)
  
  # Create empty stack
  stack_dynamic <- stack()
  
  # Loop for each dynamic variable
  for (j in 1:length(env_dyn_vars)){
    jvar <- prepareGridCatalogue(var = env_dyn_vars[j], catalog = catalog, name = env_dyn_vars[j], date = date, m = m, method = "bilinear", input_data = input_data)
    stack_dynamic <- stack(stack_dynamic, jvar)
  }
  
  # Combine with static stack
  s <- stack(stack_static, stack_dynamic)
  s <- setZ(s, rep(date, nlayers(s)))
  
  # Set/create folder
  product_folder <- file.path(outdir, YYYY, MM, DD)  # Set folder to include day
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # Create output directory if it does not exist
  
  # Store file in GRD format
  outfile <- file.path(product_folder, paste0(format(date, "%Y%m%d"), "_enviro.grd"))
  writeRaster(s, outfile, bandorder = 'BIL', overwrite = TRUE)
  
  # Progress update (optional)
  setTxtProgressBar(progress, i)
}

# Stop the cluster
stopCluster(cl)
close(progress)

print("Daily stack ready")




