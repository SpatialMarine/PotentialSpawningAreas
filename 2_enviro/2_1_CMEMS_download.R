# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 2.1. Download environmental data from CMEMS
#-------------------------------------------------------------------------------
library(reticulate)
library(lubridate)
library(dplyr)
library(beepr)

# Import data catalog
catalog <- read.csv2("input/Catalog_CMEMS.csv", sep=";")
head(catalog)

catalog <- catalog %>%
  mutate(
    xmin = as.numeric(gsub(",", ".", xmin)),
    xmax = as.numeric(gsub(",", ".", xmax)),
    ymin = as.numeric(gsub(",", ".", ymin)),
    ymax = as.numeric(gsub(",", ".", ymax)),
    depth_min = as.numeric(gsub(",", ".", depth_min)),
    depth_max = as.numeric(gsub(",", ".", depth_max)))


# 1) Prepare data required for requesting download to CMEMS -------------------
# Import our data to extract our dates
data <- read.csv2("temp/pres_absData.csv", sep = ";")

# extract the dates in which there is data:
data$date <- as.Date(data$date)
Days <- unique(data$date)
Days_df <- data.frame(Days)
Days_df$Days <- as.Date(Days_df$Days)

# Add a new column with the year information
Days_df <- Days_df %>%
  mutate(Year = format(Days, "%Y"),
         Month = format(Days, "%m"),
         Day = format(Days, "%d"))
head(Days_df)

# add mins and secs:
# initialize an empty vector to store the new values
new_days <- vector()
# loop through each value in the dataframe
for (j in 1:nrow(Days_df)) {
  # Concatenate the date with the time string
  new_value <- paste0(Days_df$Days[j], " 11:00:00")
  # Append the new value to the vector
  new_days <- c(new_days, new_value)
}
# Convert to POSIXct without the timezone
new_days_posix <- as.POSIXct(new_days, format="%Y-%m-%d %H:%M:%S", tz="UTC")
# Convert back to character strings without timezone information (format required for cm$subset function)
Days_df$Days_with_time <- format(new_days_posix, format="%Y-%m-%d %H:%M:%S")
head(Days_df)

# 2) Load CMEMS package through python (currently CMEMS data can only be accessed this way) -------------------------
# install python 
install_python() 
virtualenv_create(envname = "cmems")
virtualenv_install("cmems", packages = c("copernicusmarine"))
use_virtualenv("cmems", required = TRUE)
py_install("copernicusmarine")

# load package / import library (py)
cm <- import("copernicusmarine")

# log in in your CMEMS user (you should have another script with this info)
cm$login(username, password)
y
# 3) Download data -------------------------------------------------------------
# Define the time subset you want:
df <- Days_df 

# Define the catalog subset you want:
cat <- catalog  

# Define the name of the file and the destination
destination_folder <- paste0(input_data, "/cmems")
if (!dir.exists(destination_folder)) dir.create(destination_folder, recursive = TRUE)

t <- Sys.time()
for(i in 1:nrow(cat)){ 
  
  # Calculate remaining products
  remaining_products <- nrow(cat) - i
  
  # Print the current product and remaining products
  print(paste("Processing product", i, "of", nrow(cat), "-", remaining_products, "remaining"))
  
  # Create the folder for each product if it doesn't exist already 
  dir_path <- file.path(destination_folder, cat$service[i], cat$layer[i])
  if (!file.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)}
  
  #If you need a folder per each date:
  for(j in 1:nrow(df)){
    # Calculate remaining dates
    remaining_dates <- nrow(df) - j
    
    # Print the current date and remaining dates
    print(paste("Processing date", j, "of", nrow(df), "-", remaining_dates, "remaining"))
    
    # Create folders for different dates inside the variable folders
    date_dir <- file.path(dir_path, df$Year[j], df$Month[j], df$Day[j])
    if (!file.exists(date_dir)) {
      dir.create(date_dir, recursive = TRUE)}
    
    # Define the file name using the current date
    file_name <- paste0(cat$var_name[i], "_", df$Days[j], ".nc")
    
    # download data
    cm$subset(
      dataset_id = cat$layer[i],
      start_datetime = df$Days_with_time[j], #format example "1994-05-16 12:00:00"
      end_datetime = df$Days_with_time[j],
      variables = list(cat$variable[i]), # attention - variable must be a list
      minimum_longitude = cat$xmin[i],
      maximum_longitude =  cat$xmax[i],
      minimum_latitude =  cat$ymin[i],
      maximum_latitude = cat$ymax[i],
      minimum_depth = cat$depth_min[i],
      maximum_depth = cat$depth_max[i],
      output_filename = file_name,
      output_directory = date_dir,
      force_download = TRUE)
  }
}
Sys.time() - t 
beep()


# Check it all with an example: 
nc<- nc_open("input/cmems/MEDSEA_MULTIYEAR_PHY_006_004/med-cmcc-tem-rean-d/2020/12/02/SBT_Reanalysis_2020-12-02.nc")
nclon <- nc$dim$lon$vals#ncvar_get(nc, varid="lon") # nc$dim$lon$vals => same or faster?
nclat <- nc$dim$lat$vals#ncvar_get(nc, varid="lat") 

sbt_reanalysis <- brick("input/cmems/MEDSEA_MULTIYEAR_PHY_006_004/med-cmcc-tem-rean-d/2020/12/02/SBT_Reanalysis_2020-12-02.nc")
time <- getZ(sbt_reanalysis)
time_seconds <- time * 60  # Convert minutes to seconds
days <- as.POSIXct(time_seconds, origin = "1900-01-01", tz = "UTC")
