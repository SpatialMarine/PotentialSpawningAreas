# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 2.7. Download environmental data from CMEMS for predict
#-------------------------------------------------------------------------------
library(reticulate)
library(lubridate)
library(dplyr)
library(beepr)

# Import data catalog
catalog <- read.csv2("input/Catalog_CMEMS.csv", sep=";")
str(catalog) #ensure numerical variables are numeric

catalog <- catalog %>%
  mutate(
    xmin = as.numeric(gsub(",", ".", xmin)),
    xmax = as.numeric(gsub(",", ".", xmax)),
    ymin = as.numeric(gsub(",", ".", ymin)),
    ymax = as.numeric(gsub(",", ".", ymax)),
    depth_min = as.numeric(gsub(",", ".", depth_min)),
    depth_max = as.numeric(gsub(",", ".", depth_max)))




# 1. Create a dates dataframe for the dates you want to get data from ----------
# Create a sequence of dates from January 1, 2021, to December 31, 2021
dates <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day")

# Convert the dates to the desired format with the time "11:00:00"
date_times <- paste(dates, "11:00:00")

# Create a dataframe
dates <- data.frame(date = dates, date_time = date_times)
dates$date <- as.Date(dates$date)

# Add a new column with the year, month and day information
dates <- dates %>%
  mutate(Year = format(date, "%Y"),
         Month = format(date, "%m"),
         Day = format(date, "%d"))
head(dates)





# 2) Load CMEMS package through python -----------------------------------------
# install python 
#install_python() 
#virtualenv_create(envname = "cmems")
#virtualenv_install("cmems", packages = c("copernicusmarine"))
use_virtualenv("cmems", required = TRUE)

# load package / import library (py)
cm <- import("copernicusmarine")

# log in in your CMEMS user (you should have another script with this info)
cm$login(username, password)
# for session yes == y
y




# 3) Download data -------------------------------------------------------------
# Define the time subset you want:
df <- dates 

# Define the catalog subset you want:
cat <- catalog
#cat <- catalog %>%
#  filter(variable %in% c("uo", "vo"), product_type %in% c("Reanalysis")) 


# Define the name of the file and the destination
destination_folder <- paste0(input_data, "/cmems_predict")
if (!dir.exists(destination_folder)) dir.create(destination_folder, recursive = TRUE)

t <- Sys.time()
for(i in 1:nrow(cat)){ 
  
  # Calculate remaining products
  remaining_products <- nrow(cat) - i
  
  # Create the folder for each product if it doesn't exist already 
  dir_path <- file.path(destination_folder, dates$Year[j], dates$Month[j], dates$Day[j])
  if (!file.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)}
  
  #If you need a folder per each date:
  for(j in 1:nrow(df)){
    # Calculate remaining dates
    remaining_dates <- nrow(df) - j
    
    # Print the current product and remaining products
    print(paste("Processing product", i, "of", nrow(cat), "-", remaining_products, "remaining"))
    # Print the current date and remaining dates
    print(paste("Processing date", j, "of", nrow(df), "-", remaining_dates, "remaining"))
    
    # Define the file name using the current date
    file_name <- paste0(format(as.Date(dates$date[j], origin = "1970-01-01"), "%Y%m%d"),"_", catalog$variable[i], ".nc")
    
    # download data
    cm$subset(
      dataset_id = cat$layer[i],
      start_datetime = df$date_time[j], #format example "1994-05-16 12:00:00"
      end_datetime = df$date_time[j],
      variables = list(cat$variable[i]), # attention - variable must be a list
      minimum_longitude = cat$xmin[i],
      maximum_longitude =  cat$xmax[i],
      minimum_latitude =  cat$ymin[i],
      maximum_latitude = cat$ymax[i],
      minimum_depth = cat$depth_min[i],
      maximum_depth = cat$depth_max[i],
      output_filename = file_name,
      output_directory = dir_path,
      force_download = TRUE)
  }
}
Sys.time() - t 
beep()


