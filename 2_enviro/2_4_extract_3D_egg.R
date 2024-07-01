# ------------------------------------------------------------------------------

# Title: 

#-------------------------------------------------------------------------------
# 2.4. Extract 3D data from raster to points 
#-------------------------------------------------------------------------------
library(dplyr)

#Load data
data <- read.csv("temp/data_2D_dist.csv", sep = ";") #remember having date format in your .csv
summary(data)
head(data)

# explore temporal and spatial range
# use same temporal resolution (day) and numeric for lon and lat
data$date <- as.Date(data$date) #if your time scale has not hours
data$lon <- as.numeric(gsub(",", ".", data$lon))
data$lat <- as.numeric(gsub(",", ".", data$lat))
data$depth <- as.numeric(gsub(",", ".", data$depth))
range(data$date)
range(data$lon)
range(data$lat)
range(data$depth)

# Add a new column with the year information
data <- data %>%
  mutate(Year = format(date, "%Y"),
         Month = format(date, "%m"),
         Day = format(date, "%d"))
head(data)

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

#open catalog
catalog <- read.csv("input/Catalog_CMEMS.csv", sep=";")
cat <- catalog %>%
  filter(dimensions %in% c("3D"))
head(cat)

#--------------------------------------------------------------------------------------
# Extract your own data
#--------------------------------------------------------------------------------------
# Repository to folder where netCDFs are:
repo <- paste0(input_data, "/cmems") #You will have to reorganise your netCDFs within subfolders named as: "product_1" and "product_2" as in catalog.

# Iterate over each productid in 'cat' dataframe
for (pid in unique(cat$id_product)) {
  # Filter data corresponding to current productid
  subset_data <- subset(cat, id_product == pid)
  
  # Apply cmems3d_bottom function to subset of data
  data <- cmems3d_bottom(lon=data$lon, lat=data$lat, date=data$date, productid=pid, repo=repo, data=data)
  
  # Optionally, you can assign the modified 'data' back to your original dataframe or save it somewhere
  # For example, if you want to update 'data' in place, you can do:
  # data <- data
  
  # Print or save any necessary output or results
  print(paste("Processed productid:", pid))
}
head(data)

# Save dataframe
write.csv2(data, "temp/data_2D_3D_dist.csv", row.names = FALSE)
