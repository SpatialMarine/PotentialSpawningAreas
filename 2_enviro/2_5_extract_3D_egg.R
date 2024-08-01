# ------------------------------------------------------------------------------

# Title: 

#-------------------------------------------------------------------------------
# 2.5. Extract 3D data from raster to points 
#-------------------------------------------------------------------------------
library(dplyr)

#Load data
data <- read.csv("temp/env_data2D.csv", sep = ";") #remember having date format in your .csv
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
range(data$date_time)

# Add a new column with the year information
data <- data %>%
  mutate(Year = format(date, "%Y"),
         Month = format(date, "%m"),
         Day = format(date, "%d"))
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
repo <- paste0(input_data, "/cmems") 

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
write.csv(data, "temp/data_2D_3D_dist.csv", row.names = FALSE)
