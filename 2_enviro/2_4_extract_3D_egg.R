# ------------------------------------------------------------------------------

# Title: 

#-------------------------------------------------------------------------------
# 2.4. Extract 3D data from raster to points 
#-------------------------------------------------------------------------------

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

#open catalog
catalog <- read.csv("input/Catalog_CMEMS.csv", sep=";")
cat <- catalog %>%
  filter(dimensions %in% c("3D"))
head(cat)

#productid=2
#repo <- paste0(input_data, "/cmems")

# create new extract function
#--------------------------------------------------------------------------------------
# cmems3dmat       Extract vertical profiles into matrix from 3D ROMS numerical models netcdfs along the path
#--------------------------------------------------------------------------------------
#adapted from dmarch github: https://github.com/dmarch/ocean3d/blob/8b525bd1b13bea93f608e89f40ae9a561ca49e64/R/cmems2track_v2.R#L138

cmems3dmat <- function(lon, lat, date, productid, repo, maxZ){
  # Description
  # Extracts oceanographic information from 3D numerical models downloaded from CMEMS
  
  # Arguments
  # lon         longitude
  # lat         latitude
  # date        POSIXct date time or Date
  # depth       depth value, in meters (positive)
  # productid   id of the product from catalog table. This is used to find the netcdf file from the repository (repo)
  # repo        repository path with netcdf files. it follows the same structure as the CMEMS FTP server
  # maxZ        optional. Maximum Z level to extract the data from. ROMS has 35 levels. (level 23=300m)
  
  # Value
  # A data frame with: varname, var0(optional), zmax(optional)
  
  # Description
  # Extraction of values is done using the nearest neighbor 3d point.
  
  # Note
  # This function currently uses a loop, which could be slower than other approaches.
  
  
  # Load libraries
  library(lubridate)
  library(ncdf4)
  library(dplyr)
  
  # Get information and variable name for a given product
  product_info <- filter(cat, id_product == productid)
  var <- as.character(product_info$variable)
  
  # get all product files
  # product_files <- list.files(paste(repo, product_info$product1, product_info$product2, sep="/"), full.names=TRUE, recursive=TRUE)
  product_files <- list.files(paste(repo, product_info$service, product_info$layer, cat$var_name, sep="/"), full.names=TRUE, recursive=TRUE) #data$Year[j], data$Month[j], data$Day[j],
  
  # select first file and get dimensions
  ncfile <- product_files[1]
  nc <- nc_open(ncfile)
  nclon <- nc$dim$lon$vals#ncvar_get(nc, varid="lon") # nc$dim$lon$vals => same or faster?
  nclat <- nc$dim$lat$vals#ncvar_get(nc, varid="lat") 
  ncdepth <- nc$dim$depth$vals
  nctime <- nc$dim$time$vals
  ncday <- as.POSIXct(nctime, origin = "1970-01-01", tz = "UTC") 
  #you will have to adecuate this to transform properly the date
  #reference_time <- as.POSIXct("1900-01-01", tz = "UTC")
  #ncday <- reference_time + as.difftime(nctime, units = "mins")
  maxZ <- nc$dim$depth$len 
  nc_close(nc)
  
  # output matrix
  #out <- matrix(data=NA, nrow=length(lon), ncol=length(var))#, dimnames=as.list(var))
  
  out <- matrix(data = NA, nrow = maxZ, ncol = length(date), byrow = FALSE,  
                dimnames = list(ncdepth[1:maxZ], date)) #dinames = NULL or dimnames = list(ncdepth, data$date)
  
  # get data for each observation
  for (i in 1:length(date)){
    print(i)
    
    ## get day, lon, lat, depth
    iday <- as.POSIXct(data$time_hours[i], format = "%d-%m-%y %H:%M", tz = "UTC")# convert to ROMS file date format
    ilon <- data$lon[i]
    ilat <- data$lat[i]
    idepth <- data$depth[i]
    
    ## open netcdf matching(d)
    product_files <- list.files(paste(repo, product_info$service, product_info$layer, sep="/"), full.names=TRUE, recursive=TRUE)
    ncfile <- product_files[1]
    nc <- nc_open(ncfile)
    
    ## identify nearest neighbour locations
    minlon <- which.min(abs(nclon - ilon)) # search for nearest longitude dimension element
    minlat <- which.min(abs(nclat - ilat)) # search for nearest latitude dimension element
    mindepth <- which.min(abs(ncdepth - idepth)) # search for nearest depth dimension element
    mintime <- which.min(abs(ncday - iday)) #or ncday depending # search for nearest date dimension element
    
    ## get variable
    ncdata <- ncvar_get(nc, varid=var, start=c(minlon, minlat, 1, mintime), count=c(1,1,maxZ,1)) #[lon,lat,depth,time]
    
    # This means that the parameters lon, lat, depth and time are approximated to the one of the netCDF to which they are closer
    # this happens even if the parameters is very far away from the closer netCDF point. For example, if you .nc has data from 
    # 2021-01-01 to 2023-07-01 and you want to extact data on 1990-01-01, it will extract for the first day of the .nc (2021-01-01)
    # and the same happens for depth, lon and lat.
    
    out[, i] <- ncdata
    
    ## close nc
    nc_close(nc)
  }
  
  # return data.frame
  return(out)
}

#--------------------------------------------------------------------------------------
# Extract your own data
#--------------------------------------------------------------------------------------
#You will have to repeat the process for each raster:

# Repository to folder where netCDFs are:
repo <- paste0(output_data, "/cmems") #You will have to reorganise your netCDFs within subfolders named as: "product_1" and "product_2" as in catalog.
View(catalog)
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# (1.1) Dissolved Oxygen (SSO) ANALYSIS 
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Extract data:
SSO_analysis <- cmems3dmat(lon=data$lon, lat=data$lat, date=data$date, productid=4, repo=repo) #each column name is the date of the observation: e.g. as.Date(18691, origin = "1970-01-01") = 2021-05-03
head(SSO_analysis)
#View(SSO_analysis)

# The extracted data regards to the parameters lon, lat, depth and time that are closer to the one of the netCDF 
# this happens even if the parameters are very far away from the closer netCDF point. For example, if you .nc has data from 
# 2021-01-01 to 2023-07-01 and you want to extact data on 1990-01-01, it will extract for the first day of the .nc (2021-01-01)
# and the same happens for depth, lon and lat.

# In terms of depth, it extracts all the values at the closer lat, lon and date to the netCDF but at all depths.
# Thus, you have to later select which one is the value you are interested in (in my case the deepest and the shallower).

#First re-arrange the extracted data set by: 

# (1) Make list of each of the columns you will use to create the dataframe:
#Create a column for depth categories:
depth_layer <- as.numeric(dimnames(SSO_analysis)[[1]])

# Initialize a list to store the selected columns
all_columns_o2 <- list()
# Loop through each column and select all values
for (i in 1:ncol(SSO_analysis)) {
  all_columns_o2[[i]] <- SSO_analysis[, i]
}

# (2) Change the name of the columns to date format to understand better which dates are outside the range of the particular
# netCDF you are obtaining the data from. In my case half of the data comes from Reanalysis netCDF and part from Analysis.

# Generate a sequence of names regarding to the dates
col_names <- colnames(SSO_analysis)
# Assign the names to the list
names(all_columns_o2) <- as.Date(as.numeric(col_names), origin = "1970-01-01")

# (3) Create a dataframe with 1 column regarding to depth and the rest to each of the points (3078 points):

# Create a new DataFrame with the "depth_layer" and "o2_level" columns
SSO_analysis_df <- data.frame(depth_layer, all_columns_o2)
head(SSO_analysis_df)
#View(SSO_analysis_df)

# (4) Select the first and last datum regarding to the shallowest (surface) and deepest possition within each location point (lon, lat, depth)

# Initialize empty vectors to store the unique deepest and surface values
unique_deepest <- vector("numeric", length(SSO_analysis_df) - 1)
unique_surface <- vector("numeric", length(SSO_analysis_df) - 1)

# Loop through the columns of SSO_analysis_df starting from the second column (index 2)
for (i in 2:length(SSO_analysis_df)) {
  col <- SSO_analysis_df[[i]]
  last_non_na_row <- max(which(!is.na(col)))
  
  # Store the unique deepest and surface values in the respective vectors
  unique_deepest[i - 1] <- col[last_non_na_row]
  unique_surface[i - 1] <- col[1]
}

# (5) Add the data to your dataframe:

# Add the unique values to the "data" dataframe
data$seabottom_o2 <- unique_deepest
data$seasurface_o2 <- unique_surface

View(data)
