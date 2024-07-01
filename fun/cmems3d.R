# create new extract function
#--------------------------------------------------------------------------------------
# cmems3dmat       Extract vertical profiles into matrix from 3D ROMS numerical models netcdfs along the path
#--------------------------------------------------------------------------------------
#adapted from dmarch github: https://github.com/dmarch/ocean3d/blob/8b525bd1b13bea93f608e89f40ae9a561ca49e64/R/cmems2track_v2.R#L138

cmems3d_surface <- function(lon, lat, date, productid, repo, data, maxZ = NULL) {
  # Description
  # Extracts oceanographic information from 3D numerical models downloaded from CMEMS
  
  # Arguments
  # data        Your database
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
  
  # Load libraries
  library(lubridate)
  library(ncdf4)
  library(dplyr)
  
  # Get information and variable name for a given product
  product_info <- filter(cat, id_product == productid)
  var <- as.character(product_info$variable)
  
  # get all product files
  product_files <- list.files(paste(repo, product_info$service, product_info$layer, product_info$var_name, sep="/"), full.names=TRUE, recursive=TRUE)
  if (length(product_files) == 0) {
    stop("No product files found in the specified repository path.")
  }
  
  # select first file and get dimensions
  ncfile <- product_files[1]
  nc <- nc_open(ncfile)
  nclon <- nc$dim$lon$vals # ncvar_get(nc, varid="lon")
  nclat <- nc$dim$lat$vals # ncvar_get(nc, varid="lat")
  ncdepth <- nc$dim$depth$vals
  nctime <- nc$dim$time$vals
  ncday <- as.POSIXct(nctime, origin = "1970-01-01", tz = "UTC")
  maxZ <- if (is.null(maxZ)) nc$dim$depth$len else maxZ
  nc_close(nc)
  
  # output matrix
  out <- matrix(data = NA, nrow = maxZ, ncol = length(data$date), byrow = FALSE,  
                dimnames = list(ncdepth[1:maxZ], data$date))
  
  # get data for each observation
  for (i in 1:length(date)) {
    print(i)
    
    # get day, lon, lat, depth
    iday <- as.POSIXct(data$date_time[i], format = "%Y-%m-%d %H:%M", tz = "UTC")
    ilon <- data$lon[i]
    ilat <- data$lat[i]
    idepth <- data$depth[i]
    
    # open netcdf matching(d)
    product_files <- list.files(paste(repo, product_info$service, product_info$layer, product_info$var_name, sep="/"), full.names=TRUE, recursive=TRUE)
    ncfile <- product_files[1]
    nc <- nc_open(ncfile)
    
    # identify nearest neighbour locations
    minlon <- which.min(abs(nclon - ilon))
    minlat <- which.min(abs(nclat - ilat))
    mindepth <- which.min(abs(ncdepth - idepth))
    mintime <- which.min(abs(ncday - iday))
    
    # get variable
    ncdata <- ncvar_get(nc, varid=var, start=c(minlon, minlat, 1, mintime), count=c(1,1,maxZ,1))
    
    out[, i] <- ncdata
    
    # close nc
    nc_close(nc)
  }
  
  # (1) Make list of each of the columns you will use to create the dataframe
  depth_layer <- as.numeric(dimnames(out)[[1]])
  all_columns <- list()
  for (i in 1:ncol(out)) {
    all_columns[[i]] <- out[, i]
  }
  
  # (2) Change the name of the columns to date format
  col_names <- colnames(out)
  names(all_columns) <- as.Date(as.numeric(col_names), origin = "1970-01-01")
  
  # (3) Create a dataframe with 1 column regarding to depth and the rest to each of the points
  df <- data.frame(depth_layer, all_columns)
  
  # (4) Select the first, last, and nearest depth data
  unique_surface <- vector("numeric", length(df) - 1)

  for (i in 2:length(df)) {
    col <- df[[i]]
    unique_surface[i - 1] <- col[1]
   }
  
  # (5) Add the data to your dataframe
  data[[paste0("seasurface_", product_info$variable)]] <- unique_surface
 
  # return final dataframe
  return(data)
}

cmems3d_bottom <- function(lon, lat, date, productid, repo, data, maxZ = NULL) {
  # Description
  # Extracts oceanographic information from 3D numerical models downloaded from CMEMS
  
  # Arguments
  # data        Your database
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
  
  # Load libraries
  library(lubridate)
  library(ncdf4)
  library(dplyr)
  
  # Get information and variable name for a given product
  product_info <- filter(cat, id_product == productid)
  var <- as.character(product_info$variable)
  
  # get all product files
  product_files <- list.files(paste(repo, product_info$service, product_info$layer, product_info$var_name, sep="/"), full.names=TRUE, recursive=TRUE)
  if (length(product_files) == 0) {
    stop("No product files found in the specified repository path.")
  }
  
  # select first file and get dimensions
  ncfile <- product_files[1]
  nc <- nc_open(ncfile)
  nclon <- nc$dim$lon$vals # ncvar_get(nc, varid="lon")
  nclat <- nc$dim$lat$vals # ncvar_get(nc, varid="lat")
  ncdepth <- nc$dim$depth$vals
  nctime <- nc$dim$time$vals
  ncday <- as.POSIXct(nctime, origin = "1970-01-01", tz = "UTC")
  maxZ <- if (is.null(maxZ)) nc$dim$depth$len else maxZ
  nc_close(nc)
  
  # output matrix
  out <- matrix(data = NA, nrow = maxZ, ncol = length(data$date), byrow = FALSE,  
                dimnames = list(ncdepth[1:maxZ], data$date))
  
  # get data for each observation
  for (i in 1:length(date)) {
    print(i)
    
    # get day, lon, lat, depth
    iday <- as.POSIXct(data$date_time[i], format = "%Y-%m-%d %H:%M", tz = "UTC")
    ilon <- data$lon[i]
    ilat <- data$lat[i]
    idepth <- data$depth[i]
    
    # open netcdf matching(d)
    product_files <- list.files(paste(repo, product_info$service, product_info$layer, product_info$var_name, sep="/"), full.names=TRUE, recursive=TRUE)
    ncfile <- product_files[1]
    nc <- nc_open(ncfile)
    
    # identify nearest neighbour locations
    minlon <- which.min(abs(nclon - ilon))
    minlat <- which.min(abs(nclat - ilat))
    mindepth <- which.min(abs(ncdepth - idepth))
    mintime <- which.min(abs(ncday - iday))
    
    # get variable
    ncdata <- ncvar_get(nc, varid=var, start=c(minlon, minlat, 1, mintime), count=c(1,1,maxZ,1))
    
    out[, i] <- ncdata
    
    # close nc
    nc_close(nc)
  }
  
  # (1) Make list of each of the columns you will use to create the dataframe
  depth_layer <- as.numeric(dimnames(out)[[1]])
  all_columns <- list()
  for (i in 1:ncol(out)) {
    all_columns[[i]] <- out[, i]
  }
  
  # (2) Change the name of the columns to date format
  col_names <- colnames(out)
  names(all_columns) <- as.Date(as.numeric(col_names), origin = "1970-01-01")
  
  # (3) Create a dataframe with 1 column regarding to depth and the rest to each of the points
  df <- data.frame(depth_layer, all_columns)
  
  # (4) Select the first, last, and nearest depth data
  unique_deepest <- vector("numeric", length(df) - 1)

  for (i in 2:length(df)) {
    col <- df[[i]]
    last_non_na_row <- max(which(!is.na(col)))
    unique_deepest[i - 1] <- col[last_non_na_row]
  }
  
  # (5) Add the data to your dataframe
  data[[paste0("seabottom_", product_info$variable)]] <- unique_deepest

  # return final dataframe
  return(data)
}

cmems3d_nearest <- function(lon, lat, date, productid, repo, data, maxZ = NULL) {
  # Description
  # Extracts oceanographic information from 3D numerical models downloaded from CMEMS
  
  # Arguments
  # data        Your database
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
  
  # Load libraries
  library(lubridate)
  library(ncdf4)
  library(dplyr)
  
  # Get information and variable name for a given product
  product_info <- filter(cat, id_product == productid)
  var <- as.character(product_info$variable)
  
  # get all product files
  product_files <- list.files(paste(repo, product_info$service, product_info$layer, product_info$var_name, sep="/"), full.names=TRUE, recursive=TRUE)
  if (length(product_files) == 0) {
    stop("No product files found in the specified repository path.")
  }
  
  # select first file and get dimensions
  ncfile <- product_files[1]
  nc <- nc_open(ncfile)
  nclon <- nc$dim$lon$vals # ncvar_get(nc, varid="lon")
  nclat <- nc$dim$lat$vals # ncvar_get(nc, varid="lat")
  ncdepth <- nc$dim$depth$vals
  nctime <- nc$dim$time$vals
  ncday <- as.POSIXct(nctime, origin = "1970-01-01", tz = "UTC")
  maxZ <- if (is.null(maxZ)) nc$dim$depth$len else maxZ
  nc_close(nc)
  
  # output matrix
  out <- matrix(data = NA, nrow = maxZ, ncol = length(data$date), byrow = FALSE,  
                dimnames = list(ncdepth[1:maxZ], data$date))
  
  # get data for each observation
  for (i in 1:length(date)) {
    print(i)
    
    # get day, lon, lat, depth
    iday <- as.POSIXct(data$date_time[i], format = "%Y-%m-%d %H:%M", tz = "UTC")
    ilon <- data$lon[i]
    ilat <- data$lat[i]
    idepth <- data$depth[i]
    
    # open netcdf matching(d)
    product_files <- list.files(paste(repo, product_info$service, product_info$layer, product_info$var_name, sep="/"), full.names=TRUE, recursive=TRUE)
    ncfile <- product_files[1]
    nc <- nc_open(ncfile)
    
    # identify nearest neighbour locations
    minlon <- which.min(abs(nclon - ilon))
    minlat <- which.min(abs(nclat - ilat))
    mindepth <- which.min(abs(ncdepth - idepth))
    mintime <- which.min(abs(ncday - iday))
    
    # get variable
    ncdata <- ncvar_get(nc, varid=var, start=c(minlon, minlat, 1, mintime), count=c(1,1,maxZ,1))
    
    out[, i] <- ncdata
    
    # close nc
    nc_close(nc)
  }
  
  # (1) Make list of each of the columns you will use to create the dataframe
  depth_layer <- as.numeric(dimnames(out)[[1]])
  all_columns <- list()
  for (i in 1:ncol(out)) {
    all_columns[[i]] <- out[, i]
  }
  
  # (2) Change the name of the columns to date format
  col_names <- colnames(out)
  names(all_columns) <- as.Date(as.numeric(col_names), origin = "1970-01-01")
  
  # (3) Create a dataframe with 1 column regarding to depth and the rest to each of the points
  df <- data.frame(depth_layer, all_columns)
  
  # (4) Select the first, last, and nearest depth data
  unique_nearest <- vector("numeric", length(df) - 1)
  
  for (i in 2:length(df)) {
    col <- df[[i]]
    min_depth_diff <- which.min(abs(depth_layer - data$depth[i-1])) # Find the index of the depth closest to the desired depth
    #data$depth[i-1]: This is the target depth value from the data dataframe for the current observation. Note that we use i-1 because data$depth is indexed starting from 1, while the loop for (i in 2:length(SSO_analysis_df)) starts from 2.
    unique_nearest[i - 1] <- col[min_depth_diff]
  }
  
  # (5) Add the data to your dataframe
  data[[paste0("nearest_", product_info$variable)]] <- unique_nearest
  
  # return final dataframe
  return(data)
}

