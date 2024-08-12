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


# 1. Set data repository -------------------------------------------------------
# Set raster resolution and extent
res <- 0.1
e <- extent(-6, 30, 40, 46)

# Set period
date_start <- as.Date("2021-01-01")
date_end <- as.Date("2021-12-31")

# dynamic variables to extract (same names as catalog)
catalog <- read.csv2("input/Catalog_CMEMS.csv", sep=";")
catalog$variable
env_dyn_vars <- c("bottomT", "o2", "nppv", "ph", "nh4", "no3", "po4", "so", "uo", "vo")

# import catalogue with oceanographic products
provider_paths <- list(CMEMS = paste0(input_data, "/cmems_predict"))

# path to environmental static data
static_data <- paste0(input_data, "/summary_static/")

# path to output
outdir <- paste0(temp_data, "/stack_daily/") 
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)




# 2. Create oceanmask-----------------------------------------------------------
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





# 4. Prepare dynamic variables--------------------------------------------------
# prepare function for stack:
prepareGridCatalogue <- function(var, date, catalog, m, method, name){
  # use the catalog as main source to locate where the data is found
  
  
  library(stringr)
  library(lubridate)
  
  # create empty map with NA in case no file is found
  empty_m <- m
  empty_m[empty_m == 1] <- NA
  
  # locate product by variable name and date
  c <- catalog %>%
    dplyr::filter(catalog$var_name == var)
  
  # get date information
  YYYY <- year(dates)
  MM <- sprintf("%02d", month(dates))
  DD <- sprintf("%02d", day(dates))
  
  # get frequency of the product and set threshold
  # A threshold is define in order to extract data from the closest day/week in case no file available
  # for a given date
  #freq <- c$temporal_resolution
  #temp_thrs <- 1  # will get data from previous/next day
  #if(freq == "daily") temp_thrs <- 1  # will get data from previous/next day
  #if(freq == "weekly") temp_thrs <- 8  # will get data from previous/next week
  #if(freq == "monthly")  temp_thrs <- 30  # will get data from previous/next month
  
  # locate folder
  #repo = paste0(output_data, "/cmems") CHANGED THIS (GRETA)
  repo = paste0(input_data, "/cmems_predict")
  #service = c$service
  #product = c$product
  product_folder <- paste(repo, YYYY, MM, DD, sep="/")  # Set folder
  
  
  # list dates of files
  variable = c$variable
  folder_files <- list.files(product_folder, full.names=TRUE, pattern = variable)  # import to differentiate between vars
  #folder_dates <- ymd(as.numeric(str_extract(folder_files, "\\d{8}")))
  
  
  # find closest date within frequency threshold
  #dif_time <- as.numeric(abs(difftime(as.Date(date), folder_dates, units = "days")))
  
  # check that there is data to retrieve
  if (length(folder_files)==1){
    
    # select file
    #sel_dif <- which(dif_time == min(dif_time))[1]  # in case >1 selected (eg. no data for date i, and select both previous and after)
    #file_path <- folder_files[sel_dif]
    
    # import file
    r <- raster(folder_files, var=variable)  # open nc
    
    # resample at common grid
    pg <- prepareGrid(r, m, method=method, name=name)
    
    
  } else {
    pg <- empty_m
    names(pg) <- name
  }
  
  return(pg)
}


# set number of cores 
cores <- 6
# Prepare cluster
cl <- makeCluster(cores)
registerDoParallel(cl)

# Create dates
dates <- seq.Date(date_start, date_end, by="day")

# Process data
foreach(i=1:length(dates), .packages=c("lubridate", "raster", "stringr", "dplyr")) %dopar% {
  
  # set day i
  date <- dates[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  print(date)
  
  # creaty empty stack
  stack_dynamic <- stack()
  
  # loop for each dynamic variable
  for (j in 1:length(env_dyn_vars)){
    
    jvar <- prepareGridCatalogue(var = env_dyn_vars[j], catalog = catalog, name = env_dyn_vars[j], date = date, m = m, method = "bilinear")
    stack_dynamic <- stack(stack_dynamic, jvar)
  }
  
  # modify ice variables to add zero values
  stack_dynamic$SIC[is.na(stack_dynamic$SIC)] <- 0
  stack_dynamic$SIC <- raster::mask(stack_dynamic$SIC, bat)
  
  stack_dynamic$SIT[is.na(stack_dynamic$SIT)] <- 0
  stack_dynamic$SIT <- raster::mask(stack_dynamic$SIT, bat)
  
  # combine with static stack
  s <- stack(stack_static, stack_dynamic)
  s <- setZ(s, rep(date, nlayers(s)))
  
  # set/create folder
  product_folder <- paste(outdir, YYYY, MM, sep="/")  # Set folder
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  # store file in GRD format
  outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_enviro.grd")
  writeRaster(s, outfile, bandorder='BIL', overwrite=TRUE)
}



#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

print("Daily stack ready")  





