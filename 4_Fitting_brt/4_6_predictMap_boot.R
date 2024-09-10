# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 4.7. Predict BRT bootstrap maps
#-------------------------------------------------------------------------------
library(beepr)
library(parallel)
library(doParallel)
library(lubridate)
library(sf)
library(raster)
library(gbm)
library(viridis)
library(foreach)


bootstrap <- T

genus <- "Scyliorhinus" #"Raja" #"Scyliorhinus"
family <- "LN_laplace_Final" #bernuilli #LN_laplace_sinO2
type <- "_NKm2" #"_NKm2" "_PA" "only_P
mod_code <- "brt"
dataset <- "ALL" #ALL, train


# 1. Set data repository--------------------------------------------------------
indir <- paste(output_data, mod_code, paste0(genus, type, "_", family), sep="/")


outdir <- paste0(indir, "/predict_boost")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Import landmask
mask <- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
print(mask)
mask <- st_transform(mask, crs = 4326)

# crop it:
e <- c(-1.5, 5, 37, 43) 
e <- extent(e)
bbox <- st_as_sfc(st_bbox(e))
# Set the CRS of bbox to match the mask
st_crs(bbox) <- st_crs(mask) 
#Crop the mask using the bounding box
mask <- st_intersection(mask, bbox)
print(mask)
#plot(mask)


bathy<- raster("input/gebco/Bathy.tif")
print(bathy)
# plot(bathy)
# Filter the values between -50 and -600 and set values outside the range to NA
bathy_filtered <- calc(bathy, function(x) {
  x[x > -1 | x < -650] <- NA  # Set values outside the range to NA
  return(x)
})

# Assign a value of 1 to the remaining (non-NA) values
bathy_mask <- calc(bathy_filtered, function(x) {
  x[!is.na(x)] <- 1
  return(x)
})

bathy_mask_resampled <- resample(bathy_mask, s, method = "bilinear")
bathy_mask_resampled <- crop(bathy_mask_resampled, e)

# list of bootstrap models
outdir_bootstrap <- paste0(indir, "/bootstrap/", paste0(genus, type, "_", family))
boots_files <- list.files(outdir_bootstrap, full.names = T)

# batch import of bootstrap models
brt_models <- lapply(boots_files, readRDS)
print(brt_models[1])
brt_models <- brt_models[1:10]

# Prepare cluster
# cores <-detectCores() #if you use all of them you, your computer may crash (consumes all the CPU).
# cores <- 2
# cl <- makeCluster(cores)
# registerDoParallel(cl)

# Create dates
date_start <- as.Date("2021-06-08") 
date_end <- as.Date("2021-12-31")
#dates <- seq.Date(date_start, date_end, by="day")  # define sequence
dates <- seq.Date(date_start, date_end, by="2 days")  # define sequence every 2 days

# Missing>
#2021-02-23 to 2021-03-01
#2021-03-25 to 2021-04-01
#

# Missing for Raja
#dates <-  as.Date(c('2021-01-03', '2021-01-05', '2021-01-06', '2021-01-09', '2021-01-11', '2021-01-13', '2021-01-15', '2021-01-17', '2021-01-19', '2021-01-21', '2021-01-23', '2021-01-25', '2021-01-27', '2021-01-29', '2021-01-31', '2021-02-02', '2021-02-04', '2021-02-06', '2021-02-08', '2021-02-10', '2021-02-12', '2021-02-14', '2021-02-16', '2021-02-18', '2021-02-20', '2021-02-22', '2021-02-24', '2021-02-26', '2021-02-28', '2021-03-03', '2021-03-05', '2021-03-07', '2021-03-09', '2021-03-11', '2021-03-13', '2021-03-15', '2021-03-17', '2021-03-19', '2021-03-21', '2021-03-23', '2021-04-02', '2021-04-04', '2021-04-06', '2021-04-08', '2021-04-10', '2021-04-12', '2021-04-14', '2021-04-16', '2021-04-18', '2021-04-20', '2021-04-22', '2021-04-24', '2021-04-26', '2021-04-28', '2021-04-30', '2021-05-02', '2021-05-04', '2021-05-06', '2021-05-08', '2021-05-10', '2021-05-12', '2021-05-14', '2021-05-16', '2021-05-18', '2021-05-20', '2021-05-22', '2021-05-24', '2021-05-26', '2021-05-28', '2021-05-30', '2021-06-02', '2021-06-04', '2021-06-06', '2021-06-08', '2021-06-10', '2021-06-12', '2021-06-14', '2021-06-16', '2021-06-18', '2021-06-20', '2021-06-22', '2021-06-24', '2021-06-26', '2021-06-28', '2021-06-30', '2021-07-02', '2021-07-04', '2021-07-06', '2021-07-08', '2021-07-10', '2021-07-12', '2021-07-14', '2021-07-16', '2021-07-18', '2021-07-20', '2021-07-22', '2021-07-24', '2021-07-26', '2021-07-28', '2021-07-30', '2021-08-01', '2021-08-03', '2021-08-05', '2021-08-07', '2021-08-09', '2021-08-11', '2021-08-13', '2021-08-15', '2021-08-17', '2021-08-19', '2021-08-21', '2021-08-23', '2021-08-25', '2021-08-27', '2021-08-29', '2021-08-31', '2021-09-02', '2021-09-04', '2021-09-06', '2021-09-08', '2021-09-10', '2021-09-12', '2021-09-14', '2021-09-16', '2021-09-18', '2021-09-20', '2021-09-22', '2021-09-24', '2021-09-26', '2021-09-28', '2021-09-30', '2021-10-02', '2021-10-04', '2021-10-06', '2021-10-08', '2021-10-10', '2021-10-12', '2021-10-14', '2021-10-16', '2021-10-18', '2021-10-20', '2021-10-22', '2021-10-24', '2021-10-26', '2021-10-28', '2021-10-30', '2021-11-01', '2021-11-03', '2021-11-05', '2021-11-07', '2021-11-09', '2021-11-11', '2021-11-13', '2021-11-15', '2021-11-17', '2021-11-19', '2021-11-21', '2021-11-23', '2021-11-25', '2021-11-27', '2021-11-29', '2021-12-01', '2021-12-03', '2021-12-05', '2021-12-07', '2021-12-09', '2021-12-11', '2021-12-13', '2021-12-15', '2021-12-17', '2021-12-19', '2021-12-21', '2021-12-23', '2021-12-26', '2021-12-28', '2021-12-30'))


# 2. Create bootstrap maps (habitat and CI)-------------------------------------
#foreach(i=1:length(dates), .packages=c("lubridate", "raster", "stringr", "dplyr", "pals", "dismo", "gbm", "scam")) %dopar% {

for(i in 1:length(dates)) {
  
  # Get time information
  #i=1
  date <- dates[i]
  print(date)
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Locate file
  pat <- paste0( "stack_", format(date, "%Y%m%d"), ".grd")
  # Get list of all month folders
  stack_repo <- paste0("input/cmems_predict_3d/2021/", MM, "/", DD)
  grdfile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
  
  # Import environmental stack
  s <- raster::stack(grdfile)
  s <- s+0

  s <- crop(s, e)
  s <- raster::mask(s, bathy_mask_resampled)
  
  # Transform variables
  s$ln_fishingEffort <- log1p(s$fishingEffort)
  #s$ln_slope <- log1p(s$slope)
  
  # Initialize a list to hold prediction results
  stack_list <- list()
  
  # Model prediction (BRT)
  for(j in 1:length(brt_models)){  
    
    # predict BRT
    # j=1
    pred_brt <- raster::predict(model = brt_models[[j]], object = s, n.trees=brt_models[[j]]$gbm.call$best.trees, type="response")
    
    # Add to stack list
    stack_list[[j]] <- pred_brt
  }
  
  # create stack from list
  pred_stack <- raster::stack(stack_list)
  
  # Average predictions
  pred_med <- raster::calc(pred_stack, median)
  
  #confidence interval 95% range 
  pred_cil <- raster::calc(pred_stack, fun = function(x){quantile(x, probs = c(0.025),na.rm=TRUE)})
  pred_ciu <- raster::calc(pred_stack, fun = function(x){quantile(x, probs = c(0.975),na.rm=TRUE)})
  pred_cir <- pred_ciu - pred_cil
  
  # set/create folder
  product_folder <- paste(outdir, YYYY, MM, sep="/")  # Set folder
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  # store file
  outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", genus, "_", mod_code, "_pred.tif")
  writeRaster(pred_med, filename=outfile, overwrite=TRUE)
  #plot(pred_med)
  
  # store file
  outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", genus, "_", mod_code, "_pred_cir.tif")
  writeRaster(pred_cir, filename=outfile, overwrite=TRUE)
  #plot(pred_cir)
  
  # export plot
  pngfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", genus, "_", mod_code, "_pred.png")
  png(pngfile, width=560, height=600, res=100)
  plot(pred_med, main = paste(genus, "   Model:", mod_code, "\n", date), col = viridis(100))
  plot(mask, col="grey80", border="grey60", add=TRUE)
  text(x = -3.5, y = 44, labels = date)
  box()
  dev.off()
  
  # export plot
  pngfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", genus, "_", mod_code, "_pred_cir.png")
  png(pngfile, width=560, height=600, res=100)
  plot(pred_cir, main = paste(genus, "   Model:", mod_code, "\n", date), col = viridis(100))
  plot(mask, col="grey80", border="grey60", add=TRUE)
  text(x = -3.5, y = 44, labels = date)
  box()
  dev.off()
  
}

#stopCluster(cl)

print("Prediction ready")  
beep()








# 2. Merge bottomTemp maps to calculate Standard Deviation ----------------------------------------

# Prepare your date list and other necessary variables
dates <- year_df #spring_df, winter_df, summer_df, autumn_df
stack_list <- vector("list", nrow(dates))  # Pre-allocate list
season <- "2021"

# Loop through each date
for (i in 1:nrow(dates)) {
  
  # Extract and format the date information
  # i=1
  date <- dates$date[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Construct the file pattern
  pat <- paste0(format(date, "%Y%m%d"), "_", genus, "_", mod_code, "_pred.tif")
  
  # Construct the path to the directory containing TIFF files
  stack_repo <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", MM)
  
  # Debugging prints
  print(paste("Stack Repo:", stack_repo))
  print(paste("Pattern:", pat))
  
  # List all TIFF files that match the pattern
  tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
  print(tiffile)  # Check the output
  
  # Debugging print
  print(paste("Found TIFF files:", length(tiffile)))
  
  if (length(tiffile) > 0) {
    s <- tryCatch({
      raster::stack(tiffile)
    }, error = function(e) {
      cat("Error in stacking raster files:", e$message, "\n")
      NULL
    })
    
    if (!is.null(s)) {
      stack_list[[i]] <- s
    }
  }
}

# Print a message indicating completion
print("Processing completed.")
beep()


# Identify which elements in the list are NULL
null_indices <- which(sapply(stack_list, is.null))

# After parallel processing, create a stack from the list of raster stacks
pred_stack <- raster::stack(stack_list)

# Calculate the median of the raster stack
pred_med <- raster::calc(pred_stack, fun = median)

# Define output paths
tifffile <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_pred_median.tif")
pngfile <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_pred_median.png")

# Save the median raster as TIFF
writeRaster(pred_med, filename = tifffile, format = "GTiff", overwrite = TRUE)

# Save the median raster as PNG
png(pngfile, width = 560, height = 600, res = 100)
plot(pred_med, main = paste(genus, "   Model:", mod_code, "\n", season), col = viridis(100))
plot(mask, col = "grey80", border = "grey60", add = TRUE)
text(x = -3.5, y = 44, labels = format(date, "%Y-%m-%d"))
box()
dev.off()







# 3. Merge Enviro maps to calculate Mean ----------------------------------------
date_start <- as.Date("2021-01-01") #"2021-06-08"
date_end <- as.Date("2021-12-31")
#dates <- seq.Date(date_start, date_end, by="day")  # define sequence
dates <- seq.Date(date_start, date_end, by="day")  # define sequence every 2 days
dates <- as.data.frame(dates)
# Prepare your date list and other necessary variables
stack_list <- vector("list", nrow(dates))  # Pre-allocate list
season <- "2021"

# Loop through each date
for (i in 1:nrow(dates)) {
  
  # Extract and format the date information
  # i=1
  date <- dates$date[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  DD <- sprintf("%02d", day(date))
  
  # Construct the file pattern
  pat <- paste0(format(date, "%Y%m%d"), "_so_3d.nc")
  
  # Construct the path to the directory containing nc files
  stack_repo <- paste0("input/cmems_predict_3d/2021/", MM, "/", DD)
  
  # Debugging prints
  print(paste("Stack Repo:", stack_repo))
  print(paste("Pattern:", pat))
  
  # List all TIFF files that match the pattern
  tiffile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
  print(tiffile)  # Check the output
  
  # Debugging print
  print(paste("Found TIFF files:", length(tiffile)))
  
  if (length(tiffile) > 0) {
    s <- tryCatch({
      raster::stack(tiffile)
    }, error = function(e) {
      cat("Error in stacking raster files:", e$message, "\n")
      NULL
    })
    
    if (!is.null(s)) {
      stack_list[[i]] <- s
    }
  }
}

# Print a message indicating completion
print("Processing completed.")
beep()


# Identify which elements in the list are NULL
null_indices <- which(sapply(stack_list, is.null))

# After parallel processing, create a stack from the list of raster stacks
pred_stack <- raster::stack(stack_list)

# Calculate the median of the raster stack
pred_med <- raster::calc(pred_stack, fun = mean)
#plot(pred_med)

# Define output paths
tifffile <- paste0("input/cmems_predict_3d/", season, "so_mean.tif")
pngfile <-  paste0("input/cmems_predict_3d/", season, "so_mean.png")
  
# Save the median raster as TIFF
writeRaster(pred_med, filename = tifffile, format = "GTiff", overwrite = TRUE)

# Save the median raster as PNG
png(pngfile, width = 560, height = 600, res = 100)
plot(pred_med, main = paste(genus, "   Model:", mod_code, "\n", season), col = viridis(100))
plot(mask, col = "grey80", border = "grey60", add = TRUE)
text(x = -3.5, y = 44, labels = format(date, "%Y-%m-%d"))
box()
dev.off()
