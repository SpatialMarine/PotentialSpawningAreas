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
family <- "LN_laplace_sinO2" #bernuilli #LN_laplace_sinO2
type <- "_PA" #"_NKm2" "_PA" "only_P
mod_code <- "brt"


# 1. Set data repository--------------------------------------------------------
indir <- paste(output_data, mod_code, paste0(genus, type, "_", family), sep="/")


outdir <- paste0(indir, "/predict_boost")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Import landmask
mask <- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
print(mask)
mask <- st_transform(mask, crs = 4326)

# crop it:
e <- c(-4, 8, 34, 45) 
e <- extent(e)
bbox <- st_as_sfc(st_bbox(e))
# Set the CRS of bbox to match the mask
st_crs(bbox) <- st_crs(mask) 
#Crop the mask using the bounding box
mask <- st_intersection(mask, bbox)
print(mask)


# list of bootstrap models
outdir_bootstrap <- paste0(indir, "/bootstrap/", paste0(genus, type, "_", family))
boots_files <- list.files(outdir_bootstrap, full.names = T)

# batch import of bootstrap models
brt_models <- lapply(boots_files, readRDS)

# Prepare cluster
cores <-detectCores() #if you use all of them you, your computer may crash (consumes all the CPU).
cores <- 7
cl <- makeCluster(cores)
registerDoParallel(cl)

# Create dates
date_start <- as.Date("2021-01-01")
date_end <- as.Date("2021-12-31")
dates <- seq.Date(date_start, date_end, by="day")  # define sequence






# 2. Create bootstrap maps (habitat and CI)-------------------------------------
foreach(i=1:length(dates), .packages=c("lubridate", "raster", "stringr", "dplyr", "pals", "dismo", "gbm", "scam")) %dopar% {
  
  # Get time information
  #i=211
  date <- dates[i]
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

stopCluster(cl)

print("Prediction ready")  
beep()




