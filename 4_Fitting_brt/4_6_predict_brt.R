# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 4.6. predict_brt          Predict BRT
#-------------------------------------------------------------------------------
library(ggplot2)
library(sf)

mod_code <- "brt"
bootstrap <- F
genus <- "Raja" #"Raja" #"Scyliorhinus"
type <- "_Nkm2" #"_Nk2" #"_PA"


# 1. Import model and data------------------------------------------------------
indir <- paste(output_data, mod_code, paste0(genus, type), sep="/")
outdir <- paste(indir, "predict_boost", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Import model
mod <- readRDS(paste0(indir , "/", genus, type, ".rds"))

# Import landmask
mask <- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
print(mask)
mask <- st_transform(mask, crs = 4326)

# Prepare clusters
cores <-detectCores()
cores #if you use all of them you, your computer may crash (consumes all the CPU).
cores <- 6  
cl <- makeCluster(cores)
registerDoParallel(cl)

# Create dates
date_start <- as.Date("2021-01-01")
date_end <- as.Date("2021-12-31")

dates <- seq.Date(date_start, date_end, by="day")  # define sequence
foreach(i=1:length(dates), .packages=c("lubridate", "raster", "stringr", "dplyr", "pals", "dismo", "gbm")) %dopar% {
  
  # Get time information
  date <- dates[i]
  YYYY <- year(date)
  MM <- sprintf("%02d", month(date))
  
  # Locate file
  pat <- paste0(format(date, "%Y%m%d"), "_enviro.grd")
  grdfile <- list.files(stack_repo, recursive = TRUE, full.names = TRUE, pattern = pat)
  
  # Import environmental stack
  s <- raster::stack(grdfile)
  s <- s+0
  
  # Model prediction
  pred <- raster::predict(model = mod, object = s, n.trees=mod$gbm.call$best.trees, type="response")
  
  # set/create folder
  product_folder <- paste(outdir, YYYY, MM, sep="/")  # Set folder
  if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)  # create output directory if does not exist
  
  # store file in ncformat
  outfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_pred.tif")
  writeRaster(pred, filename=outfile, overwrite=TRUE)
  
  # export plot
  pngfile <- paste0(product_folder, "/", format(date, "%Y%m%d"),"_", sp_code, "_", mod_code, "_pred.png")
  png(pngfile, width=560, height=600, res=100)
  plot(pred, main = paste(sp_name, "   Model:", mod_code, "\n", date), zlim=c(0,1), col = viridis(100))
  plot(land, col="grey80", border="grey60", add=TRUE)
  text(x = -3.5, y = 44, labels = date)
  box()
  dev.off()
}

#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

print("Prediction ready")  

