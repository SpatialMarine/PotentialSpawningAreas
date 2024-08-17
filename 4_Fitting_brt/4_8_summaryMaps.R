# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 4.8. Predict BRT bootstrap maps
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

genus <- "Raja" #"Raja" #"Scyliorhinus"
family <- "LN_laplace_sinO2" #bernuilli #LN_laplace_sinO2
type <- "_NKm2" #"_NKm2" "_PA" "only_P
mod_code <- "brt"

# 1. Set data repository--------------------------------------------------------
# Import landmask
mask <- st_read("input/landmask/Europa/Europe_coastline_poly.shp")
print(mask)
mask <- st_transform(mask, crs = 4326)

# crop it:
e <- c(-3, 5, 35, 43)
e <- extent(e)
bbox <- st_as_sfc(st_bbox(e))
# Set the CRS of bbox to match the mask
st_crs(bbox) <- st_crs(mask) 
#Crop the mask using the bounding box
mask <- st_intersection(mask, bbox)
print(mask)


# Create date sequences that you wish:

# Monthly:
winter_start <- as.Date("2021-01-01")
winter_end <- as.Date("2021-03-20")
winter_dates <- seq.Date(winter_start, winter_end, by="day")

spring_start <- as.Date("2021-03-21")
spring_end <- as.Date("2021-06-20")
spring_dates <- seq.Date(spring_start, spring_end, by="day")

summer_start <- as.Date("2021-06-21")
summer_end <- as.Date("2021-09-22")
summer_dates <- seq.Date(summer_start, summer_end, by="day")

autumn_start <- as.Date("2021-09-23")
autumn_end <- as.Date("2021-12-20")
autumn_dates <- seq.Date(autumn_start, autumn_end, by="day")


# Year:
date_start <- as.Date("2021-01-01")
date_end <- as.Date("2021-12-31")
dates <- seq.Date(date_start, date_end, by="day")  # define sequence


# Convert date sequences to dataframes
winter_df <- data.frame(date = winter_dates, season = "Winter")
spring_df <- data.frame(date = spring_dates, season = "Spring")
summer_df <- data.frame(date = summer_dates, season = "Summer")
autumn_df <- data.frame(date = autumn_dates, season = "Autumn")

year_df <- data.frame(date = dates, season = "Autumn")






# 2. Merge HABITAT maps to create seasonal means----------------------------------------

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







# 3. Merge ERROR maps to create seasonal means----------------------------------------
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
  pat <- paste0(format(date, "%Y%m%d"), "_", genus, "_", mod_code, "_pred_cir.tif")
  
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

# After parallel processing, create a stack from the list of raster stacks
pred_stack <- raster::stack(stack_list)

# Calculate the median of the raster stack
pred_med <- raster::calc(pred_stack, fun = median)

# Define output paths
tifffile <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_pred_CIR_median.tif")
pngfile <- paste0("output/", mod_code, "/", paste0(genus, type, "_", family), "/predict_boost/2021/", season, "_pred_CIR_median.png")

# Save the median raster as TIFF
writeRaster(pred_med, filename = tifffile, format = "GTiff", overwrite = TRUE)

# Save the median raster as PNG
png(pngfile, width = 560, height = 600, res = 100)
plot(pred_med, main = paste(genus, "   Model:", mod_code, "\n", season), col = viridis(100))
plot(mask, col = "grey80", border = "grey60", add = TRUE)
text(x = -3.5, y = 44, labels = format(date, "%Y-%m-%d"))
box()
dev.off()
