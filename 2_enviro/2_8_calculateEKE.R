# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 2.7. Derive Eddy kinetic energy
#-------------------------------------------------------------------------------

# Load libraries
library(raster)
library(foreach)  
library(doParallel)
library(lubridate)
library(dplyr)

# adapt the catalog
catalog <- read.csv("input/Catalog_CMEMS.csv", sep=";")

new_line <- data.frame(
  myName = "EKE",
  id_product = 11,
  var_name = "EKE_Reanalysis",
  variable = "eke",
  dimensions = "3D",
  product_type = "Reanalysis",
  provider = "CMEMS",
  service = "MEDSEA_MULTIYEAR_PHY_006_004",
  layer = "med-cmcc-cur-rean-d",
  standard_name = "eddy_kinetic_energy",
  units = "[m2 s-2]",
  date_min = "01/01/1987",
  date_max = "31/07/2022",
  depth_min = 1,
  depth_max = 1200,
  temporal_resolution = "daily",
  spatial_resolution = "0.042 x 0.042",
  var = "eke",
  xmin = "-2",
  xmax = "4",
  ymin = "36",
  ymax = "42",
  subvar = "eke",
  date_min_total = "01/01/1987",
  date_max_total = "31/07/2022"
)

# Append the new line to the catalog
catalog <- rbind(catalog, new_line)

catalog <- catalog %>%
  filter(variable %in% c("uo", "vo", "eke")) 


# 1. Create a dates dataframe for the dates you want to get data from ----------
# Create a sequence of dates from January 1, 2021, to December 31, 2021
surveyDates <- as.data.frame(seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day"))
colnames(surveyDates) <- "date"
str(surveyDates)



# 2. Function to calculate EKE -------------------------------------------------
generate_eke_gradient <- function(input_u_product_id, input_v_product_id, output_product_id){
  # Set repository for CMEMS products
  cmems_repo <- paste0(input_data, "/cmems_predict_3d")
  
  # Ensure character type for catalog fields
  catalog$product <- as.character(catalog$layer)
  catalog$variable <- as.character(catalog$variable)
  catalog$standard_name <- as.character(catalog$standard_name)
 #catalog$date_min <- dmy(catalog$date_min)
 #catalog$date_max <- dmy(catalog$date_max)
  
  # Retrieve input data information
  input_u_var <- catalog$variable[input_u_product_id]
  input_v_var <- catalog$variable[input_v_product_id]
  
  if (length(input_u_var) != 1 || length(input_v_var) != 1) {
    stop("input_u_var or input_v_var has length > 1")
  }
  
  # Retrieve output data information
  output_product <- catalog$product[output_product_id]
  output_var <- catalog$variable[output_product_id]
  output_name <- catalog$standard_name[output_product_id]
  
  # Add information to survey dates
  surveyDates <- surveyDates %>%
    mutate(
      dateTime = paste0(date, " 11:00:00"),
      year = year(date),
      month = sprintf("%02d", month(date)),
      day = sprintf("%02d", day(date))
    )
  
  # Prepare parallel processing
  cores <- 5
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # Parallel loop for processing each date
  y <- foreach(i = 1:length(surveyDates$date), .packages = c("raster", "lubridate"), .inorder = FALSE) %dopar% {
    tryCatch({
      # Get date components
      #i=1
      date <- surveyDates$date[i]
      YYYY <- year(date)
      MM <- sprintf("%02d", month(date))
      DD <- sprintf("%02d", day(date))
      
      # Construct file paths for U and V components
      #product_folder <- paste(cmems_repo, catalog$service, catalog$layer, sep = "/") # this one is for extraction
      product_folder <- paste(cmems_repo,YYYY, MM, DD, sep = "/")
      
      # U component
      #file_name_u <- paste0(catalog$var_name[1], "_", YYYY, "-", MM, "-", DD, ".nc") # this one is for extraction
      #file_path_u <- paste(product_folder[1], catalog$var_name[1], YYYY, MM, DD, file_name_u, sep = "/") # this one is for extraction
      file_name_u <- paste0(YYYY, MM, DD,"_", catalog$variable[1],"_3d.nc")
      file_path_u <- paste(product_folder[1],file_name_u, sep = "/")
      
      # V component
      #file_name_v <- paste0(catalog$var_name[2], "_", YYYY, "-", MM, "-", DD, ".nc") # this one is for extraction
      #file_path_v <- paste(product_folder[1], catalog$var_name[2], YYYY, MM, DD, file_name_v, sep = "/") # this one is for extraction
      file_name_v <- paste0(YYYY, MM, DD,"_", catalog$variable[2],"_3d.nc")
      file_path_v <- paste(product_folder[1],file_name_v, sep = "/")
      
      # Debugging output for paths
      print(paste("File path U:", file_path_u))
      print(paste("File path V:", file_path_v))
      
      # Check if files exist
      if (file.exists(file_path_u) && file.exists(file_path_v)) {
        u <- raster(file_path_u, var = input_u_var)
        v <- raster(file_path_v, var = input_v_var)
        
        # Calculate EKE
        p <- (u^2 + v^2) / 2
        p <- setZ(p, getZ(u))
        names(p) <- output_var
        
        # Save output raster
        #product_folder <- paste(cmems_repo, "EKE", YYYY, MM, DD, sep = "/") # this one is for extraction
        product_folder <- paste(cmems_repo,YYYY, MM, DD, sep = "/")
                if (!dir.exists(product_folder)) dir.create(product_folder, recursive = TRUE)
        file_name <- paste0(YYYY, MM, DD,"_", "EKE.nc")
        file_path <- paste(product_folder, file_name, sep = "/")
        writeRaster(p, filename = file_path, format = "CDF", overwrite = TRUE,
                    varname = output_var, longname = output_name, xname = "lon", yname = "lat")
      } else {
        warning(paste("Files not found:", file_path_u, file_path_v))
        return(NULL)
      }
      
    }, error = function(e) {
      warning(paste("Error processing date", surveyDates$date[i], ":", e$message))
      return(NULL)  # Return NULL for failed tasks
    })
  }
  
  # Stop cluster
  stopCluster(cl)
}
  
#-----------------------------------------------
# Set initial parameters: gradient
#-----------------------------------------------

# generate eddy kinetic energy gradient
head(catalog)
generate_eke_gradient(input_u_product_id = 1, input_v_product_id = 2, output_product_id = 3)


# check one:
#library(ncdf4)
#eg <- nc_open(paste0(cmems_repo, "/2021/05/07/20210507_EKE.nc"))
#library(stars)
#file_path <- paste0(cmems_repo, "/EKE/2020/06/18/EKE_2020-06-18.nc")
#egb <- read_stars(file_path)
#plot(eg)
#
#
#u <- 0.0430239
#v <- 0.173732
#p <- (u^2 + v^2) / 2
