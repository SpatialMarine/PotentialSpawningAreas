# ------------------------------------------------------------------------------

# Title:

#Analysis objective 1:
# Assessing the environmental and fishing human presures parameters affecting to 
# the distribution of elasmobranch egg cases

#-------------------------------------------------------------------------------
# 4.3. Fit Boosted Regression Tree model for density data (N/km2)
#-------------------------------------------------------------------------------

library(doParallel)
library(ggBRT)
library(lubridate)
library(data.table)
library(egg)
library(fmsb)

genus <- "Scyliorhinus" #Raja

#Load data
file <- paste0(temp_data, "/folds_dataset/", genus, "_folds_dataset.csv")
data <- read.csv2(file)

names(data)
str(data)


#1. Select only density data ---------------------------------------------------
#Chose response variable distribution: 
hist(data$N_km2)
shapiro.test(data$N_km2)

#filter non 0 values:
data <- data %>% filter(N_km2 > 0)
hist(data$N_km2)
shapiro.test(data$N_km2)

#transform response variable:
data$ln_N_km2 <- log(data2$N_km2)
hist(data$ln_N_km2)
shapiro.test(data$ln_N_km2)
names(data)






#2. Organise dataset -----------------------------------------------------------
# Change the name of some variables as you want them to appear in the figure for the paper:
colnames(data) <- c("Haul_N", "code", "Genus", "lat", "lon", "season", "depth", 
                    "swept_area_km2", "N", "N_km2", "presence_absence", "date", 
                    "date_time", "bathy", "substrate", "slope", "roughness", 
                    "fishingEffort", "distCanyons", "distMounts", "distFans", 
                    "bottom_temp", "Year", "Month", "Day", "bottom_oxygen", 
                    "bottom_nppv", "bottom_ph", "bottom_nh4", "bottom_no3", 
                    "bottom_po4", "bottom_so", "bottom_uo", "bottom_vo", 
                    "bottom_eke", "RN", "id", "fold", "ln_N_km2")

# Convert the 'time' column to Date format if needed 
data$date <- as.Date(data$date) #, format = "%Y-%m-%d"
data$date_time <- as.POSIXct(data$date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Create a season column:
data$season <- case_when(
  (month(data$date) == 12 & day(data$date) >= 21) | (month(data$date) %in% c(1, 2)) | 
    (month(data$date) == 3 & day(data$date) < 21) ~ 1,  # Winter: Dec 21 - Mar 20
  
  (month(data$date) == 3 & day(data$date) >= 21) | (month(data$date) %in% c(4, 5)) | 
    (month(data$date) == 6 & day(data$date) < 21) ~ 2,  # Spring: Mar 21 - Jun 20
  
  (month(data$date) == 6 & day(data$date) >= 21) | (month(data$date) %in% c(7, 8)) | 
    (month(data$date) == 9 & day(data$date) < 21) ~ 3,  # Summer: Jun 21 - Sep 20
  
  (month(data$date) == 9 & day(data$date) >= 21) | (month(data$date) %in% c(10, 11)) | 
    (month(data$date) == 12 & day(data$date) < 21) ~ 4 )  # Autumn: Sep 21 - Dec 20

#Set categorical predictors as categories:
data <- data %>% 
  mutate(season = factor(season, c(1:4)),
         fold = factor(data$fold)) #Haul_N = factor(data$Haul_N),

summary(data)
str(data)

# List the name of the predictor variables
vars  <- c("season", "depth", "slope", "fishingEffort",
           "distCanyons", "distMounts", "distFans", 
           "bottom_temp", "bottom_oxygen", "bottom_nppv", "bottom_ph", 
           "bottom_nh4", "bottom_so", "bottom_eke", "RN")

