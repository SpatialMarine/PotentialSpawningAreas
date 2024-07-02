#---------------------------------------------------------------------------------------------------
# Create training and testing data sets:
#---------------------------------------------------------------------------------------------------
# It may not be interesting creating a training and testing if data is limited.
# Especially when you will check the model using cross-validation and bootstrap. 
# You can create and then decide whether you will use it or not.
# In my case I won't use it as I am already using cross-validation.
sp_code <- "Sca" #Sca, Gme, Esp, Tma

#Load data
wd<- paste0(output_data, "/data_subsets")
setwd(wd)
data <- read.csv("Scanicula.csv", sep = ";") #Scanicula #Gmelastomus #Espinax #Tmarmorata
names(data)
head(data)

#Set categorical predictors as categories:
data <- data %>% 
  mutate(Haul_N = factor(data$Haul_N),
         Alive_Dead = factor(Alive_Dead, c(0, 1)),
         Sex = factor(Sex, c(0, 1)),
         subs = factor(subs, c(1:2)),
         Season = factor(Season, c(1:4)),
         Maturity = factor(Maturity, c(0:1)),
         Pregnancy = factor(Pregnancy, c(0:1)),
         Abortion = factor(Abortion, c(0:1)))

#Set categorical predictors as categories:
data <- data %>%
  mutate(TL = as.numeric(gsub(",", ".", TL)),  # Convert comma decimal to dot decimal
         DW = as.numeric(gsub(",", ".", DW)),
         lon = as.numeric(gsub(",", ".", lon)),
         lat = as.numeric(gsub(",", ".", lat)),
         Cloud.cover = as.numeric(gsub(",", ".", Cloud.cover)),
         MinsExposedtoAir = as.numeric(gsub(",", ".", MinsExposedtoAir)),
         Average_speed = as.numeric(gsub(",", ".", Average_speed)),
         TotalBiomassHaul = as.numeric(gsub(",", ".", TotalBiomassHaul)),
         Trawl_duration = as.numeric(gsub(",", ".", Trawl_duration)),
         Wind.strength = as.numeric(gsub(",", ".", Wind.strength)),
         Sea.state = as.numeric(gsub(",", ".", Sea.state)),
         Distance_covered_GPS = as.numeric(gsub(",", ".", Distance_covered_GPS)),
         Activity = as.numeric(gsub(",", ".", Activity)),
         Wunds = as.numeric(gsub(",", ".", Wunds)),
         Brusing = as.numeric(gsub(",", ".", Brusing)),
         Ectoparasites = as.numeric(gsub(",", ".", Ectoparasites)),
         at_celsius = as.numeric(gsub(",", ".", at_celsius)),
         sst_celsius = as.numeric(gsub(",", ".", sst_celsius)),
         sbt_reanalysis = as.numeric(gsub(",", ".", sbt_reanalysis)),
         diff_at_sbt = as.numeric(gsub(",", ".", diff_at_sbt)),
         diff_sst_sbt = as.numeric(gsub(",", ".", diff_sst_sbt)),
         depth = as.numeric(gsub(",", ".", depth)),
         #sso_merged = as.numeric(gsub(",", ".", sso_merged)),
         #sbo_merged = as.numeric(gsub(",", ".", sbo_merged)),
         #diff_sso_sbo = as.numeric(gsub(",", ".", diff_sso_sbo)),
         #SSSAL_merged = as.numeric(gsub(",", ".", SSSAL_merged)),
         #SBSAL_merged = as.numeric(gsub(",", ".", SBSAL_merged)),
         #diff_SSSAL_SBSAL = as.numeric(gsub(",", ".", diff_SSSAL_SBSAL)),
         #SSph_merged = as.numeric(gsub(",", ".", SSph_merged)),
         #SBph_merged = as.numeric(gsub(",", ".", SBph_merged)),
         #diff_SSph_SBph = as.numeric(gsub(",", ".", diff_SSph_SBph))
  )

# Convert the 'time' column to Date format if needed 
data$date <- as.Date(data$date) #, format = "%Y-%m-%d"
data$time_hours <- as.POSIXct(data$time_hours, format = "%d-%m-%y %H:%M", tz = "UTC")

summary(data)

# Before splitting it, create a Random Number which must be present in both training and testing datasets

# Generate Random Number (from 1 to 100) and an ID column for each row (i.e. specimen)
# RN will serve as an indicator for variables that have influence greater or less than 
# random (Scales et al., 2017; Soykan, Eguchi, Kohin, & Dewar, 2014);
# only variables with influence greater than the random number were included in the final models.

data$RN <- sample.int(100, size=nrow(data), replace=T, prob=NULL)
data <- data %>%
  mutate(id = seq_along(RN))
head(data)

# Split the data set into train and test sets. You can do it either:
# (1) dividing the data randomly (usually 70%; 30%, if data is limited, you may use cross validation instead):
# Set the seed for reproducibility#set.seed(123)
#train_frac <- 0.7
#train_indices <- sample(1:nrow(data), size = train_frac * nrow(data))
#train_data <- data[train_indices, ]
#test_data <- data[-train_indices, ]
#head(train_data)
#names(test_data)

# (2) or, as in this case, separating certain categories within your group ("random effect") factor. 
# In this case this factor is "Haul_N" (i.e. the fishing operation). 
# Count the occurrences of each value in the factor column
value_counts <- table(data$Haul_N)
# Print the value counts
print(value_counts)
# Create a bar plot
barplot(value_counts, main="Value Counts Bar Chart", xlab="Value", ylab="Count")
#You can check that it is correct:
total_count_sum <- sum(value_counts)

# Set the seed for reproducibility
set.seed(132)
# Calculate the total number of unique Haul_N values
unique_hauls <- unique(data$Haul_N)
num_unique_hauls <- length(unique_hauls)
# Calculate the number of hauls for training and testing based on your desired split
train_frac <- 0.7
num_train_hauls <- round(train_frac * num_unique_hauls)
num_test_hauls <- num_unique_hauls - num_train_hauls
# Randomly select hauls for training and testing
train_hauls <- sample(unique_hauls, size = num_train_hauls)
test_hauls <- setdiff(unique_hauls, train_hauls)
# Filter the data based on the selected hauls
train_data <- data[data$Haul_N %in% train_hauls, ]
test_data <- data[data$Haul_N %in% test_hauls, ]
# Print the number of rows in each dataset
ntrain<-nrow(train_data)
ntest<-nrow(test_data)
#Calculate percentages:
ntot<-ntrain+ntest
percetange_train<-ntrain*100/ntot
percetange_test<-ntest*100/ntot

# Print the unique hauls present in each dataset
unique(train_data$Haul_N)
unique(test_data$Haul_N)

#Check it all if needed:
View(train_data)
View(test_data)

#Create folder to save them:
output_dir <- file.path(output_data, paste0("training_testing"))
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)}

#Save them:
output_file <- file.path(output_dir, paste0(sp_code,"_train_dataset.csv"))
write.csv2(train_data, file = output_file, row.names = FALSE)

output_file <- file.path(output_dir, paste0(sp_code,"_test_dataset.csv"))
write.csv2(test_data, file = output_file, row.names = FALSE)
