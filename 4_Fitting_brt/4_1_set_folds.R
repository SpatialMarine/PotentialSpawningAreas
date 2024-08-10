# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 4.1. Set folds
#-------------------------------------------------------------------------------
library(dplyr)
library(groupdata2)

# As mentioned previously, I will use all data to fit the model and check the model using cross-validation,
# instead of a training and testing data sets.
genus <- "Scyliorhinus" #Raja

#Load data
file <- paste0(temp_data, "/data_subsets/", genus, ".csv")
data <- read.csv2(file)

names(data)
str(data)

# 1. Organise data -------------------------------------------------------------
# add a unique numerical value for each unique value of code (i.e. each tow)
data <- data %>%
  mutate(Haul_N = as.numeric(factor(code)))

# Generate Random Number (from 1 to 100) and an ID column for each row (i.e. tow)
# RN will serve as an indicator for variables that have influence greater or less than 
# random (Scales et al., 2017; Soykan, Eguchi, Kohin, & Dewar, 2014);
# only variables with influence greater than the random number were included in the final models.

# Set the seed for reproducibility
set.seed(132)
data$RN <- sample.int(100, size=nrow(data), replace=T, prob=NULL)
data <- data %>%
  mutate(id = seq_along(RN))
head(data)

# Set variables as their types:
# Set categorical predictors as categories:
data <- data %>% 
  mutate(Haul_N = factor(data$Haul_N))
str(data)

# Prepare folds
#* set number of folds
#* Create them based on the previously created N_Haul (group variable; similar to random factor) column.
#* This is to not split the data into groups for cross-validation (the model is fitted with 4 groups in this case
#* where there are 5 folds, and then it repeated every time leaving one of the groups out and calculating a deviance)
#* Thus, if you have a variable which groups the data, something like a random effect, you should respect it
#* when doing the groups. So in my case, it is the haul, I want to merge several hauls into one fold but making sure
#* that all the data from a haul is not splitted among folds.

# Set the number of folds
n.folds <- 5

# Set the seed for reproducibility
set.seed(123)

#create folds
f <- fold(data = data, id_col = "Haul_N", method = "n_dist", k = n.folds) 

data <- f %>%
  dplyr::rename(fold = .folds) %>%
  dplyr::mutate(fold = as.numeric(fold)) %>%
  as.data.frame()

#Check that each fold has a similar number of samples/specimens (rows)
table(data$fold)

data %>%
  group_by(id, fold) %>%
  dplyr::summarize(n = n())

head(data)

#Create folder to save them:
output_dir <- file.path(temp_data, paste0("folds_dataset/"))
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)}

#Save data set:
output_file <- paste0(output_dir, "/", genus, "_folds_dataset.csv")
write.csv2(data, file = output_file, row.names = FALSE)

