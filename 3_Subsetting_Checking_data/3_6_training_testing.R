# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 3.6. Create training and testing data sets:
#---------------------------------------------------------------------------------------------------
# It may not be interesting creating a training and testing if data is limited.
# Especially when you will check the model using cross-validation and bootstrap. 
# You can create and then decide whether you will use it or not.

# In my case I won't use it as I am already using cross-validation!!!!!!!!!!

genus <- "Scyliorhinus" #Raja

#Load data
file <- paste0(temp_data, "/data_subsets/", genus, ".csv")
data <- read.csv2(file)

names(data)
str(data)

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
