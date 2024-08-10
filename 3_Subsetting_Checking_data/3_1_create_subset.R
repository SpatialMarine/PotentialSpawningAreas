# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 3.1. Create data subsets
#-------------------------------------------------------------------------------
library(dplyr)

#Load data
data <- read.csv("temp/data_2D_3D_dist_eke.csv", sep = ",") 
names(data)
head(data)

output_dir <- file.path(temp_data, paste0("data_subsets"))
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)}

genera <- unique(data$Genus)

# Loop through each genus
for (genus in genera) {
  # Filter data for the current genus
  genus_data <- data %>% 
    filter(Genus == genus)
  
  # Save the dataset to a CSV file
  output_file <- file.path(output_dir, paste0(genus, ".csv"))
  write.csv2(genus_data, file = output_file, row.names = FALSE)
}
