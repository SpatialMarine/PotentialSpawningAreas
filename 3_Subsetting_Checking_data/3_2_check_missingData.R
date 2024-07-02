# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 3.2. Check missing data in predictors
#-------------------------------------------------------------------------------

genus <- "Scyliorhinus" #Raja

#Load data
file <- paste0(temp_data, "/data_subsets/", genus, ".csv")
data <- read.csv2(file)

names(data)
str(data)

#---------------------------------------------------------------------------------------------------
# Select predictors            
#---------------------------------------------------------------------------------------------------
#Create function for plot how many missing values are there in a particular column (taken from dmarch github: https://github.com/dmarch/agazella):
plot_Missing <- function(data_in, title = NULL){
  # https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("grey80", "grey10"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}

# set names of the environmental variables
vars <- c("subs", "slope", "bottomT", "distance_to_seamount", "distance_to_canyons", 
          "distance_to_fans", "depth", "seabottom_o2", "seabottom_nppv", 
          "seabottom_ph", "seabottom_nh4", "seabottom_no3", "seabottom_po4", "seabottom_so")

# Select columns with environmental data
selEnv <- data %>% dplyr::select(all_of(vars))
# Plot missing data for environmental variables and save it:
#Create folder to save it:
output_dir <- file.path(output_data, paste0("prefitting_checks/", genus))
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)}

#Plot and save:
setwd(output_dir)
jpeg(file = "MissingData.jpeg", 
     width = 23.8, height = 21.65, units = "cm", res = 300)
plot_Missing(selEnv) 
dev.off()

#Calculate percentage of NA in your predictors:
#TotalBiomass:
na_count <- sum(is.na(data$TotalBiomassHaul))
na_count
total_observations <- length(data$TotalBiomassHaul)
percentage <- (na_count / total_observations) * 100
percentage #16.15% NA for TotalBiomassHaul Sca; 23.01% for Gme; 13.18 for Esp; 15.93% for Tma

#SAL (diff_SSSAL_SBSAL; SBSAL_merged; SSSAL_merged):
#na_count <- sum(is.na(data$diff_SSSAL_SBSAL))
#na_count
#total_observations <- length(data$diff_SSSAL_SBSAL)
#percentage <- (na_count / total_observations) * 100
#percentage #10.49% NA for all SAL (diff_SSSAL_SBSAL; SBSAL_merged; SSSAL_merged) Sca; 11.80% for Gme ; 11.63% for Esp; 0.88% for Tma
