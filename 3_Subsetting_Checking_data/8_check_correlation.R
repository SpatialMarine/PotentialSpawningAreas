#-----------------------------------------------------------------
# Check correlation between predictors using pearson
#-----------------------------------------------------------------
sp_code <- "Sca" #Sca, Gme, Esp, Tma

#Load data
wd<- paste0(output_data, "/data_subsets")
setwd(wd)
data <- read.csv("Scanicula.csv", sep = ";") #Scanicula #Gmelastomus #Espinax #Tmarmorata #Scanicula_PRM #Gmelastomus_PRM
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

# Change the name of some variables as you want them to appear in the figure for the paper:
colnames(data) <- c("Species", "tripID", "lat", "lon", "organismID", "Haul_N", "hour", "Season",
                    "Alive_Dead", "TOWMASS", "TL", "DW", "Sex", "WeightLWR", "Maturity", "Pregnancy",
                    "Abortion", "Activity", "Wunds", "Brusing", "Ectoparasites", "SEASTATE",
                    "WIND", "CLOUD", "Metier", "Net_horizontal_opening", "Distance_covered_GPS",
                    "SPEED", "DUR", "Fishing_effort", "DECKTIME", "date",
                    "time_hours", "sbt_reanalysis", "ATEMP", "sst_celsius", "DEPTH", "subs",
                    "diff_sst_sbt", "DTEMP")

#"sso_merged", "sbo_merged", "diff_sso_sbo", "SSSAL_merged", "SBSAL_merged", "diff_SSSAL_SBSAL", "SSph_merged", "SBph_merged", "diff_SSph_SBph"

summary(data)

# Select specific columns from the data dataset in which you want to assess skewness
vars  <- c("TL", "SEASTATE", "CLOUD", "SPEED", "WIND",         
           "DUR", "DECKTIME", "DEPTH", "TOWMASS", "ATEMP", "DTEMP") #"Sex", "subs", "Season", "Maturity", "DIS", "DTEMP", , "ATEMP",

#"SSSAL_merged", "SBSAL_merged", "sst_celsius", "diff_at_sbt", 
# "sbt_merged", "diff_SSSAL_SBSAL", "diff_SSph_SBph", "diff_sso_sbo", 


# calculate correlations using Pearson
correlations <- cor(na.omit(dplyr::select(data, all_of(vars))), method="pearson")

## calculate p-values
#data_sub <- data[, vars]
# Loop through each pair of variables
#for (i in 1:(ncol(correlations) - 1)) {
#  for (j in (i + 1):ncol(correlations)) {
    # Extract the two variables
#    var1 <- data_sub[[i]]
#    var2 <- data_sub[[j]]
    
    # Check if both variables are numeric
#    if (is.numeric(var1) && is.numeric(var2)) {
      # Calculate correlation and p-value
#      cor_test <- cor.test(var1, var2)
      
      # Assign the p-value to the corresponding position in the matrix
#      p_values[i, j] <- cor_test$p.value
#    } else {
      # If any of the variables is not numeric, assign NA to the p-value
#      p_values[i, j] <- NA
#    }
#  }
#}
#print(p_values)

# plot correlations
output_dir<- paste0(output_data, "/prefitting_checks/", sp_code)
if (!file.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)}
pngfile <- paste0(output_dir, "/", sp_code, "_eda_corplot.png")
png(pngfile, width=2000, height=2000, res=300)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlations, method="color",col=col(200), tl.col = "black", order = "original", diag=FALSE, type="upper", 
         addCoef.col = "black") # Add coefficient of correlation
dev.off()

#Calculate a p-value in particular:
#cor.test(data_sub$TL, data_sub$DEPTH, method = "pearson")

# calcualate correlations using Spearman and clustering analysis
# plot: #A9D18E for S canicula and #F4B183 for G melastomus
pngfile <- paste0(output_dir, "/", sp_code, "_eda_cluster.png")
png(pngfile, width=2500, height=2000, res=300)
op <- par(mar=c(0.5,5,0.5,0.5))
v <- as.formula(paste("~", vars, collapse="+"))
plot(varclus(v, similarity=c("spearman"),data=data),cex=1.2) # plot cluster
abline(a=0.30,0,col="steelblue",lty=1,lwd=2)
par(op)
dev.off()

# If two variables are very correlated, there will be convergence. You must eliminate the correlated ones.
# Then you select based on your interest (more ecologic sense, something that you can explain better)
# Spearman is usually used for this. When the values is larger than 0.7 between two variables, you must select only one of this.
# All predictors are uncorrelated (Spearman correlations <0.7), therefore none of them is discarded.
# You may use VIF as well, but this is used on the model, rather than upon the variables itself. Here we want to look at variables it self, because we are using machine learning.
# VIF may be also only for linear relationships and Spearman for non-linear too, but we are not sure, should look for more info. But basically jut use Spearman.

#Make a selection eliminating those that are harder to explain or make less sense:
vars  <- c("TL", "Sea.state", "Cloud.cover", "Average_speed",           
           "Trawl_duration", "MinsExposedtoAir", "diff_sst_sbt", 
           "depth", "TotalBiomassHaul") #"Sex", "subs", "Season", "Maturity", "diff_sso_sbo",

#"Distance_covered_GPS","SSSAL_merged", "SBSAL_merged", "sst_celsius", "diff_at_sbt", 
# "at_celsius",  "sbt_merged", "diff_SSSAL_SBSAL", "diff_SSph_SBph", "Wind.strength", 

# calcualate correlations using Spearman and clustering analysis
pngfile <- paste0(output_dir, "/", sp_code, "_eda_cluster_final.png")
png(pngfile, width=2500, height=2000, res=300)
op <- par(mar=c(0.5,5,0.5,0.5))
v <- as.formula(paste("~", vars, collapse="+"))
plot(varclus(v, similarity=c("spearman"),data=data),cex=.8) # plot cluster
abline(a=0.30,0,col="grey70",lty=1,lwd=2)
par(op)
dev.off()
