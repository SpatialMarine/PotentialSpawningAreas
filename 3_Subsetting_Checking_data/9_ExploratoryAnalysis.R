#---------------------------------------------------------------------------------------------------
# Exploratory data analysis
#---------------------------------------------------------------------------------------------------
sp_code <- "Gme" #Sca, Gme, Esp, Tma

#Load data
wd<- paste0(output_data, "/data_subsets")
setwd(wd)
data <- read.csv("Gmelastomus.csv", sep = ";") #Scanicula #Gmelastomus #Espinax #Tmarmorata
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

#Create a type variable to explore the distribution of the data 
# of each variable in both observed and simulated
data$type <- NA
data$type[data$Alive_Dead == 1] <- "alive"
data$type[data$Alive_Dead == 0] <- "dead"
str(data)

#List the variables that you will use in the model:
vars  <- c("TL", "Sex", "Maturity", "Cloud.cover", "Sea.state", "MinsExposedtoAir", 
           "Average_speed", "TotalBiomassHaul", "Trawl_duration",  "subs",         
           "depth",  "diff_sst_sbt") 
#ALL: "diff_SSSAL_SBSAL", "diff_SSph_SBph", "Season"
#Gme, Esp, Tma: "Wind.strength", , "diff_sso_sbo"

# Create function for density.plot: 
# You will use this to create density plots of environmental data on alive and dead specimens:
density.plot <- function(title="", xlab="SST (ºC)", legend="", alpha=0.35, data=data, var=SST, group=type, cols = c("#d7191c", "#2c7bb6")){
  
  g <- ggplot(data, aes(x=var, color=group)) +
    geom_line(stat="density", linewidth = 1, alpha = 1.0) +
    scale_color_manual(values=cols) +
    labs(title = title, x = xlab, fill="") +
    theme_light() 
  return(g)
  
}

density.box_plot <- function(title = "", xlab = "SST (ºC)", legend = "", data = data, var = SST, group = type, cols = c("#d7191c", "#2c7bb6")) {
  
  g <- ggplot(data, aes(x = group, y = var, fill = group)) +
    geom_boxplot(width = 0.5, alpha = 0.7, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = cols) +
    labs(title = title, x = "", y = xlab) +
    theme_light() +
    theme(legend.title = element_blank())  # Hide the legend title
  
  return(g)
  
}

# This plot helps you to look at which variables may be more important in the model.
# The way you interpret this is: when there is not overlap between dead (absence) and alive (presence)
# then the variable may be important. This indicates that there is a preference towards certain values.
# create plot per variable

p1 <- density.plot(title = "", xlab = "TL", legend = "", data = data, var = data$TL, group = data$type)
p2 <- density.box_plot(title="", xlab="Sex", legend="", data=data, var=data$Sex, group=data$type)
p3 <- density.box_plot(title="", xlab="Maturity", legend="", data=data, var=data$Maturity, group=data$type)
p4 <- density.plot(title="", xlab="Cloud.cover", legend="", alpha=0.35, data=data, var=data$Cloud.cover, group=data$type)
p5 <- density.plot(title="", xlab="Sea.state", legend="", alpha=0.35, data=data, var=data$Sea.state, group=data$type)
#p6 <- density.plot(title="", xlab="Wind.strength", legend="", alpha=0.35, data=data, var=data$Wind.strength, group=data$type)
p7 <- density.plot(title="", xlab="MinsExposedtoAir", legend="", alpha=0.35, data=data, var=data$MinsExposedtoAir, group=data$type)
p8 <- density.plot(title="", xlab="Average_speed", legend="", alpha=0.35, data=data, var=data$Average_speed, group=data$type)
p9 <- density.plot(title="", xlab="TotalBiomassHaul", legend="", alpha=0.35, data=data, var=data$TotalBiomassHaul, group=data$type)
p10 <- density.plot(title="", xlab="Trawl_duration", legend="", alpha=0.35, data=data, var=data$Trawl_duration, group=data$type)
p11 <- density.box_plot(title="", xlab="subs", legend="", data=data, var=data$subs, group=data$type)
p12 <- density.plot(title="", xlab="depth", legend="", alpha=0.35, data=data, var=data$depth, group=data$type)
p13 <- density.plot(title="", xlab="diff_at_sbt", legend="", alpha=0.35, data=data, var=data$diff_at_sbt, group=data$type)
#p14 <- density.plot(title="", xlab="diff_sso_sbo", legend="", alpha=0.35, data=data, var=data$diff_sso_sbo, group=data$type)
#p15 <- density.plot(title="", xlab="diff_SSSAL_SBSAL", legend="", alpha=0.35, data=data, var=data$diff_SSSAL_SBSAL, group=data$type)
#p16 <- density.plot(title="", xlab="diff_SSph_SBph", legend="", alpha=0.35, data=data, var=data$diff_SSph_SBph, group=data$type)

# create layaout
lay <- rbind(c(1,2),
             c(3,4),
             c(5,6),
             c(7,8),
             c(9,10),
             c(11,12),
             c(13))#,
             #c(15,16))
p <- grid.arrange(p1, p2,p3,p4,
                  p5, p7, p8,p9,
                  p10, p11, p12, p13,
                  layout_matrix = lay) 
#all: p15, p16, 
#Gme, Esp: p6, 

#' Those variables in which the blue and the red curves are very similar
#' may not be good predictors of the response variable.

# Aspect to improve: for categorical variables use box plots instead of these curves as these won't provide you
# much information as they are planned for continuous variables. 

#Save plot
output_dir <- file.path(output_data, paste0("prefitting_checks/", sp_code))
setwd(output_dir)
jpeg(file = "DensityPlots.jpeg", 
     width = 25, height = 35, units = "cm", res = 300)
grid.draw(p)
dev.off()

