# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 3.4. Exploratory data analysis
#-------------------------------------------------------------------------------

genus <- "Scyliorhinus" #Raja

#Load data
file <- paste0(temp_data, "/data_subsets/", genus, ".csv")
data <- read.csv2(file)

names(data)
str(data)

#List the variables that you will use in the model:
vars <- c("subs", "slope", "bottomT", "distance_to_seamount", "distance_to_canyons", 
          "distance_to_fans", "depth", "seabottom_o2", "seabottom_nppv", 
          "seabottom_ph", "seabottom_nh4", "seabottom_no3", "seabottom_po4", "seabottom_so")

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

