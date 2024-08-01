#---------------------------------------------------------------------------------------------------
# Optimization of BRT hyper-parameters (see Appendix 1 for a summary of Elith et al. 2008 recommendations)
#---------------------------------------------------------------------------------------------------

# --- --- --- --- ---
#Analysis objective 1:
# --- --- --- --- ---
#Assessing the biological, environmental, and fishing-related parameters affecting to the at-vessel survival probability of chondrichthyan

#Analysis 1: Species-specific
#Response variable: proportion that is alive before releasing: binomial distribution (0 or 1). 
#Predictors:  3 different types: Biological (TL, sex), Environmental (diff_at_sbt, Cloud.cover, Sea.state), 
#and fishing-related (MinsExposedtoAir, Average_speed, TotalBiomassHaul, Trawl_duration)

sp_code <- "Sca" #Sca, Gme, Esp, Tma

#Load data
wd<- paste0(output_data, "/folds_dataset")
setwd(wd)
data <- read.csv("Sca_folds_dataset.csv", sep = ";") #Sca_folds_dataset.csv, Gme_folds_dataset.csv, Esp_folds_dataset.csv, Tma_folds_dataset.csv
names(data)
head(data)

# Actually results are more easily understood when mortality = 1 and survival = 0. 
# Thus, let's swipe those values.
data$Alive_Dead <- ifelse(data$Alive_Dead == 1, 0, 1)

#Set categorical predictors as categories:
data <- data %>% 
  mutate(Sex = factor(Sex, c(0, 1)),
         subs = factor(subs, c(1:2)),
         Season = factor(Season, c(1:4)),
         Maturity = factor(Maturity, c(0:1)),
         Pregnancy = factor(Pregnancy, c(0:1)),
         Abortion = factor(Abortion, c(0:1)),
         fold = factor(data$fold)) #Haul_N = factor(data$Haul_N),

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
str(data)

#List the name of the predictor variables
vars  <- c("TL", "Sex", "Maturity", "Cloud.cover", "Wind.strength", "MinsExposedtoAir", 
           "Average_speed", "Trawl_duration",  "subs", "RN", "TotalBiomassHaul",        
           "depth",  "at_celsius")  

#All: "diff_SSSAL_SBSAL", "diff_SSph_SBph", , "diff_sso_sbo"
#Gme, Esp, Tma: "Wind.strength",

# Define number of trees
ini.nt = 50
max.nt = 10000
step.nt = 50 #des pas de 50
tree.list <- seq(ini.nt,max.nt,by=step.nt) #list of trees for evaluation

# Define combination of hyper-parameters
comb <- expand.grid(lr=c(0.001, 0.005, 0.01, 0.05), tc=c(1,3,5), bf=c(0.5, 0.6, 0.7)) #combination
#comb <- expand.grid(lr=c(0.001, 0.004, 0.02, 0.05), tc=c(1,3,4), bf=c(0.4, 0.5, 0.6)) #combination
#comb <- expand.grid(lr=c(0.001, 0.01, 0.02, 0.05), tc=c(1,2,3), bf=c(0.5, 0.6, 0.7)) #combination
#comb <- expand.grid(lr=c(0.001, 0.01, 0.1, 0.0005), tc=c(5,7,10), bf=c(0.5, 0.6, 0.7)) #combination

## Prepare clusters
cores <-detectCores()
cores #if you use all of them you, your computer may crash (consumes all the CPU).
cores <- 6  
cl <- makeCluster(cores)
registerDoParallel(cl)

#Chose response variable distribution: 
#hist(data$PRM_prob)
#shapiro.test(data$PRM_prob)

#  Create output data repository
outdir <- paste0(output_data, "/brt/", sp_code)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

set.seed(131)

all_list <- foreach(i=1:nrow(comb), .packages=c("dismo", "gbm", "dplyr")) %dopar% {
  
  # Fit model
  # Uses a block cross-validation
  # faster learning rate means larger values
  mod <- #tryCatch(
    dismo::gbm.step(data = data,             # data.frame with data
                    gbm.x = vars,          # predictor variables
                    gbm.y = "Alive_Dead",            # response variable
                    family = "bernoulli",  # the nature of error structure
                    tree.complexity = comb$tc[i],   # tree complexity
                    learning.rate = comb$lr[i],  # learning rate
                    bag.fraction = comb$bf[i],    # bag fraction
                    fold.vector = data$fold,
                    n.folds = length(unique(data$fold)),
                    n.trees = ini.nt, 
                    step.size = step.nt, 
                    max.trees = max.nt)   
    #, error = function(e) return(NULL))

  
  if(!is.null(mod)) {
    # Keep CV parameters
    mod_out <- data.frame(
      tree.complexity = mod$interaction.depth,
      learning.rate = mod$shrinkage,
      bag.fraction = mod$bag.fraction,
      n.trees = mod$n.trees,
      AUC = mod$self.statistics$discrimination,
      cv.AUC = mod$cv.statistics$discrimination.mean,
      deviance = mod$self.statistics$mean.resid,
      cv.deviance = mod$cv.statistics$deviance.mean,
      PER = (1-mod$self.statistics$mean.resid/mod$self.statistics$mean.null)*100,
      cv.PER = (1-mod$cv.statistics$deviance.mean/mod$self.statistics$mean.null)*100
    ) 
    
    # keep deviance values for all trees
    cv_deviance <- mod$cv.values
    cv_deviance <- c(cv_deviance, rep(NA, length(tree.list) - length(cv_deviance)))  #fill with NA
    
    # selected variables
    pred_order <- summary(mod)$var
    rn_position <- which(pred_order == "RN")
    pred_list <- as.character(pred_order[1:(rn_position-1)])
    
    
    list(mod_out = mod_out, cv_deviance = cv_deviance, pred_list = pred_list)
  } else {
    # Return NULL or an empty list if mod is NULL
    list(mod_out = NULL, cv_deviance = NULL, pred_list = NULL)
  }
  }
  
## combine model outputs
mod_list <- foreach(i=1:nrow(comb)) %dopar% all_list[[i]]$mod_out
mod_list[!lengths(mod_list)] <-  list(data.frame(tree.complexity = NA))
mod_out <- rbindlist(mod_list, fill = TRUE)
mod_out <- bind_cols(comb,
                     dplyr::select(mod_out, -c(tree.complexity, learning.rate, bag.fraction))) %>%
  dplyr::mutate(id = 1:n())

## combine deviance outputs
deviance_list <- list()
for(i in 1:nrow(mod_out)){
  # extract deviance data
  dev <- all_list[[i]]$cv_deviance
  
  # check that there is no null data
  if(is.null(dev)) dev <- rep(NA,length(tree.list))
  
  # make data.frame with number of trees
  df <- data.frame(id=mod_out$id[i],lr = mod_out$lr[i], tc = mod_out$tc[i], bf = mod_out$bf[i], ntrees = tree.list, cv_deviance = dev)
  deviance_list[[i]] <- df
}
cv_deviance <- rbindlist(deviance_list)

## get selected variables
predict_list <- foreach(i=1:nrow(comb)) %dopar% all_list[[i]]$pred_list

## stop clusters
stopCluster(cl)

## plot profiles
p <- ggplot(data = cv_deviance) +
  geom_line(data = dplyr::rename(cv_deviance, comb = id), aes(x = ntrees, y = cv_deviance, group = comb), color = "grey80") +
  geom_line(aes(x = ntrees, y = cv_deviance, group = id), color = "firebrick3") +
  scale_x_continuous(limits = c(0, max(cv_deviance$ntrees[!is.na(cv_deviance$cv_deviance)]))) +
  facet_wrap(id ~.,) +
  theme_article()

mod_code <- "brt"
outdir <- paste0(output_data, "/brt/", sp_code)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

#  Create output repository
outfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_", "optim_params.png")
ggsave(outfile, p, width=25, height=14, units="cm", dpi=300)

## export outputs
outfile <- paste0(outdir, "/brt_optim_params.csv")
write.csv2(mod_out, outfile, row.names = FALSE)

outfile <- paste0(outdir, "/brt_cv_deviance.csv")
write.csv(cv_deviance, outfile, row.names = FALSE)

outfile <- paste0(outdir, "/brt_predlist.rds")
saveRDS(predict_list, outfile)

#-----------------------------------------------------------------
# Boosted Regression Tree - Fit full model
#-----------------------------------------------------------------

#Open the created dataset containing the models information and select the best model.
mod_code <- "brt"
#sp_code <- Gme, Sca, Esp, Tma

outdir <- paste0(output_data, "/brt/", sp_code)
mod_out <- read.csv2(paste0(outdir, "/brt_optim_params.csv"))
predict_list <- readRDS(paste0(outdir, "/brt_predlist.rds"))

View(mod_out)
View(predict_list)

#Save as excel to build the table for the paper:
#library(openxlsx)
#outfile <- paste0(outdir, "/brt_optim_params.xlsx")
#write.xlsx(mod_out, outfile, rowNames = FALSE)

#' selection is based on parameters, criteria and checking curves (see appendix 1 for Elith et al. 2008 recommendations)
#' But as summary:
#' 1) The model with the lowest cv_deviance which n.trees is >1000
#' 2) Then, if there are two or more very similar: the one with the highest AUC
#' 3) Then, if there are two or more very similar: the one with the largest nt, lt and tc.

select_model_id <- 21 #Sca at = 21 (at_sbt: 33, Sca (sbt_sst): 27); Gme at = 2 (in both cases): 2); Esp: none; Tma: 25

#List the name of the predictor variables
vars  <- c("TL", "Sex", "Maturity", "Cloud.cover", "Wind.strength", "MinsExposedtoAir", 
           "Average_speed", "Trawl_duration",  "subs", "RN", "TotalBiomassHaul",        
           "depth",  "at_celsius")  #"diff_sso_sbo", diff_at_sbt

tc <- mod_out$tc[select_model_id]
lr <- mod_out$lr[select_model_id]
bf <- mod_out$bf[select_model_id]
ntrees <- mod_out$n.trees[select_model_id]
pred_list <- vars[vars %in% predict_list[[select_model_id]]]

# remove variables not selected
# fir BRT with selected parameters
mod_full <- dismo::gbm.fixed(data = data,             # data.frame with data
                             gbm.x = pred_list,          # predictor variables
                             gbm.y = "Alive_Dead",            # response variable
                             family = "bernoulli",  # the nature of errror structure
                             tree.complexity = tc,   # tree complexity
                             learning.rate = lr,  # learning rate
                             bag.fraction = bf,    # bag fraction
                             n.trees = ntrees) 

# Save model
saveRDS(mod_full, file = paste0(outdir, "/", sp_code, ".rds"))  # save model
mod_full <- readRDS(paste0(outdir, "/", sp_code, ".rds"))

#-----------------------------------------------------------------------------------
# radarPlot       Radar plot of variable contribution from models
#-----------------------------------------------------------------------------------
radarPlot <- function(var_imp, var_order, colors_border=rgb(0.2,0.5,0.5,0.9), colors_in=rgb(0.2,0.5,0.5,0.4)){
  
  # set parameters for plot
  min_val <- 0
  max_val <- ceiling(max(var_imp))#100
  nseg <- 4
  
  # prepare data.frame
  var_imp <- dplyr::select(data.frame(t(var_imp)), var_order)
  data <- rbind(rep(max_val, length(var_imp)) , rep(min_val, length(var_imp)), var_imp)
  data <- data.frame(data)
  row.names(data) <- c("max", "min", "MaxEnt")
  
  radarchart(data  , axistype=1 , 
             #custom polygon
             pcol=colors_border , pfcol=colors_in , plwd=3 , plty=1,
             # number of segments
             seg=nseg,
             #custom the grid
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=round(seq(min_val,max_val,max_val/nseg),1), cglwd=0.8,
             #custom labels
             vlcex=1)
}

# Plot variable contribution using radar plot
var_imp <- summary(mod_full)$rel.inf
names(var_imp) <- summary(mod_full)$var
asc <- names(var_imp)
pngfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_var_radar.png")
png(pngfile, width=1500, height=1000, res=150)
radarPlot(var_imp, var_order=asc)
dev.off()

# Plot variable contribution using bar plot
pngfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_var_influence.png")
png(pngfile, width=1000, height=1000, res=150)
ggBRT::ggInfluence(mod_full, show.signif = F, col.bar = "skyblue3")
dev.off()

# Plot response curves
pngfile <- paste0(outdir, "/", sp_code, "_", mod_code, "_response.png")
png(pngfile, width=1000, height=2000, res=200)
names(mod_full$gbm.call)[1] <- "dataframe"
ggBRT::ggPD(mod_full, n.plots =13, smooth = F, rug = F, ncol=2, col.line = "skyblue3")
dev.off()

#-----------------------------------------------------------------
# Boosted Regression Tree - Interactions
#-----------------------------------------------------------------
#* Check whether there are important interactions among variables.
#* for that purpose you assess the magnitude of 2nd order interaction effects in gbm models fitted with interaction depths greater than 1.
names(mod_full$gbm.call)[1] <- "dataframe"

find.int <- dismo::gbm.interactions(mod_full)
find.int$interactions

#See the list of potential interactions:
find.int$rank.list

outdir_interaction <- paste0(outdir, "/interactions")
if (!dir.exists(outdir_interaction)) dir.create(outdir_interaction, recursive = TRUE)

#Assess the level of interaction between the potential interactions:
#*A higher value (close to 1) suggests a stronger and potentially significant interaction between 
#*the two variables. A lower value (close to 0) would indicate a less significant interaction.

#*# Set the angle for the 3D plot
theta <- 320  # Adjust the azimuthal angle as desired
phi <- 40    # Adjust the polar angle as desired

#Plot:
pngfile <- paste0(outdir_interaction, "/", sp_code, "_", mod_code, "_interaction_1.png")
png(pngfile, width=1500, height=1500, res=200)
dismo::gbm.perspec(mod_full, 9, 4, theta = theta, phi = phi, smooth = 0.5)
dev.off()

#*# Set the angle for the 3D plot
theta <- 200  # Adjust the azimuthal angle as desired
phi <- 40    # Adjust the polar angle as desired

#Check how the correlation between the 2 variables occur in 3D (x=var1, z=var2, y=respuesta)
pngfile <- paste0(outdir_interaction, "/", sp_code, "_", mod_code, "_interaction_2.png")
png(pngfile, width=1500, height=1500, res=200)
dismo::gbm.perspec(mod_full, 4, 1, theta = theta, phi = phi, smooth = 0.5)
dev.off()

pngfile <- paste0(outdir_interaction, "/", sp_code, "_", mod_code, "_interaction_3.png")
png(pngfile, width=1500, height=1500, res=200)
dismo::gbm.perspec(mod_full, 9, 1, theta = theta, phi = phi, smooth = 0.5)
dev.off()

#dismo::gbm.perspec(mod_full, 7, 2)
#dismo::gbm.perspec(mod_full, 4, 2)

#-----------------------------------------------------------------
# Boosted Regression Tree - Predict (Bootstrap approach)
#-----------------------------------------------------------------
# For each  model, we fit the model 50 times (n.boot <- 50).
# For each of the 50 iterations, we used the parameter values chosen for the final model, but we sampled
# half the data (with replacement) to fit the model. (Hindell et al. 2020):
# idata <- stratified(data, c("Alive_Dead", "fold"), 0.5, replace = TRUE) 
# The variable you put here are: your response variable "Alive_dead", and the fold variable "N_Haul" in my case.

# Set output directory
# Each bootstrap model is stored here
outdir_bootstrap <- paste0(outdir, "/bootstrap/", sp_code)
if (!dir.exists(outdir_bootstrap)) dir.create(outdir_bootstrap, recursive = TRUE)

# Define number of bootstrap models
n.boot <- 100  # number of model fits

## Prepare clusters
cores <- 5
cl <- makeCluster(cores)
registerDoParallel(cl)

foreach(i=1:n.boot, .packages=c("dismo", "gbm", "dplyr", "splitstackshape", "stringr"), .combine = "c") %dopar% {
  
  # sampled half the data (with replacement) to fit the model (Hindell et al. 2020)
  idata <- stratified(data, c("Alive_Dead", "Haul_N"), 0.5, replace = TRUE) 
  
  # fit BRT
  mod_boot <- dismo::gbm.fixed(data = idata,             # data.frame with data
                               gbm.x = pred_list,          # predictor variables
                               gbm.y = "Alive_Dead",            # response variable
                               family = "bernoulli",  # the nature of error structure
                               tree.complexity = tc,   # tree complexity
                               learning.rate = lr,  # learning rate
                               bag.fraction = bf,    # bag fraction
                               n.trees = ntrees) 
    # store model
  outfile <- paste0(outdir_bootstrap, "/", str_pad(i, 2, pad = "0"), "_", sp_code, "_", mod_code, "_boot.rds")
  saveRDS(mod_boot, file = outfile)  # save model
  
  # Return something here if needed
  return(mod_boot)
}

## stop clusters
stopCluster(cl)

