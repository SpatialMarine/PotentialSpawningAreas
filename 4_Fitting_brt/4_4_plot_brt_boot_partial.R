# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------
# 4.5. plot_brt_boot_partial          Plot BRT partial effects using bootstrap
#-------------------------------------------------------------------------------
library(gbm)
library(data.table)
library(dplyr)
library(egg)


bootstrap <- T
n_boot <- 100


genus <- "Raja" #"Raja" #"Scyliorhinus"
family <- "LN_laplace_Final" #bernuilli #LN_laplace_sinO2
type <- "_NKm2" #"_NKm2" "_PA" "only_P
mod_code <- "brt"
dataset <- "ALL" #ALL, train


# First species:
# 1. Set data repository-------------------------------------------------------
brtDir <- paste(output_data, mod_code, paste0(genus, type, "_", family), sep="/")
outdir <- paste(brtDir, "predict_boost", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import model full model
mod <- readRDS(paste0(brtDir, "/", genus, "_Nkm2.rds"))

# list of bootstrap models
outdir_bootstrap <- paste0(brtDir, "/bootstrap/", genus, type, "_", family)
boots_files <- list.files(outdir_bootstrap, full.names = T)

# batch import of bootstrap models
models <- lapply(boots_files, readRDS)
n_models <- length(models)

# make a list of values to predict per variable from a single model
n_res <- 100
gbm_list <- ggBRT::plot.gbm.4list(models[[1]], continuous.resolution = n_res)

# get predictor names
pred.names <- models[[1]]$var.names
n_var <- length(pred.names)

# create empty matrix to store data
boot_mat <- array(NA, dim=c(n_res, n_var, n_boot))

for(i in 1:length(models)){
  # get model
  mi <- models[[i]]
  
  # predict values for list of values
  ipred_boot <- ggBRT::plot.gbm.boot(mi, list.4.preds = gbm_list, continuous.resolution = n_res)
  
  # append data
  #boot_predicts[[i]] <- ipred_boot
  boot_mat[,,i] <- ipred_boot
}


# calculate median and CI per variable
boot_med <- apply(boot_mat, c(1,2), median, na.rm=T)
boot_cil <- apply(boot_mat, c(1,2), quantile, prob = 0.025, na.rm=T)
boot_ciu <- apply(boot_mat, c(1,2), quantile, prob = 0.975, na.rm=T)

# create a data.frame
data_list <- list()
for(i in 1:n_var){
  
  # create data.frame
  idf <- data.frame(
    var = pred.names[i],
    xval = gbm_list[[i]]$X1,
    med = boot_med[,i],
    cil = boot_cil[,i],
    ciu = boot_ciu[,i]
  )
  #append
  data_list[[i]] <- idf
}



# combine data
data <- rbindlist(data_list)

# relative importance
data$var <- factor(data$var, levels = mod$contributions$var)
relinf <- round(mod$contributions$rel.inf, 1)
labels <- paste0(mod$contributions$var, " (", relinf, "%)")
names(labels) <- mod$contributions$var

# select number of variables to plot
n_plots <- 10
data2 <- filter(data, var %in% mod$contributions$var[1:n_plots])


# plot: #orange for S canicula and #steelbluefor G melastomus
p_skates <- ggplot(data2, aes(x = xval)) +
  geom_ribbon(aes(ymin = cil, ymax = ciu), fill="steelblue", alpha=.2, linetype=0) +
  geom_line(aes(y = med), color="steelblue") +
  ylab("Fitted function") + xlab("") +
  facet_wrap(var~., scales = "free_x", ncol =2, strip.position = "bottom", labeller=labeller(var=labels)) +
  theme_article(base_size = 14) +
  theme(
    strip.placement = "outside",
    plot.margin = unit(c(10,10,10,10), "points"),
    axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0))
  )

p_skates

# export plot
outfile <- paste0(outdir, "/", genus, "_", mod_code, "_", family, "_response_boot_c.png")
ggsave(outfile, p, width=17, height=13, units="cm", dpi=300)





# Second species:
genus <- "Scyliorhinus" #"Raja" #"Scyliorhinus"

# 1. Set data repository-------------------------------------------------------
brtDir <- paste(output_data, mod_code, paste0(genus, type, "_", family), sep="/")
outdir <- paste(brtDir, "predict_boost", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import model full model
mod <- readRDS(paste0(brtDir, "/", genus, "_Nkm2.rds"))

# list of bootstrap models
outdir_bootstrap <- paste0(brtDir, "/bootstrap/", genus, type, "_", family)
boots_files <- list.files(outdir_bootstrap, full.names = T)

# batch import of bootstrap models
models <- lapply(boots_files, readRDS)
n_models <- length(models)

# make a list of values to predict per variable from a single model
n_res <- 100
gbm_list <- ggBRT::plot.gbm.4list(models[[1]], continuous.resolution = n_res)

# get predictor names
pred.names <- models[[1]]$var.names
n_var <- length(pred.names)

# create empty matrix to store data
boot_mat <- array(NA, dim=c(n_res, n_var, n_boot))

for(i in 1:length(models)){
  # get model
  mi <- models[[i]]
  
  # predict values for list of values
  ipred_boot <- ggBRT::plot.gbm.boot(mi, list.4.preds = gbm_list, continuous.resolution = n_res)
  
  # append data
  #boot_predicts[[i]] <- ipred_boot
  boot_mat[,,i] <- ipred_boot
}


# calculate median and CI per variable
boot_med <- apply(boot_mat, c(1,2), median, na.rm=T)
boot_cil <- apply(boot_mat, c(1,2), quantile, prob = 0.025, na.rm=T)
boot_ciu <- apply(boot_mat, c(1,2), quantile, prob = 0.975, na.rm=T)

# create a data.frame
data_list <- list()
for(i in 1:n_var){
  
  # create data.frame
  idf <- data.frame(
    var = pred.names[i],
    xval = gbm_list[[i]]$X1,
    med = boot_med[,i],
    cil = boot_cil[,i],
    ciu = boot_ciu[,i]
  )
  #append
  data_list[[i]] <- idf
}



# combine data
data <- rbindlist(data_list)

# relative importance
data$var <- factor(data$var, levels = mod$contributions$var)
relinf <- round(mod$contributions$rel.inf, 1)
labels <- paste0(mod$contributions$var, " (", relinf, "%)")
names(labels) <- mod$contributions$var

# select number of variables to plot
n_plots <- 10
data3 <- filter(data, var %in% mod$contributions$var[1:n_plots])


# plot: #orange for S canicula and #steelbluefor G melastomus
p_catsharks <- ggplot(data3, aes(x = xval)) +
  geom_ribbon(aes(ymin = cil, ymax = ciu), fill="steelblue", alpha=.2, linetype=0) +
  geom_line(aes(y = med), color="steelblue") +
  ylab("Fitted function") + xlab("") +
  facet_wrap(var~., scales = "free_x", ncol =2, strip.position = "bottom", labeller=labeller(var=labels)) +
  theme_article(base_size = 14) +
  theme(
    strip.placement = "outside",
    plot.margin = unit(c(10,10,10,10), "points"),
    axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0))
  )

p_catsharks





# Combine both:
# Step 1: Combine the datasets, with an additional 'species' column to differentiate skates and catsharks
data2$species <- "Skates"      # Skates data2
data3$species <- "Catsharks"   # Catsharks data3

# Combine the two datasets into a single dataframe
combined_data <- rbind(data2, data3)

# Step 2: Define the plot with ggplot
combined_plot <- ggplot(combined_data, aes(x = xval, y = med, color = species)) +
  
  # Add confidence ribbon with different colors for each species
  geom_ribbon(aes(ymin = cil, ymax = ciu, fill = species), alpha = 0.2, linetype = 0) +
  
  # Add the fitted line for each species
  geom_line(size = 1) +
  
  # Facet wrap by predictor variable (var), as done before
  facet_wrap(var~., scales = "free_x", ncol = 2, strip.position = "bottom", labeller = labeller(var = labels)) +
  
  # Set axis labels
  ylab("Fitted function") + xlab("") +
  
  # Theme settings
  theme_article(base_size = 14) +
  theme(
    strip.placement = "outside",
    plot.margin = unit(c(10, 10, 10, 10), "points"),
    axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
    legend.position = "right"  # Optional: place the legend on the right
  ) +
  
  # Define colors for species
  scale_color_manual(values = c("Skates" = "steelblue", "Catsharks" = "orange")) + 
  scale_fill_manual(values = c("Skates" = "steelblue", "Catsharks" = "orange")) #"#3CB371", #2E8B57


# Step 3: Print the combined plot
print(combined_plot)

# export plot
outfile <- paste0(outdir, "/combined_", mod_code, "_", family, "_response_boot_c.png")
ggsave(outfile, combined_plot, width=17, height=13, units="cm", dpi=300)
