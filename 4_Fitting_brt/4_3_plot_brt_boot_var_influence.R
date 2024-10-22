# ------------------------------------------------------------------------------

# Title:

#-------------------------------------------------------------------------------#---------------------------------------------------------------------------------------------------
# plot_brt_boot_var_influence          Plot BRT variable relative incluence using bootstrap
#---------------------------------------------------------------------------------------------------
library(gbm)
library(data.table)
library(dplyr)
library(ggplot2)
library(egg)


# Raja
genus <- "Raja" #"Raja" #"Scyliorhinus"
family <- "LN_laplace_Final" #bernuilli #LN_laplace_sinO2
type <- "_NKm2" #"_NKm2" "_PA" "only_P
mod_code <- "brt"
dataset <- "ALL" #ALL, train


#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- paste(output_data, mod_code, paste0(genus, type, "_", family), sep="/")

outdir <- paste(indir, "predict_boost", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import model full model
mod <- readRDS(paste0(indir, "/", genus, "_Nkm2.rds"))

# list of bootstrap models
outdir_bootstrap <- paste0(indir, "/bootstrap/", genus, type, "_", family)
boots_files <- list.files(outdir_bootstrap, full.names = T)

# batch import of bootstrap models
models <- lapply(boots_files, readRDS)

# create empty list to store data
data_list <- list()

for(i in 1:length(models)){
  # get model
  #i=1
    mi <- models[[i]]
  #print(summary(mi))

  # get relative importance for variables
  df <- data.frame(boot = i, var = summary(mi)$var, rel.inf = summary(mi)$rel.inf)
  
  # append data
  data_list[[i]] <- df
}

# combine all data
data <- rbindlist(data_list)

# calculate median and CI per variable
data <- data %>%
  dplyr::group_by(var) %>%
  dplyr::summarize(median = median(rel.inf),
                   cil = quantile(rel.inf, prob = 0.025),
                   ciu = quantile(rel.inf, prob = 0.975)) %>%
  arrange(median)

# plot
# reorder factors for plot in descending order
data$var <- factor(data$var, levels = data$var)

# plot:
p_skates <- ggplot(data=data, mapping=aes(x=var, y=median, ymin=cil, ymax=ciu)) + 
  geom_pointrange(col="steelblue") +
  coord_flip() +
  ylab("Relative influence (%)") + xlab("") +
  theme_article(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(linewidth =.1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0))
  )

p_skates


# export plot
p_png <- paste0(outdir, "/", genus, "_", mod_code, "_", family, "_var_influence_boot_c.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)




#-------------------------------------------------------------------------------
# Repeat for Scyliorhinus
genus <- "Scyliorhinus" #"Raja" #"Scyliorhinus"


#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- paste(output_data, mod_code, paste0(genus, type, "_", family), sep="/")

outdir <- paste(indir, "predict_boost", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import model full model
mod <- readRDS(paste0(indir, "/", genus, "_Nkm2.rds"))

# list of bootstrap models
outdir_bootstrap <- paste0(indir, "/bootstrap/", genus, type, "_", family)
boots_files <- list.files(outdir_bootstrap, full.names = T)

# batch import of bootstrap models
models <- lapply(boots_files, readRDS)

# create empty list to store data
data_list <- list()

for(i in 1:length(models)){
  # get model
  #i=1
  mi <- models[[i]]
  #print(summary(mi))
  
  # get relative importance for variables
  df <- data.frame(boot = i, var = summary(mi)$var, rel.inf = summary(mi)$rel.inf)
  
  # append data
  data_list[[i]] <- df
}

# combine all data
data2 <- rbindlist(data_list)

# calculate median and CI per variable
data2 <- data2 %>%
  dplyr::group_by(var) %>%
  dplyr::summarize(median = median(rel.inf),
                   cil = quantile(rel.inf, prob = 0.025),
                   ciu = quantile(rel.inf, prob = 0.975)) %>%
  arrange(median)

# plot
# reorder factors for plot in descending order
data2$var <- factor(data2$var, levels = data2$var)

# plot:
p_catsharks <- ggplot(data=data2, mapping=aes(x=var, y=median, ymin=cil, ymax=ciu)) + 
  geom_pointrange(col="steelblue") +
  coord_flip() +
  ylab("Relative influence (%)") + xlab("") +
  theme_article(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(linewidth =.1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0))
  )

p_catsharks


# Combine both ------------------------------------------------------------------
# Assuming 'data' is for skates and 'data2' is for catsharks
data_combined <- rbind(
  data %>% mutate(species = "Skates"),
  data2 %>% mutate(species = "Catsharks")
)

# Create the combined plot
p_combined <- ggplot(data=data_combined, 
                     mapping=aes(x=var, y=median, ymin=cil, ymax=ciu, color=species)) + 
  geom_pointrange(position = position_dodge(width=0.5)) + # dodge to avoid overlap
  coord_flip() +
  scale_color_manual(values = c("Skates" = "steelblue", "Catsharks" = "orange")) + # Green for skates, blue for catsharks
  ylab("Relative influence (%)") + xlab("") +
  theme_article(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(linewidth = .1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
    legend.position = "right" # Optional: Add legend for species
  )

# Print the combined plot
p_combined


# export plot
p_png <- paste0(outdir, "/combined_", mod_code, "_", family, "_var_influence_boot_c.png")
ggsave(p_png, p_combined, width=17, height=13, units="cm", dpi=300)

