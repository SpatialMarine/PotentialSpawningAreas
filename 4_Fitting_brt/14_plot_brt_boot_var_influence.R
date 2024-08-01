#---------------------------------------------------------------------------------------------------
# plot_brt_boot_var_influence          Plot BRT variable relative incluence using bootstrap
#---------------------------------------------------------------------------------------------------
mod_code <- "brt"
bootstrap <- T
n_boot <- 100
sp_code <- "Gme" #Sca, Gme, Esp, Tma

#---------------------------------------------------------------
# 1. Set data repository
#---------------------------------------------------------------
indir <- paste(output_data, mod_code, sp_code, sep="/")

outdir <- paste(indir, "predict_boost", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import model full model
mod <- readRDS(paste0(indir, "/", sp_code, ".rds"))

# list of bootstrap models
outdir_bootstrap <- paste0(indir, "/bootstrap/", sp_code)
boots_files <- list.files(outdir_bootstrap, full.names = T)

# batch import of bootstrap models
models <- lapply(boots_files, readRDS)

# create empty list to store data
data_list <- list()

for(i in 1:length(models)){
  # get model
  mi <- models[[i]]
  
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

# plot: #orange for S canicula and #steelblue for G melastomus
p <- ggplot(data=data, mapping=aes(x=var, y=median, ymin=cil, ymax=ciu)) + 
  geom_pointrange(col="steelblue") +
  coord_flip() +
  ylab("Relative influence (%)") + xlab("") +
  theme_article(base_size = 14) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0))
  )

# export plot
p_png <- paste0(outdir, "/", sp_code, "_", mod_code, "_var_influence_boot_c.png")
ggsave(p_png, p, width=14, height=17, units="cm", dpi=300)

