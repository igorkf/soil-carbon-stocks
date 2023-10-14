##################################################################################
# based on the script developed by Dr. Leo Bastos
# https://github.com/leombastos/2023_aghack_vwc/blob/main/code/02%20visualization.qmd
##################################################################################

library(ggplot2)
library(sf)
library(terra)

# read all rasters
rasters <- list.files(path = "output", pattern = ".tif$", all.files = T, full.names = T)

# read points of interest and intersect with VWC
df_points <- read.csv("output/points.csv")
ps <- vect(df_points[, c("lon", "lat")], crs = "epsg:4326")

# read raster, project, and intersect with points of interest
df_vwc <- data.frame()
for (path in rasters) {
  r <- rast(path)
  r <- project(r, "epsg:4326")
  df_temp <- extract(r, ps)
  colnames(df_temp) <- c("id", "vwc")
  df_temp$path <- path
  df_temp <- cbind(df_points, df_temp[, c("vwc", "path")])  # bind points dataset
  df_vwc <- rbind(df_vwc, df_temp)
  cat("raster:", path, "\n")
}

# order by point id
df_vwc <- df_vwc[order(df_vwc$id), ]
rownames(df_vwc) <- NULL

# create index from path
df_vwc$index <- sapply(strsplit(df_vwc$path, split = "_"), `[`, 3)
df_vwc$index <- as.integer(df_vwc$index)
df_vwc$id <- factor(df_vwc$id)

# plot VWC time series for each county
ggplot(df_vwc, aes(x = index, y = vwc)) +
  geom_line(aes(group = id), linewidth = 0.2, alpha = 0.5) +
  facet_wrap(~county) + 
  labs(x = 'Week number', y = 'VWC', 
       title = 'Volumetric Water Content of 20 sampled points from Jan to Set (2023)') +
  theme_bw()

# write vwc to file
write.csv(df_vwc, "output/vwc.csv", row.names = F)
