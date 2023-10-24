##################################################################################
# based on the script developed by Dr. Leo Bastos
# https://github.com/leombastos/2023_aghack_vwc/blob/main/code/02%20visualization.qmd
##################################################################################

library(ggplot2)
library(sf)
library(terra)

intersect_points <- function(data, path_rasters) {
  ps <- vect(data[, c("lon", "lat")], crs = "epsg:4326")
  rasters <- list.files(path = path_rasters, pattern = ".tif$", all.files = T, full.names = T)
  df_vwc <- data.frame()
  for (path in rasters) {
    r <- rast(path)
    r <- project(r, "epsg:4326")
    df_temp <- extract(r, ps)
    colnames(df_temp) <- c("id", "vwc")
    df_temp$path <- path
    df_temp <- cbind(data, df_temp[, c("vwc", "path")])  # bind points dataset
    df_vwc <- rbind(df_vwc, df_temp)
    cat("raster:", path, "\n")
  }
  return(df_vwc)
}

prepare_points <- function(data) {
  data <- data[order(data$id), ]
  rownames(data) <- NULL
  # create index from path
  data$day <- sequence(tabulate(data$id))
  data$id <- factor(data$id)
  return(data)
}


# ARKANSAS
points_ar <- read.csv("output/Arkansas/points.csv")
vwc_ar <- intersect_points(points_ar, "output/Arkansas")
vwc_ar <- prepare_points(vwc_ar)
ggplot(vwc_ar, aes(x = day, y = vwc)) +
  geom_line(aes(group = id), linewidth = 0.2, alpha = 0.5) +
  facet_wrap(~county) + 
  labs(x = 'Day of the Year', y = 'VWC', 
       title = 'Volumetric Water Content of 20 sampled points from Jan to Set (2023)') +
  theme_bw()
write.csv(vwc_ar, "output/Arkansas/vwc.csv", row.names = F)

# CALIFORNIA
points_ca <- read.csv("output/California/points.csv")
vwc_ca <- intersect_points(points_ca, "output/California")
vwc_ca <- prepare_points(vwc_ca)
ggplot(vwc_ca, aes(x = day, y = vwc)) +
  geom_line(aes(group = id), linewidth = 0.2, alpha = 0.5) +
  facet_wrap(~county) + 
  labs(x = 'Day of the Year', y = 'VWC', 
       title = 'Volumetric Water Content of 20 sampled points from Jan to Set (2023)') +
  theme_bw()
write.csv(vwc_ca, "output/California/vwc.csv", row.names = F)
