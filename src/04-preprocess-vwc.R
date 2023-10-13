##################################################################################
# based on the script developed by Dr. Leo Bastos
# https://github.com/leombastos/2023_aghack_vwc/blob/main/code/02%20visualization.qmd
##################################################################################

# install if needed
if(!require(USAboundaries)){
  remotes::install_github("ropensci/USAboundaries")
  install.packages("USAboundariesData", repos = "https://ropensci.r-universe.dev", type = "source")
}

library(sf)
library(USAboundaries)
library(terra)

# read all rasters
rasters <- list.files(path = "output", pattern = ".tif$", all.files = T, full.names = T)
# df_rasters <- data.frame(path = rasters)
# df_rasters$start_date <- as.Date(substr(df$path, 36, 45), "%Y.%m.%d")
# df_rasters$end_date <- as.Date(substr(df$path, 47, 56), "%Y.%m.%d")

# read points of interest and intersect with WVC rasters
df_points <- read.csv("output/points_soc.csv")
df_points$id <- 1:nrow(df_points)
ps <- vect(df_points[, c("lon", "lat")], crs = "epsg:4326")

# read raster, project, and intersect with points of interest
df_wvc <- data.frame()
for (path in rasters) {
  r <- rast(path)
  r <- project(r, "epsg:4326")
  df_temp <- extract(r, ps)
  colnames(df_temp) <- c("id", "wvc")
  df_temp$path <- path
  df_temp <- cbind(df_points, df_temp[, c("wvc", "path")])  # bind points dataset
  df_wvc <- rbind(df_wvc, df_temp)
  cat("raster:", path, "\n")
}

# plot sequence of WVC across time for some points
plot(wvc ~ 1, data = df_wvc[df_wvc$id == 1, ], type = "b")
plot(wvc ~ 1, data = df_wvc[df_wvc$id == 100, ], type = "b")
plot(wvc ~ 1, data = df_wvc[df_wvc$id == 200, ], type = "b")
plot(wvc ~ 1, data = df_wvc[df_wvc$id == 300, ], type = "b")
plot(wvc ~ 1, data = df_wvc[df_wvc$id == 400, ], type = "b")

# write WVC to file
write.csv(df_wvc, "output/points_soc_wvc.csv", row.names = F)

# TODO: check ways to do aggregation in WVC as it is a time-series data
# aggregate by point id
df_agg <- aggregate(cbind(wvc, stock) ~ id, data = df_wvc, FUN = median)
cor(df_agg$wvc, df_agg$stock)
plot(df_agg$wvc, df_agg$stock)
