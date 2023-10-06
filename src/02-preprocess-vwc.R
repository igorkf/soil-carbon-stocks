##################################################################################
# based on the script developed by Dr. Leo Bastos
# https://github.com/leombastos/2023_aghack_vwc/blob/main/code/02%20visualization.qmd
##################################################################################

remotes::install_github("ropensci/USAboundaries")
install.packages("USAboundariesData", repos = "https://ropensci.r-universe.dev", type = "source")

library(tidyverse)
library(sf)
library(stars)
library(USAboundaries)

rasters <- list.files(path = "output", pattern = '.tif$', all.files = T, full.names = T) 

df <- data.frame(source = rasters) %>%
  separate(source, into = c("path", "meta"), sep = "/", remove = F) %>% 
  mutate(start_date = as.Date(substr(meta, 29, 38), "%Y.%m.%d")) %>% 
  mutate(end_date = as.Date(substr(meta, 29, 38), "%Y.%m.%d")) %>% 
  dplyr::select(start_date, end_date, source) %>% 
  mutate(raster = map(source, ~read_stars(.x) %>% rename(vwc = 1)))

# check first raster
st_crs(df$raster[[1]])

# plot 
ggplot() +
  geom_stars(data = df$raster[[1]], sf = T) +
  scale_fill_viridis_c() #  + geom_sf(data = ga_fields)

ggplot() +
  geom_stars(data = df$raster[[60]], sf = T) +
  scale_fill_viridis_c()
