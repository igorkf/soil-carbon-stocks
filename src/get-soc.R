library(ggplot2)

# read list of counties that we will focus 
counties <- readxl::read_xlsx("data/arkansas_counties.xlsx")

# read counties geometries from Arkansas
counties_geom <- tigris::counties(state = "Arkansas")
counties_geom <- counties_geom[, c("NAME", "geometry")]

# merge both to keep only some counties
counties <- merge(counties, counties_geom, by.x = "Counties", by.y = "NAME")

# sample k points for each county
set.seed(2023)
K <- 5
df_points <- data.frame()
for (i in 1:nrow(counties)) {
  sampled_points <- sf::st_sample(counties$geometry[[i]], size = K)
  df_lat_lon <- as.data.frame(do.call(rbind, sf::st_geometry(sampled_points)))
  colnames(df_lat_lon) <- c("lon", "lat")
  df_lat_lon$countie <- counties$Counties[[i]]
  df_points <- rbind(df_points, df_lat_lon)
}
df_points$id <- rownames(df_points)

# check map
df_sf <- st_as_sf(df_points, coords = c('lon', 'lat'))
st_crs(df_sf) <- 4326
ggplot(df_sf) + 
  geom_sf()

# download SOC from a list of coordinates (takes a while)
# some points might have not-available (NA) SOC
# TODO: download only some points because I don't know how to filter by DATE
soil_grids <- soilDB::fetchSoilGrids(df_points[1:10, c("id", "lat", "lon")], variables = "soc", depth_intervals = "0-5", progress = T)

df_points[1:length(soil_grids$socQ05), "soc05"] <- soil_grids$socQ05 
summary(df_points)

# check SOC across points
df_sf <- merge(my_sf, df_points)
ggplot(df_sf) + 
  geom_sf(aes(color = soc05))
