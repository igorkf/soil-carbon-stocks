library(sf)

# read list of counties that we will focus 
counties <- readxl::read_xlsx("data/arkansas_counties.xlsx")

# read counties geometries from Arkansas
counties_geom <- tigris::counties(state = "Arkansas")
counties_geom <- counties_geom[, c("NAME", "geometry")]

# merge both to keep only some counties
counties <- merge(counties, counties_geom, by.x = "Counties", by.y = "NAME")
summary(counties)

# sample k points for each county
set.seed(2023)
K <- 10
df_points <- data.frame()
for (i in 1:nrow(counties)) {
  sampled_points <- sf::st_sample(counties$geometry[[i]], size = K)
  df_lat_lon <- as.data.frame(do.call(rbind, sf::st_geometry(sampled_points)))
  colnames(df_lat_lon) <- c("lon", "lat")
  df_lat_lon$countie <- counties$Counties[[i]]
  df_points <- rbind(df_points, df_lat_lon)
}
df_points$id <- rownames(df_points)
summary(df_points)

# check map
df_sf <- st_as_sf(df_points, coords = c("lon", "lat"))
st_crs(df_sf) <- 4326
ggplot() + 
  geom_sf(data = df_sf)

# write dataframe to disk
write.csv(df_points, "output/points.csv", row.names = F)
