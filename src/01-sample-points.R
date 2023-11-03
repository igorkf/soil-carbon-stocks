library(sf)
library(ggplot2)

prepare_counties <- function(state, counties_path) {
  counties <- readxl::read_xlsx(counties_path)
  counties_geom <- tigris::counties(state = state)
  counties_geom <- counties_geom[, c("NAME", "geometry")]
  counties <- merge(counties, counties_geom, by.x = "Counties", by.y = "NAME")
  return(counties)
}

sample_points <- function(data, k = 20) {
  df_points <- data.frame()
  for (i in 1:nrow(data)) {
    sampled_points <- sf::st_sample(data$geometry[[i]], size = k)
    df_lat_lon <- as.data.frame(do.call(rbind, sf::st_geometry(sampled_points)))
    colnames(df_lat_lon) <- c("lon", "lat")
    df_lat_lon$county <- data$Counties[[i]]
    df_points <- rbind(df_points, df_lat_lon)
  }
  df_points$id <- rownames(df_points)
  return(df_points)
}


# ARKANSAS
set.seed(2023)
K <- 20
counties_ar <- prepare_counties("Arkansas", "data/arkansas_counties.xlsx")
points_ar <- sample_points(counties_ar, k = K)
summary(points_ar)
df_sf <- st_as_sf(points_ar, coords = c("lon", "lat"))
st_crs(df_sf) <- 4326
ggplot(df_sf) + 
  geom_sf(size = 0.5) + 
  theme_bw()

write.csv(points_ar, "output/Arkansas/points.csv", row.names = F)

# CALIFORNIA
set.seed(2023)
K <- 20
counties_ca <- prepare_counties("California", "data/california_counties.xlsx")
points_ca <- sample_points(counties_ca, k = K)
summary(points_ca)
df_sf <- st_as_sf(points_ca, coords = c("lon", "lat"))
st_crs(df_sf) <- 4326
ggplot(df_sf) + 
  geom_sf(size = 0.5) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_bw()

write.csv(points_ca, "output/California/points.csv", row.names = F)
