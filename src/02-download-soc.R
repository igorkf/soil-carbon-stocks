library(ggplot2)
library(sf)
library(aqp)
library(soilDB)
library(sharpshootR)
library(terra)


# q <- "SELECT * FROM sacatalog WHERE areasymbol LIKE 'AR%';"
# x <- SDA_query(q)
# head(x)


# load sol util functions
source("src/soil_utils.R")

df_points <- read.csv("output/points.csv")

cat("# points to extract SOC stock:", nrow(df_points), "\n")
socs <- data.frame()
for (i in 1:nrow(df_points)) {
  try({
    p <- vect(df_points[i, c("lon", "lat")], crs = "epsg:4326")
    mu <- SDA_spatialQuery(p, what = 'mupolygon', geomIntersection = T)
    s <- processSSURGO(mu$mukey, .top = 0, .bottom = 20)
    soc <- estimateStock(s)
    soc$id <- df_points[i, "id"]
    socs <- rbind(socs, soc)
  })
}

# remove duplicated points based on mukey
socs <- socs[!duplicated(socs$mukey), ]

# merge socs to the points dataframe
socs$id <- as.integer(socs$id)
df_points <- merge(df_points, socs, by = "id")
df_points <- df_points[order(df_points$id), ]
rownames(df_points) <- NULL

# check number of points with non-NA SOC stocks per county
cat("# point-wise SOC stock estimations per county:")
table(df_points[!is.na(df_points$stock), "county"])
cat("\n")

# plot SOC map
df_sf <- st_as_sf(df_points, coords = c("lon", "lat"))
df_sf <- df_sf[!is.na(df_sf$stock), ]
st_crs(df_sf) <- 4326
set.seed(2)
ggplot(df_sf) + 
  geom_sf(aes(color = stock), size = 2) +
  scale_color_viridis_c() +
  theme_bw()

# write points with SOC
write.csv(df_points, "output/points_soc.csv", row.names = F)

