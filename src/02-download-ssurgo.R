library(sf)
library(aqp)
library(soilDB)
library(sharpshootR)
library(terra)

# load sol util functions
source("src/soil_utils.R")

df_points <- read.csv("output/points.csv")

cat("# points to extract SSURGO data:", nrow(df_points), "\n")
socs <- data.frame()
horizons <- data.frame()
for (i in 1:nrow(df_points)) {
  try({
    p <- vect(df_points[i, c("lon", "lat")], crs = "epsg:4326")
    mu <- SDA_spatialQuery(p, what = 'mupolygon', geomIntersection = T)
    s <- processSSURGO(mu$mukey, .top = 0, .bottom = 20)
    # soc
    soc <- estimateStock(s)
    soc$id <- df_points[i, "id"]
    socs <- rbind(socs, soc)
    # horizons
    hz <- horizons(s)
    hz$mukey <- mu$mukey
    horizons <- rbind(horizons, hz)
  })
}

# remove duplicated points because some sampled points lie in the same mukey
socs <- socs[!duplicated(socs$mukey), ]
rownames(socs) <- NULL
socs$id <- as.integer(socs$id)
horizons$hzID_mukey <- paste0(horizons$hzID, "_", horizons$mukey)
horizons <- horizons[!duplicated(horizons$hzID_mukey), ]
rownames(horizons) <- NULL
horizons$hzID_mukey <- NULL

# summaries
cat("summary SOC:\n")
summary(socs)
cat("summary horizons\n:")
summary(horizons)

# write datasets
write.csv(socs, "output/soc.csv", row.names = F)
write.csv(horizons, "output/horizons.csv", row.names = F)
