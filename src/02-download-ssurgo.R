library(sf)
library(aqp)
library(soilDB)
library(sharpshootR)
library(terra)

# load sol util functions
source("src/soil_utils.R")

extract_ssurgo <- function(data) {
  cat("# points to extract SSURGO data:", nrow(data), "\n")
  socs <- data.frame()
  horizons <- data.frame()
  for (i in 1:nrow(data)) {
    try({
      p <- vect(data[i, c("lon", "lat")], crs = "epsg:4326")
      mu <- SDA_spatialQuery(p, what = 'mupolygon', geomIntersection = T)
      s <- processSSURGO(mu$mukey, .top = 0, .bottom = 20)
      # soc
      soc <- estimateStock(s)
      soc$id <- data[i, "id"]
      socs <- rbind(socs, soc)
      # horizons
      hz <- aqp::horizons(s)
      hz$mukey <- mu$mukey
      hz$id <- data[i, "id"]
      horizons <- rbind(horizons, hz)
    })
  }
  return(list(socs = socs, horizons = horizons))
}

clean_ssurgo <- function(socs, horizons) {
  # remove duplicated points because some sampled points lie in the same mukey
  socs <- socs[!duplicated(socs$mukey), ]
  rownames(socs) <- NULL
  socs$id <- as.integer(socs$id)
  horizons$hzID_mukey <- paste0(horizons$hzID, "_", horizons$mukey)
  horizons <- horizons[!duplicated(horizons$hzID_mukey), ]
  rownames(horizons) <- NULL
  horizons$hzID_mukey <- NULL
  return(list(socs = socs, horizons = horizons))
}


# ARKANSAS
points_ar <- read.csv("output/Arkansas/points.csv")
ssurgo_ar <- extract_ssurgo(points_ar)
ssurgo_ar <- clean_ssurgo(ssurgo_ar$socs, ssurgo_ar$horizons)
summary(ssurgo_ar$socs)
summary(ssurgo_ar$horizons)
write.csv(ssurgo_ar$socs, "output/Arkansas/socs.csv", row.names = F)
write.csv(ssurgo_ar$horizons, "output/Arkansas/horizons.csv", row.names = F)

# CALIFORNIA
points_ca <- read.csv("output/California/points.csv")
ssurgo_ca <- extract_ssurgo(points_ca)
ssurgo_ca <- clean_ssurgo(ssurgo_ca$socs, ssurgo_ca$horizons)
summary(ssurgo_ca$socs)
summary(ssurgo_ca$horizons)
write.csv(ssurgo_ca$socs, "output/California/socs.csv", row.names = F)
write.csv(ssurgo_ca$horizons, "output/California/horizons.csv", row.names = F)
