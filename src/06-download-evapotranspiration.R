#########################################################################
# rgee package setup
# from https://www.youtube.com/watch?v=1-k6wNL2hlo
#########################################################################

# 1. register a new unpaid-usage project if you don't have one project yet
# https://code.earthengine.google.com/

# 2. install miniconda
# https://docs.conda.io/projects/miniconda/en/latest/miniconda-install.html

# 3. create a new python environment
# run all these steps in your terminal
# conda create -n rgee_py python=3.9
# conda activate rgee_py
# pip install google-api-python-client earthengine-api numpy

# 4. get the rgee_py environment path that you just installed
# if reticulate does not find you anaconda installation, do this:
# do "conda env list" in your terminal, and get the path to the environment
# in Windows is somthing like this: C:/Users/igorf/AppData/Local/miniconda3/envs/rgee_py
# run this command using your path: reticulate::use_python("C:/Users/igorf/AppData/Local/miniconda3/envs/rgee_py")
environments <- reticulate::conda_list()
rgee_environment_dir <- environments[environments$name == "rgee_py", "python"]

# 4. install rgeos and geojsonio
install.packages("https://cran.r-project.org/src/contrib/Archive/rgeos/rgeos_0.6-4.tar.gz", type = "source")  # rgeos was archived in CRAN
# geojsonio is cumbersome to install in Linux
# if you are in Linux, you can follow some rules here: https://github.com/ropensci/geojsonio#install
# you may need to install packages in linux such as libjq-dev, libprotobuf-dev, and protobuf-compiler
install.packages("geojsonio")
install.packages("remotes")
remotes::install_github("r-spatial/rgee")

# 5. set python and rgee
reticulate::use_python(rgee_environment_dir, required = T)
rgee::ee_install_set_pyenv(py_path = rgee_environment_dir, py_env = "rgee_py")
Sys.setenv(RETICULATE_PYTHON = rgee_environment_dir)
Sys.setenv(EARTHENGINE_PYTHON = rgee_environment_dir)

# 6. initilize
# if you get an error run rgee::ee_clean_user_credentials()
# next, copy the token and paste in the R console
# the R console may ask you the root folder...I put users/igorkf
# sometimes you get an error...you can just hit ESC and continue to the next section
rgee::ee_clean_user_credentials()
rgee::ee_Initialize(drive = T)


#########################################################################
# evapotranspiration extraction
# by Mario Soto Valencia
#########################################################################

require(raster)
require(sf)
require(sp)
library(rgee)

extract_evapotranspiration <- function(data, start, end) {
  pointCoordinates <- data[, c("lon", "lat")]
  coordinates(pointCoordinates) <- ~ lon + lat
  proj4string(pointCoordinates) <- CRS("+proj=longlat +datum=WGS84")
  poligono <- rgeos::gConvexHull(pointCoordinates)
  poligono <- st_as_sf(poligono)
  poligono <- rgee::sf_as_ee(poligono)
  dataset <- ee$ImageCollection("MODIS/061/MOD16A2")$filter(ee$Filter$date(start, end))$filterBounds(poligono)
  dataset$size()$getInfo()
  evapotranspiration <- dataset$select("ET")
  listImages <- evapotranspiration$toList(91)
  dates <- seq(as.Date(start), as.Date(end), by = "month")
  stackImages <- list()
  FinalDates <- list()
  for (i in 1:(length(dates) - 1)) {
    d1 <- ee$Date(as.character(dates[i]))
    d2 <- ee$Date(as.character(dates[i + 1]))
    ETMean <- dataset$filter(ee$Filter$date(d1, d2))
    
    if(ETMean$size()$getInfo() > 0) {
      ETMean <- ETMean$select("ET")$mean()$clip(poligono)$rename("ETMean")
      ETMean <- ETMean$unmask(-9999)
      ##DataFrame de indice
      latlon <- ee$Image$pixelLonLat()
      latlon <- latlon$addBands(ETMean)
      # apply reducer to list
      latlonreduce <- latlon$reduceRegion(ee$Reducer$toList(), poligono, maxPixels=1e9, scale=500);
      et <- array(latlonreduce$get("ETMean")$getInfo())
      lats <- array(latlonreduce$get("latitude")$getInfo())
      lons <- array(latlonreduce$get("longitude")$getInfo())
      df <- data.frame(x = lons, y = lats, ET = et)
      ##Genera raster y filtra raster de indice
      XYZ <- rasterFromXYZ(df)
      XYZ[XYZ == -9999] <- NA
      FinalDates[[i]] <- dates[i] + floor((dates[i + 1] - dates[i]) / 2)
      stackImages[[i]] <- XYZ
      cat(dates[i], "\n")
    }
  }
  stackImages <- Filter(Negate(is.null), stackImages)
  FinalDates <- Filter(Negate(is.null), FinalDates)
  ETFinal <- stack(stackImages)
  proj4string(ETFinal) <- CRS("+proj=longlat +datum=WGS84")
  # writeRaster(et_ar, "output/Arkansas/StackET.tif", overwrite = T)
  rasValue <- as.data.frame(raster::extract(ETFinal, pointCoordinates))
  FinalDatesCharacter <- sapply(FinalDates, as.character)
  names(rasValue) <- FinalDatesCharacter
  rasValue <- cbind(data, rasValue)
  return(rasValue)
}


start <- "2021-01-01"
end <- "2023-12-31"

# ARKANSAS
points_ar <- read.csv("output/Arkansas/points.csv")
et_ar <- extract_evapotranspiration(points_ar, start, end)
write.table(et_ar, "output/Arkansas/evapotranspiration.csv", dec = ".", sep = ",", row.names = F)

# CALIFORNIA
points_ca <- read.csv("output/California/points.csv")
et_ca <- extract_evapotranspiration(points_ca, start, end)
write.table(et_ca, "output/California/evapotranspiration.csv", dec = ".", sep = ",", row.names = F)
