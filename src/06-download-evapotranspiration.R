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
# conda activate rgee_py (in Windows is: activate rgee_py)
# pip install google-api-python-client
# pip install earthengine-api
# pip install numpy

# 4. get the rgee_py environment path
environments <- reticulate::conda_list()
rgee_environment_dir <- environments[environments$name == "rgee_py", "python"]

# 4. install geojsonio
# this package is cumbersome to install in Linux
# if you are in Linux, you can follow some rules here: https://github.com/ropensci/geojsonio#install
install.packages("https://cran.r-project.org/src/contrib/Archive/rgeos/rgeos_0.6-4.tar.gz", type = "source")  # rgeos was archived in CRAN
install.packages("geojsonio")
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
rgee::ee_Initialize(drive = T)


#########################################################################
# evapotranspiration extraction
# by Mario Soto Valencia
#########################################################################

require(raster)
require(sf)
require(sp)
library(rgee)

points <- read.csv("output/points.csv")
pointCoordinates <- points[, c(1,2)]
coordinates(pointCoordinates) <- ~ lon+ lat
proj4string(pointCoordinates) <- CRS("+proj=longlat +datum=WGS84")
poligono <- rgeos::gConvexHull(pointCoordinates)
poligono <- st_as_sf(poligono)
poligono <- rgee::sf_as_ee(poligono)
dataset <- ee$ImageCollection('MODIS/061/MOD16A2')$filter(ee$Filter$date("2021-01-01", "2023-12-31"))$filterBounds(poligono)
dataset$size()$getInfo()
evapotranspiration <- dataset$select("ET")
listImages <- evapotranspiration$toList(91)
dates <- seq(as.Date("2021-01-01"), as.Date("2023-12-31"), by = "month")
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
    data <- array(latlonreduce$get("ETMean")$getInfo())
    lats <- array(latlonreduce$get("latitude")$getInfo())
    lons <- array(latlonreduce$get("longitude")$getInfo())
    df <- data.frame(x = lons, y = lats, ET = data)
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
writeRaster(ETFinal, "output/StackET.tif", overwrite = T)

# Extract ET values from points 
rasValue <- as.data.frame(raster::extract(ETFinal, pointCoordinates))
FinalDatesCharacter <- sapply(FinalDates, as.character)
names(rasValue) <- FinalDatesCharacter
rasValue <- cbind(points, rasValue)
write.table(rasValue, "output/Evapotranspiration.csv", dec = ".", sep = ",", row.names = F)
