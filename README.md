# soil-carbon-stocks

## Reproducing
Firstly, clone the repository. Now run all the following scripts, in order:   

1. Sample points for each county in Arkansas
```
src/01-sample-points.R
```

2. Estimate Soil Organic Carbon (SOC) stocks:
```
src/02-download-soc.R
```

2. Download weekly 1 km resolution Water Volumetric Content (WVC) raster files:
```
src/03-download-vwc.R
```
