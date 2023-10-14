# soil-carbon-stocks

## Reproducing
Firstly, clone or download the repository. Now run all the following scripts, in order:   

1. Sample points for each county in Arkansas:
```
src/01-sample-points.R
```

2. Download SSURGO variables, and estimate SOC:
```
src/02-download-ssurgo.R
```

3. Download weekly 1 km resolution Volumetric Water Content (VWC) raster files:
```
src/03-download-vwc.R
```

4. Preprocess VWC data:
```
src/04-preprocess-vwc.R
```
