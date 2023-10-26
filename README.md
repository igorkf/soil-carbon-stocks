# soil-carbon-stocks

## Reproducing
Firstly, clone or download the repository. Now run all the following scripts, in order:   

1. Sample points for each county:
```
src/01-sample-points.R
```

2. Download SSURGO variables (bulk density, SOC, etc.):
```
src/02-download-ssurgo.R
```

3. Download top daily 9 km resolution Volumetric Water Content (VWC) raster files from Crop-CASMA:
```
src/03-download-vwc.R
```

4. Preprocess VWC data:
```
src/04-preprocess-vwc.R
```

5. Download daily precipitation data from NASA POWER:
```
src/05-download-precipitation.R
```

6. Download evapotranspiration data from GEE:    

```
src/06-download-evapotranspiration.R
```
_Disclaimer_: This part needs a lot of additional dependencies (python, rgee, etc.). Follow the instructions contained on the script to setup everything you need.

7. Merge all data:
```
src/07-merge-data.R
```

8. Run a linear mixed model:
```
src/08-model.R
```

Tested on:    
- Windows 10
- Ubuntu 22


