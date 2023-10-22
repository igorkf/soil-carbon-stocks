##################################################################################
# based on the script developed by Dr. Leo Bastos, and adapted from 
# https://github.com/leombastos/2023_aghack_vwc/blob/main/code/01%20download.qmd
##################################################################################

# data naming convention: https://nassgeo.csiss.gmu.edu/Crop-CASMA-Developer/data/dataNamingConvention
# map: https://nassgeo.csiss.gmu.edu/CropCASMA/
# top soil (top 5 cm)
# sub soil (top 100 cm)

start <- as.Date("2023-01-01")
end <- as.Date("2023-08-31")
df <- data.frame(date = seq(start, end, 1))
df$date <- gsub("-", ".", df$date)

# build URLs
stateFIPS <- "05"  # Arkansas
df$layer <- paste0("SMAP-9KM-DAILY-TOP_", df$date, "_AVERAGE")
df$url <- paste0(
  "https://cloud.csiss.gmu.edu/smap_service?service=WPS&version=1.0.0&request=Execute&identifier=GetFileByFips&DataInputs=layer=",
  df$layer, ';fips=', stateFIPS
)

# download all images
for (i in 1:nrow(df)) {
  destfile <- paste0(file.path("output", df$layer[[i]]), ".tif")
  lines <- readLines(df$url[[i]], warn = F)
  lines <- strsplit(lines, "(<|>)")
  lines <- unlist(lines)
  url <- grep("https://.*.tif", lines, value = T)
  if (!identical(url, character(0))) {
    download.file(url, destfile, method = "wget", extra = "--no-check-certificate", quiet = T)
  }
  cat("Day:", df$date[[i]], "\n")
}
