##################################################################################
# based on the script developed by Dr. Leo Bastos, and adapted from 
# https://github.com/leombastos/2023_aghack_vwc/blob/main/code/01%20download.qmd
##################################################################################

# data naming convention: https://nassgeo.csiss.gmu.edu/Crop-CASMA-Developer/data/dataNamingConvention
# map: https://nassgeo.csiss.gmu.edu/CropCASMA/
# top soil (top 5 cm)
# sub soil (top 100 cm)

build_urls <- function(data, fips) {
  # build URLs
  data$layer <- paste0("SMAP-9KM-DAILY-TOP_", data$date, "_AVERAGE")
  data$url <- paste0(
    "https://cloud.csiss.gmu.edu/smap_service?service=WPS&version=1.0.0&request=Execute&identifier=GetFileByFips&DataInputs=layer=",
    data$layer, ';fips=', fips
  )
  return(data)
}

extract_vwc <- function(data, state) {
  for (i in 1:nrow(data)) {
    destfile <- paste0(file.path("output/", state, "/", data$layer[[i]]), ".tif")
    lines <- readLines(data$url[[i]], warn = F)
    lines <- strsplit(lines, "(<|>)")
    lines <- unlist(lines)
    url <- grep("https://.*.tif", lines, value = T)
    if (!identical(url, character(0))) {
      download.file(url, destfile, method = "wget", extra = "--no-check-certificate", quiet = T)
    }
    cat("Day:", data$date[[i]], "\n")
  }
}


start <- as.Date("2023-01-01")
end <- as.Date("2023-08-31")
data <- data.frame(date = seq(start, end, 1))
data$date <- gsub("-", ".", data$date)

# ARKANSAS
urls_ar <- build_urls(data, fips = "05")
extract_vwc(urls_ar, "Arkansas")

# CALIFORNIA
urls_ca <- build_urls(data, fips = "06")
extract_vwc(urls_ca, "California")
