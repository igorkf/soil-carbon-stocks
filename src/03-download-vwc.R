##################################################################################
# based on the script developed by Dr. Leo Bastos, and adapted from 
# https://nassgeo.csiss.gmu.edu/Crop-CASMA-Developer/wps/overview/
# https://github.com/leombastos/2023_aghack_vwc/blob/main/code/01%20download.qmd
##################################################################################

# data naming convention: https://nassgeo.csiss.gmu.edu/Crop-CASMA-Developer/data/dataNamingConvention
# map: https://nassgeo.csiss.gmu.edu/CropCASMA/
# top soil (top 5 cm)
# sub soil (top 100 cm)

# from https://stackoverflow.com/a/20671197/11122513
seq_weekday <- function(selday, start, end) {
  fwd.7 <- start + 0:6
  first.day <- fwd.7[as.numeric(format(fwd.7, "%w")) == selday]
  seq.Date(first.day, end, by = "week")
}


df <- data.frame()
years <- 2020:2023
for (year in years) {
  start <- as.Date(paste0(year, "-01-01"))  # first day of current year
  end <- as.Date(paste0(year + 1, "-01-01")) - 1  # last day of current year
  seq_mondays <- seq_weekday(1, start, end)  # pick all mondays of current year
  df_temp <- data.frame(mondays = seq_mondays, sundays = seq_mondays + 6)
  df_temp$year <- strftime(df_temp$mondays, format = '%Y')
  df_temp$week <- strftime(df_temp$mondays, format = '%V')
  df_temp$mondays <- gsub("-", ".", df_temp$mondays)
  df_temp$sundays <- gsub("-", ".", df_temp$sundays)
  df <- rbind(df, df_temp)
}

stateFIPS <- "05"  # Arkansas
df$layer <- paste0("SMAP-HYB-1KM-WEEKLY_", df$year, "_", df$week, "_", df$mondays, "_", df$sundays, "_PM")
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
}
