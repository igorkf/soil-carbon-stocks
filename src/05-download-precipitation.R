library(nasapower)

extract_precipitation <- function(data) {
  df_prec <- data.frame()
  for (i in 1:nrow(data)) {
    df <- get_power(
      community = "ag",
      lonlat = c(data[i, "lon"], data[i, "lat"]),
      pars = c("PRECTOTCORR"),
      dates = c("2023-01-01", "2023-08-31"),
      temporal_api = "daily"
    )  # mm/day
    df <- df[, c("PRECTOTCORR", "DOY")]
    df$id <- data[i, "id"]
    df_prec <- rbind(df_prec, df)
    cat("Point:", i, "\n")
  }
  colnames(df_prec) <- c("precipitation", "day", "id")
  return(df_prec)
}


start <- "2023-01-01"
end <- "2023-08-31"

# ARKANSAS
points_ar <- read.csv("output/Arkansas/points.csv")
prec_ar <- extract_precipitation(points_ar)
write.csv(prec_ar, "output/Arkansas/precipitation.csv", row.names = F)

# CALIFORNIA
points_ca <- read.csv("output/California/points.csv")
prec_ca <- extract_precipitation(points_ca)
write.csv(prec_ca, "output/California/precipitation.csv", row.names = F)
