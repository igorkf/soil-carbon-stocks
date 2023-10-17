library(nasapower)

df_points <- read.csv("output/points.csv")

df_prec <- data.frame()
for (i in 1:nrow(df_points)) {
  df <- get_power(
    community = "ag",
    lonlat = c(df_points[i, "lon"], df_points[i, "lat"]),
    pars = c("PRECTOTCORR"),
    dates = c("2023-01-01", "2023-08-31"),
    temporal_api = "daily"
  )  # mm/day
  df <- df[, c("YEAR", "MM", "PRECTOTCORR")]
  df <- aggregate(PRECTOTCORR ~ YEAR + MM, data = df, FUN = sum)
  df$id <- df_points[i, "id"]
  df_prec <- rbind(df_prec, df)
}
colnames(df_prec) <- c("year", "month", "precipitation", "id")

# write to disk
write.csv(df_prec, "output/precipitation.csv", row.names = F)
