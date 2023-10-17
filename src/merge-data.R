library(tidyverse)
library(zoo)

# points
points <- read.csv("output/points.csv")

# soc
soc <- read.csv("output/soc.csv")

# precipitation
pr <- read.csv("output/Precipitation.csv")
pr <- pr[pr$YEAR == 2022, ]
rownames(pr) <- NULL
pr <- pr %>% 
  pivot_longer(5:16) %>% 
  select(-c(PARAMETER, YEAR, ANN)) %>% 
  rename(month = name, evapotranspiration = value) %>% 
  mutate(month = case_when(
    month == 'JAN' ~ 1,
    month == 'FEB' ~ 2,
    month == 'MAR' ~ 3,
    month == 'APR' ~ 4,
    month == 'MAY' ~ 5,
    month == 'JUN' ~ 6,
    month == 'JUL' ~ 7,
    month == 'AUG' ~ 8,
    month == 'SEP' ~ 9,
    month == 'OCT' ~ 10,
    month == 'NOV' ~ 11,
    month == 'DEC' ~ 12
    )
  )

# evapotranspiration
et <- read.csv("output/Evapotranspiration.csv")
et_years <- names(et[, grepl("2022", names(et))])
et_cols <- c("lon", "lat", "county", "id", et_years)
et <- et[, et_cols]
et <- et %>% 
  pivot_longer(-c(1:4)) %>% 
  rename(month = name, preciptation = value) %>% 
  mutate(month = as.integer(str_sub(month, start = 7, end = 8)))

# ssurgo
hz <- read.csv("output/horizons.csv")
hz <- hz[hz$hzID == 1, ]  # first horizon layer only
rownames(hz) <- NULL
table(hz$id)
hz <- hz[, c("id", "mukey", "dbovendry_r", "om_r", "oc")]
hz <- left_join(points, hz, by = "id")

# volumetric water content
vwc <- read.csv("output/vwc.csv")
vwc <- vwc %>% 
  mutate(start = as.Date(str_sub(path, 36, 45), format = "%Y.%m.%d")) %>% 
  mutate(end = as.Date(str_sub(path, 47, 56), format = "%Y.%m.%d"))
  # TODO: create a group from 1 to 12

# convert to daily zoo series
# to.day <- function(i) with(vwc, zoo::zoo(vwc[i], seq(start[i], end[i], "day")))
# z.day <- do.call(c, lapply(1:nrow(vwc), to.day))


# joining
data <- left_join(et, pr, by = c("id", "month")) %>% left_join(hz, by = "id")
data$prec_minus_et <- data$preciptation - data$evapotranspiration
data$total_porosity <- 1 - (data$dbovendry_r / 2.65)
data$litter_to_saturation <- ((data$total_porosity / data$dbovendry_r) * (data$dbovendry_r * 100000)) / 1000

# hist(data$prec_minus_et)
# hist(data$total_porosity)
hist(data$litter_to_saturation)
