library(tidyverse)

# soc
soc <- read.csv("output/soc.csv")
soc <- soc[, c("id", "fraction", "stock")]

# switch-case months
months <- do.call(function(x) toupper(substr(x, 1, 3)), list(month.name))
months_ids <- seq(1, length(months))

# precipitation
pr <- read.csv("output/Precipitation.csv")
pr <- pr[pr$YEAR == 2022, ]
rownames(pr) <- NULL
pr <- pr %>% 
  select(-c(PARAMETER, YEAR, ANN, LAT, LON)) %>% 
  pivot_longer(-c(county, id)) %>% 
  rename(month = name, evapotranspiration = value) 
pr$month <- match(str_to_title(pr$month), month.abb)

# evapotranspiration
et <- read.csv("output/Evapotranspiration.csv")
et_years <- names(et[, grepl("2022", names(et))])
et_cols <- c("id", "lon", "lat", et_years)
et <- et[, et_cols]
et <- et %>% 
  pivot_longer(-c(id, lon, lat)) %>% 
  rename(month = name, preciptation = value) %>% 
  mutate(month = as.integer(str_sub(month, start = 7, end = 8)))

# ssurgo
hz <- read.csv("output/horizons.csv")
hz <- hz[hz$hzID == 1, ]  # first horizon layer only
rownames(hz) <- NULL
hz <- hz[, c("id", "mukey", "dbovendry_r", "om_r", "oc")]

# volumetric water content
vwc <- read.csv("output/vwc.csv")
vwc <- vwc %>% 
  mutate(start = as.Date(str_sub(path, 36, 45), format = "%Y.%m.%d")) %>% 
  mutate(month = month(start)) %>% 
  group_by(id, month) %>%
  summarise(vwc = mean(vwc))

# joining
data <- left_join(et, pr, by = c("id", "month")) %>% 
  left_join(vwc, by = c("id", "month")) %>% 
  left_join(hz, by = "id") %>% 
  left_join(soc, by = "id")

# dropping NA
data <- data[!is.na(data$stock), ]
data <- data[!is.na(data$preciptation), ]
data <- data[!is.na(data$vwc), ]
colSums(is.na(data))

# number of points per county?
table(filter(data, month == 1)$county)

# creating new variables
data$prec_minus_et <- data$preciptation - data$evapotranspiration
data$total_porosity <- 1 - (data$dbovendry_r / 2.65)  # same as litter_to_saturation but different scale?
data$litter_to_saturation <- ((data$total_porosity / data$dbovendry_r) * (data$dbovendry_r * 100000)) / 1000

summary(data)
hist(data$prec_minus_et)
hist(data$total_porosity)
hist(data$litter_to_saturation)
hist(data$stock)
hist(data$dbovendry_r)
