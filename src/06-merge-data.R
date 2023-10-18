library(tidyverse)
library(soilDB)

# soc
soc <- read.csv("output/soc.csv")
soc <- soc[, c("id", "fraction", "stock")]

# switch-case months
# months <- do.call(function(x) toupper(substr(x, 1, 3)), list(month.name))
# months_ids <- seq(1, length(months))

# precipitation
pr <- read.csv("output/precipitation.csv")

# evapotranspiration
et <- read.csv("output/Evapotranspiration.csv")
et_years <- names(et[, grepl("2023", names(et))])
et_cols <- c("id", "lon", "lat", "county", et_years)
et <- et[, et_cols]
et <- et %>% 
  pivot_longer(-c(id, lon, lat, county)) %>% 
  rename(month = name, evapotranspiration = value) %>% 
  mutate(month = as.integer(str_sub(month, start = 7, end = 8))) %>% 
  filter(month <= 8) %>% 
  drop_na()

# linear interpolation on et
n <- as.integer(as.Date("2023-08-31") - as.Date("2023-01-01")) + 1
et2 <- data.frame()
for (id in unique(et$id)) {
  et_sub <- et[et$id == id, ]
  lin <- data.frame(evapotranspiration = approx(x = et_sub$month, et_sub$evapotranspiration, n = n)$y)
  lin$id <- id
  lin$day <- 1:nrow(lin)
  et2 <- rbind(et2, lin)
}
et2$evapotranspiration <- et2$evapotranspiration / 8
et2 <- inner_join(et2, distinct(et[, c("id", "lon", "lat", "county")]), by = "id")
# ggplot(et2, aes(x = day, y = evapotranspiration)) +
#   geom_point() +
#   facet_wrap(~county)

# ssurgo
hz <- read.csv("output/horizons.csv")
hz <- hz[hz$hzID == 1, ]  # first horizon layer only
rownames(hz) <- NULL
hz <- hz[, c("id", "mukey", "chkey", "dbovendry_r", "om_r", "oc")]

# add texture on horizons
query <- sprintf("SELECT chkey, texture, texdesc FROM chtexturegrp WHERE chkey IN %s", format_SQL_in_statement(unique(hz$chkey)))
texture <- SDA_query(query)
fc <- readxl::read_excel("data/texture_fc.xlsx")

# volumetric water content
vwc <- read.csv("output/vwc.csv")
vwc$date <- as.Date(gsub("\\.", "\\-", substr(vwc$path, 27, 36)))
vwc$mo <- strftime(vwc$date, "%m")
vwc$yr <- strftime(vwc$date, "%Y")
vwc <- vwc[, c("id", "vwc", "day", "date", "mo", "yr")]

# joining
data <- left_join(et2, vwc, by = c("id", "day")) %>%
  left_join(pr, by = c("id", "day")) %>%
  left_join(hz, by = "id") %>% 
  left_join(soc, by = "id") %>% 
  left_join(texture, by = "chkey") %>% 
  left_join(fc, by = "texture")

# keep first texture because of mismatch
data <- data %>% 
  group_by(id, day) %>% 
  slice(1)

# dropping NA
colSums(is.na(data))
data <- data[!is.na(data$vwc), ]
data <- data[!is.na(data$stock), ]
colSums(is.na(data))

# number of points per county?
table(filter(data, day == 1)$county)

# creating new variables
# data$evapotranspiration <- data$evapotranspiration * 0.1
data$vwc_L <- ((data$vwc / data$dbovendry_r) * (data$dbovendry_r * 50000)) / 1000
data$fc_L <- ((data$fc / data$dbovendry_r) * (data$dbovendry_r * 50000)) / 1000
data$potential_water <- data$precipitation - data$evapotranspiration + data$vwc_L
data$total_porosity <- 1 - (data$dbovendry_r / 2.65)
data$litter_to_saturation <- ((data$total_porosity / data$dbovendry_r) * (data$dbovendry_r * 50000)) / 1000
data$potential_water_minus_runoff <- ifelse(
  data$potential_water > data$litter_to_saturation, 
  data$litter_to_saturation, 
  data$potential_water
)
data$potential_water_minus_runoff <- ifelse(data$potential_water_minus_runoff < 0, 0, data$potential_water_minus_runoff) 
data$percolation <- ifelse(
  data$potential_water_minus_runoff < 0, 
  0, 
  data$potential_water - data$potential_water_minus_runoff - data$fc_L
)
data$volumetric_flux <- 0.05 / 86400
data$pore_velocity <- data$volumetric_flux / data$fc
data$ke <- 0.5 * data$potential_water_minus_runoff * (data$pore_velocity ^ 2)

# summary(data)
# hist(data$ke)
# hist(data$pore_velocity)
# hist(data$volumetric_flux)
# hist(data$potential_water_minus_runoff)
# hist(data$percolation)
# hist(data$vwc)
# hist(data$vwc_L)
# hist(data$evapotranspiration)
# hist(data$precipitation)
# hist(data$total_porosity)
# hist(data$litter_to_saturation)
# hist(data$stock)
# hist(data$dbovendry_r)

# write to disk
write.csv(data, "output/final_data.csv", row.names = F)
