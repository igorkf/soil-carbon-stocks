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
pr$year <- NULL

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
n <- as.integer(as.Date("2023-08-31") - as.Date("2023-01-01"))
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
  left_join(soc, by = "id") %>% 
  left_join(texture, by = "chkey")

# keep first texture because of mismatch
data <- data %>% 
  group_by(id, month) %>% 
  slice(1)

# dropping NA
data <- data[!is.na(data$evapotranspiration), ]
data <- data[!is.na(data$vwc), ]
data <- data[!is.na(data$stock), ]
colSums(is.na(data))

# number of points per county?
table(filter(data, month == 1)$county)

# creating new variables
# data$evapotranspiration <- data$evapotranspiration * 0.1
data$vwc_L <- ((data$vwc / data$dbovendry_r) * (data$dbovendry_r * 100000)) / 1000
data$prec_minus_et <- data$precipitation - data$evapotranspiration
data$total_porosity <- 1 - (data$dbovendry_r / 2.65)
data$litter_to_saturation <- ((data$total_porosity / data$dbovendry_r) * (data$dbovendry_r * 100000)) / 1000

# write to disk
write.csv(data, "output/final_data.csv", row.names = F)

summary(data)
hist(data$vwc)
hist(data$vwc_L)
hist(data$evapotranspiration)
hist(data$precipitation)
hist(data$prec_minus_et)
hist(data$total_porosity)
hist(data$litter_to_saturation)
hist(data$stock)
hist(data$dbovendry_r)

# aggregate
data_agg <- data %>% 
  group_by(id, county) %>% 
  summarise(
    texture = first(texture), 
    min_precipitation = min(precipitation),
    stock = first(stock)
  )

data_agg
data_agg$texture <- factor(data_agg$texture)
mod <- lm(log(stock) ~ county + texture + min_precipitation, data = data_agg)
summary(mod)
plot(data_agg$stock, mod$fitted.values)
plot(log(data_agg$stock), mod$fitted.values)
cor(log(data_agg$stock), mod$fitted.values)
