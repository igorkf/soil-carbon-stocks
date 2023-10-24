library(tidyverse)
library(soilDB)

merge_data <- function(state, start, end) {
  # soc
  soc <- read.csv(paste0("output/", state, "/socs.csv"))
  soc <- soc[, c("id", "fraction", "stock")]
  colnames(soc) <- c("id", "fraction", "soc")
  
  # precipitation
  pr <- read.csv(paste0("output/", state, "/precipitation.csv"))
  
  # evapotranspiration
  et <- read.csv(paste0("output/", state, "/evapotranspiration.csv"))
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
  n <- as.integer(as.Date(end) - as.Date(start)) + 1
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
  
  # ssurgo
  hz <- read.csv(paste0("output/", state, "/horizons.csv"))
  hz <- hz[hz$hzID == 1, ]  # first horizon layer only
  rownames(hz) <- NULL
  hz <- hz[, c("id", "mukey", "chkey", "dbovendry_r", "om_r", "oc")]
  
  # add texture on horizons
  query <- sprintf(
    "SELECT chkey, texture, texdesc FROM chtexturegrp WHERE chkey IN %s", 
    format_SQL_in_statement(unique(hz$chkey))
  )
  texture <- SDA_query(query)
  fc <- readxl::read_excel("data/texture_fc.xlsx")
  
  # volumetric water content
  vwc <- read.csv(paste0("output/", state, "/vwc.csv"))
  vwc$date_temp <- gsub(".*TOP_(.+)_AVERAGE.*", "\\1", vwc$path)
  vwc$date <- as.Date(gsub("\\.", "\\-", vwc$date_temp))
  vwc_date_temp <- NULL
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
  
  return(data)
}

clean_data <- function(data) {
  # keep first texture because of mismatch
  data <- data %>% 
    group_by(id, day) %>% 
    slice(1)
  
  # dropping NA
  colSums(is.na(data))
  data <- data[!is.na(data$vwc), ]
  data <- data[!is.na(data$soc), ]
  data <- data[!is.na(data$dbovendry_r), ]
  colSums(is.na(data))
  
  return(data)
}

feature_engineering <- function(data) {
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
  data$percolation <- ifelse(data$percolation < 0, 0, data$percolation)
  data$volumetric_flux <- data$percolation / 1000 / 86400
  data$pore_velocity <- data$volumetric_flux / data$fc
  data$ke <- 0.5 * data$potential_water_minus_runoff * (data$pore_velocity ^ 2)
  return(data)
}


start <- "2023-01-01"
end <- "2023-08-31"


# ARKANSAS
data_ar <- merge_data("Arkansas", start, end)
data_ar <- clean_data(data_ar)
table(filter(data_ar, day == 1)$county)
data_ar <- feature_engineering(data_ar)
write.csv(data_ar, "output/Arkansas/final_data.csv", row.names = F)

# CALIFORNIA
data_ca <- merge_data("California", start, end)
data_ca <- clean_data(data_ca)
table(filter(data_ca, day == 1)$county)
data_ca <- feature_engineering(data_ca)
write.csv(data_ca, "output/California/final_data.csv", row.names = F)

# ~75% is AR (for training)
# ~25% of data is CA (for validation)
nrow(data_ca) / nrow(data_ar)



# just a sketch (don't need to run)
# data$stock_fitted <- -1.538 - (0.118 * log(data$ke))
# data_clean <- data[data$stock_fitted != Inf, ]
# rownames(data_clean) <- NULL
# data_clean$stock_fitted <- exp(data_clean$stock_fitted)
# plot(data_clean[, c("stock_fitted", "vwc", "evapotranspiration", "precipitation")])
# plot(data_clean$stock_fitted)
# model
# library(nlme)
# m <- lm(log(stock_fitted) ~ log(precipitation), data = data_clean)
# summary(m)
# m <- lme(log(stock_fitted) ~ precipitation + as.factor(day), random = ~ 1 | county, data = data_clean)
# summary(m)
# plot(m)
# theta0 <- min(data_clean$stock_fitted) * 0.1
# model0 <- lm(log(stock_fitted - theta0) ~ precipitation, data = data_clean)
# alpha0 <- exp(coef(model0)[1])
# beta0 <- coef(model0)[2]
# start <- list(alpha = alpha0, beta = beta0, theta = theta0)
# model <- nls(stock_fitted ~ alpha * exp(beta * ke) + theta, data = data_clean, start = start)
# interpreting
# summary(model)

