library(nlme)
library(ggplot2)

# useful materials
# https://online.stat.psu.edu/stat485/lesson/19/19.8
# https://www.zachrutledge.com/uploads/1/2/5/6/125679559/interpreting_regression_coefficients.pdf
# https://library.virginia.edu/data/articles/interpreting-log-transformations-in-a-linear-model
# https://bookdown.org/steve_midway/DAR/random-effects.html
# https://www.rdocumentation.org/packages/nlme/versions/3.1-163/topics/predict.lme
# https://stackoverflow.com/questions/13828360/access-outlier-ids-in-lme-plot
# https://rpubs.com/mengxu/exponential-model

agg <- function(data) {
  # per id-county aggregation
  
  # sum of energy
  data_agg <- aggregate(ke ~ id + county, data = data, FUN = sum)
  
  # just first value (because all values are the same anyway)
  data_agg$soc <- aggregate(soc ~ id + county, data = data, FUN = function(x) head(x, 1))$soc
  data_agg$lon <- aggregate(lon ~ id + county, data = data, FUN = function(x) head(x, 1))$lon
  data_agg$lat <- aggregate(lat ~ id + county, data = data, FUN = function(x) head(x, 1))$lat
  data_agg$texture <- aggregate(texture ~ id + county, data = data, FUN = function(x) head(x, 1))$texture
  data_agg <- data_agg[data_agg$ke > 0, ]
  rownames(data_agg) <- NULL
  return(data_agg)
}

# ARKANSAS
data_ar <- read.csv("output/Arkansas/final_data.csv")
data_ar_agg <- agg(data_ar)
data_ar_agg$texture <- factor(data_ar_agg$texture)
p1 <- ggplot(data_ar_agg, aes(x = ke, y = soc)) +
  geom_point() +
  labs(x = 'KE (Joules)', y = bquote('SOC' ~ (kg/m ^ 2))) + 
  theme_bw()
p1

p2 <- ggplot(data_ar_agg, aes(x = log(ke), y = log(soc))) +
  geom_point() +
  labs(x = 'log(KE) (Joules)', y = bquote('log(SOC)' ~ (kg/m ^ 2))) + 
  theme_bw()
p2

# CALIFORNIA
data_ca <- read.csv("output/California/final_data.csv")
data_ca <- data_ca[data_ca$texture %in% unique(data_ar$texture), ]
rownames(data_ca) <- NULL
data_ca_agg <- agg(data_ca)
data_ca_agg <- data_ca_agg[data_ca_agg$soc < 15, ]
rownames(data_ca_agg) <- NULL
data_ca_agg$texture <- factor(data_ca_agg$texture)

# comparing both
data_ar_ca_agg <- rbind(
  transform(data_ar_agg, state = "AR"),
  transform(data_ca_agg, state = "CA")
)
ggplot(data_ar_ca_agg, aes(x = ke, y = soc)) +
  geom_point() +
  facet_wrap(~state) +
  theme_bw()
ggplot(data_ar_ca_agg, aes(x = log(ke), y = log(soc))) +
  geom_point() +
  facet_wrap(~state) +
  theme_bw()


###################################################################
# first model for explanability that uses county as random effect
###################################################################
m1 <- lme(log(soc) ~ log(ke), random = ~ 1 | county, data = data_ar_agg)
summary(m1)
# removing potential outliers and fitting again
re <- residuals(m1, type = "normalized")
data_ar_agg2 <- data_ar_agg[abs(re) < 2, ]  # =~ qnorm(0.977)
rownames(data_ar_agg2) <- NULL
m2 <- lme(log(soc) ~ log(ke), random = ~ 1 | county, data = data_ar_agg2)
p3 <- plot(m2)
p3

# plot(m2)
summary(m2)
cor(data_ar_agg2$soc, exp(predict(m2, newdata = data_ar_agg2)))
coefs <- coef(summary(m2))
coefs  

# interpretation of coefficients
# %1 change in "ke" is associated with a "b1" % change in Y (when holding all other variables in the model)

# just plotting the interpretation
# increasing KE on steps of 10%
df_decrease <- data.frame(x = seq(0, 1, 0.10))
df_decrease$y <- ((1 + df_decrease$x) ^ (coefs[2]) - 1)
p4 <- ggplot(df_decrease, aes(x = x, y = y)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "% increase in KE", y = "% decrease in SOC") +
  theme_bw()
p4

# variance components
VarCorr(m2)

# ICC
var_county <- as.numeric(VarCorr(m2)[1, 1])
var_residuals <- as.numeric(VarCorr(m2)[2, 1])
icc <- var_county / (var_county + var_residuals)
icc

# residual plots
qqtemp <- qqnorm(m2)
df_resid <- data.frame(
  residual = residuals(m2, type = "normalized"),
  fitted = exp(fitted(m2)),
  qq = qqtemp$panel.args[[1]]$y,
  lon = data_ar_agg2$lon,
  lat = data_ar_agg2$lat
)

res1 <- ggplot(df_resid, aes(x = fitted, residual)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = "Fitted values", y = "Standardized residuals") + 
  theme_bw()
res1

res2 <- ggplot(df_resid, aes(x = residual, qq)) +
  geom_point() +
  coord_cartesian(expand = F, xlim = c(-3.2, 3.2), ylim = c(-3.2, 3.2)) +
  geom_abline(linetype = "dashed", color = "grey50") +
  theme(aspect.ratio = 1) +
  labs(x = qqtemp$xlab, y = qqtemp$ylab) +
  theme_bw()
res2

res3 <- ggplot(df_resid, aes(x = 1:nrow(df_resid), y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(x = "ID", y = "Standardized residuals") + 
  theme_bw()
res3
  
res4 <- ggplot(df_resid, aes(x = lon, y = lat, colour = residual)) +
  geom_point(alpha = 0.9) +
  labs(x = "Longitude", y = "Latitude") + 
  theme_bw()
res4





# predict on California
# level = 0 means we want population-level predictions because we have unknown levels
data_ca_agg$yhat <- exp(predict(m2, newdata = data_ca_agg, level = 0))
cor(data_ca_agg$soc, data_ca_agg$yhat)
p5 <- ggplot(data_ca_agg, aes(x = soc, y = yhat)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(linetype = "dashed", color = "grey60") +
  coord_cartesian(expand = F, xlim = c(0, NA), ylim = c(0, max(data_ca_agg$soc))) +
  labs(x = "SOC", y = "Predicted SOC") +
  theme_bw() +
  theme(aspect.ratio = 1)
p5


##############################################################

# sketch
# PLOTS
# p1 <- ggplot(d_clean, aes(x = day, y = yhat)) +
#   geom_point() +
#   geom_smooth(method = "loess") +
#   scale_x_continuous(n.breaks = 10) +
#   scale_y_continuous(n.breaks = 8) +
#   labs(x = "Days of the Year", y = bquote('Predicted SOC' ~ (kg/m ^ 2))) +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(size = 18, color = "black"),
#     axis.text.y = element_text(size = 18, color = "black"),
#     axis.title.x = element_text(size = 20, color = "black"),
#     axis.title.y = element_text(size = 20, color = "black")
#   )
# p1
# 
# p2 <- ggplot(d_clean, aes(x = day, y = ke)) +
#   geom_point() +
#   geom_smooth(method = "loess") +
#   scale_x_continuous(n.breaks = 10) +
#   scale_y_continuous(n.breaks = 8) +
#   labs(x = "Days of the Year", y = bquote('KE' ~ (Joules))) +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(size = 18, color = "black"),
#     axis.text.y = element_text(size = 18, color = "black"),
#     axis.title.x = element_text(size = 20, color = "black"),
#     axis.title.y = element_text(size = 20, color = "black")
#   )
# p2
# 
# p3 <- ggplot(d_clean, aes(x = day, y = precipitation)) +
#   geom_point() +
#   geom_smooth(method = "loess") +
#   scale_x_continuous(n.breaks = 10) +
#   scale_y_continuous(n.breaks = 8) +
#   labs(x = "Days of the Year", y = bquote('Precipitation' ~ (mm/day))) +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(size = 18, color = "black"),
#     axis.text.y = element_text(size = 18, color = "black"),
#     axis.title.x = element_text(size = 20, color = "black"),
#     axis.title.y = element_text(size = 20, color = "black")
#   )
# p3
