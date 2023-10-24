library(nlme)
library(ggplot2)

# useful materials
# https://online.stat.psu.edu/stat485/lesson/19/19.8
# https://www.zachrutledge.com/uploads/1/2/5/6/125679559/interpreting_regression_coefficients.pdf
# https://library.virginia.edu/data/articles/interpreting-log-transformations-in-a-linear-model
# https://rpubs.com/mengxu/exponential-model

agg <- function(data) {
  # per id-county aggregation
  
  # sum of energy
  data_agg <- aggregate(ke ~ id + county, data = data, FUN = sum)
  
  # just first value (because all values are the same anyway)
  data_agg$soc <- aggregate(soc ~ id + county, data = data, FUN = function(x) head(x, 1))$soc
  data_agg$lon <- aggregate(lon ~ id + county, data = data, FUN = function(x) head(x, 1))$lon
  data_agg$lat <- aggregate(lat ~ id + county, data = data, FUN = function(x) head(x, 1))$lat
  data_agg <- data_agg[data_agg$ke > 0, ]
  rownames(data_agg) <- NULL
  return(data_agg)
}

# ARKANSAS
data_ar <- read.csv("output/Arkansas/final_data.csv")
data_ar_agg <- agg(data_ar)

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
data_ar_agg2 <- data_ar_agg[abs(re) < 2, ]
rownames(data_ar_agg2) <- NULL
m2 <- lme(log(soc) ~ log(ke), random = ~ 1 | county, data = data_ar_agg2)
p3 <- plot(m2)
p3

# plot(m2)
summary(m2)
cor(data_ar_agg2$soc, fitted(m2))
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

##########################################################################
# second model is an attempt to extrapolate predictions to another region
##########################################################################
m3 <- lm(log(soc) ~ log(ke), data = data_ar_agg)
summary(m3)
# removing potential outliers and fitting again
re <- rstandard(m3)
data_ar_agg3 <- data_ar_agg[abs(re) < 2, ]
rownames(data_ar_agg3) <- NULL
m4 <- lm(log(soc) ~ log(ke), data = data_ar_agg3)
coefs <- coef(summary(m4))
summary(m4)
coefs
plot(data_ar_agg3$soc, exp(fitted(m4)))
cor(data_ar_agg3$soc, exp(fitted(m4)))

############################################################################

# CALIFORNIA
# plot(data_ca_agg$ke, data_ca_agg$soc)
# plot(data_ca_agg$ke, sqrt(data_ca_agg$soc))
# plot(data_ca_agg$ke, log(data_ca_agg$soc))
# plot(log(data_ca_agg$ke), log(data_ca_agg$soc))

data_ca_agg$yhat <- exp(predict(m4, newdata = data_ca_agg))
plot(data_ca_agg$soc, data_ca_agg$yhat)
cor(data_ca_agg$soc, data_ca_agg$yhat)
p <- ggplot(data_ca_agg, aes(x = soc, y = yhat)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(linetype = "dashed") +
  coord_cartesian(expand = F, xlim = c(0, NA), ylim = c(0, NA)) +
  labs(x = "SOC", y = "Predicted SOC") +
  theme_bw()
p
# r <- max(abs(layer_scales(p)$x$range$range))
# s <- max(abs(layer_scales(p)$y$range$range))
# t <- round(max(r,s),1)
# p <- p + coord_equal(xlim = c(0, t), ylim = c(0, t))


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
