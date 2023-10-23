library(nlme)
library(ggplot2)

# useful materials
# https://online.stat.psu.edu/stat485/lesson/19/19.8
# https://www.zachrutledge.com/uploads/1/2/5/6/125679559/interpreting_regression_coefficients.pdf
# https://rpubs.com/mengxu/exponential-model

d <- read.csv("output/final_data.csv")

# per id-county aggregation
data <- aggregate(ke ~ id + county, data = d, FUN = sum)
data$stock <- aggregate(stock ~ id + county, data = d, FUN = function(x) head(x, 1))$stock
data$lon <- aggregate(lon ~ id + county, data = d, FUN = function(x) head(x, 1))$lon
data$lat <- aggregate(lat ~ id + county, data = d, FUN = function(x) head(x, 1))$lat
data <- data[data$ke > 0, ]
rownames(data) <- NULL
plot(data$ke, data$stock)
plot(data$ke, sqrt(data$stock))
plot(data$ke, log(data$stock))
plot(log(data$ke), log(data$stock))

# mixed model
m1 <- lme(log(stock) ~ log(ke), random = ~ 1 | county, data = data)
plot(m1)

# removing potential outliers and fitting again
re <- residuals(m1, type = "normalized")
data2 <- data[abs(re) < 2, ]
rownames(data2) <- NULL
m2 <- lme(log(stock) ~ log(ke), random = ~ 1 | county, data = data2)
coefs <- coef(summary(m2))
coefs

# interpretation
# %1 change in "ke" is associated with a b1% change in Y (when holding all other variables in the model)


# results
plot(m2)
summary(m2)
plot(data2$stock, exp(fitted(m2)))
cor(data2$stock, exp(fitted(m2)))

# checking residuals spatial autocorrelation
data2$residual <- residuals(m2, type = "normalized")
ggplot(data2, aes(x = lon, y = lat, color = residual)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw()

# binding predictions
b0 <- coefs[rownames(coefs) == "(Intercept)", "Value"]
b1 <- coefs[rownames(coefs) == "log(ke)", "Value"]
d$yhat <- exp(b0 + (b1 * log(d$ke)))
d_clean <- d[d$yhat != Inf, ]
rownames(d_clean) <- NULL

##############################################################

# PLOTS
p1 <- ggplot(d_clean, aes(x = day, y = yhat)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 8) +
  labs(x = "Days of the Year", y = bquote('Predicted SOC' ~ (kg/m ^ 2))) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 18, color = "black"),
    axis.title.x = element_text(size = 20, color = "black"),
    axis.title.y = element_text(size = 20, color = "black")
  )
p1

p2 <- ggplot(d_clean, aes(x = day, y = ke)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 8) +
  labs(x = "Days of the Year", y = bquote('KE' ~ (Joules))) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 18, color = "black"),
    axis.title.x = element_text(size = 20, color = "black"),
    axis.title.y = element_text(size = 20, color = "black")
  )
p2

p3 <- ggplot(d_clean, aes(x = day, y = precipitation)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 8) +
  labs(x = "Days of the Year", y = bquote('Precipitation' ~ (mm/day))) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 18, color = "black"),
    axis.title.x = element_text(size = 20, color = "black"),
    axis.title.y = element_text(size = 20, color = "black")
  )
p3
