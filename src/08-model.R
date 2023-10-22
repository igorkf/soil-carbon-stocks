library(nlme)
library(ggplot2)

# useful materials
# https://rpubs.com/mengxu/exponential-model
# https://online.stat.psu.edu/stat485/lesson/19/19.8

d <- read.csv("output/final_data.csv")

# per month aggregation
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
plot(m2)
summary(m2)
plot(data2$stock, exp(fitted(m2)))
cor(data2$stock, exp(fitted(m2)))

# checking residuals spatial autocorrelation
data2$residual <- residuals(m2, type = "normalized")
ggplot(data2, aes(x = lon, y = lat, color = residual)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_bw()


# just sketch (don't need to run)
# exponential model
# theta0 <- min(data$stock) * 0.1
# model0 <- lm(log(stock - theta0) ~ ke, data = data)
# alpha0 <- exp(coef(model0)[1])
# beta0 <- coef(model0)[2]
# start <- list(alpha = alpha0, beta = beta0, theta = theta0)
# model <- nls(stock ~ alpha * exp(beta * ke) + theta, data = data, start = start)

# interpreting
# summary(model)

# predictions
# yhat <- predict(model, list(ke = data$ke))
# data_pred <- data.frame(ke = data$ke, stock = data$stock, yhat = yhat)
# data_pred <- data_pred[order(data_pred$ke), ]
# rownames(data_pred) <- NULL

# plot
# plot(stock ~ ke, data = data_pred)
# lines(data_pred$ke, data_pred$yhat, col = "red", lwd = 3)

# equation
# y = alpha * exp(beta * x) + theta
# coef(model)
# coef(model)[1]
# coef(model)[2]
# coef(model)[3]

# plot from equation
# plot(data_pred$ke, data_pred$stock)
# lines(data_pred$ke, 3.83 * exp(-723386098 * data_pred$ke) + 3.19, col = "red", lwd = 3)  