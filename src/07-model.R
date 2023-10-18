# useful materials
# https://rpubs.com/mengxu/exponential-model
# https://online.stat.psu.edu/stat485/lesson/19/19.8

d <- read.csv("output/final_data.csv")

# per month aggregation
data <- aggregate(ke ~ id + county, data = d, FUN = sum)
data$stock <- aggregate(stock ~ id + county, data = d, FUN = first)$stock
plot(data$ke, data$stock)

# exponential model
theta0 <- min(data$stock) * 0.01
model0 <- lm(log(stock - theta0) ~ ke, data = data)
alpha0 <- exp(coef(model0)[1])
beta0 <- coef(model0)[2]
start <- list(alpha = alpha0, beta = beta0, theta = theta0)
model <- nls(stock ~ alpha * exp(beta * ke) + theta, data = data, start = start)

# interpreting
summary(model)

# predictions
yhat <- predict(model, list(ke = data$ke))
data_pred <- data.frame(ke = data$ke, stock = data$stock, yhat = yhat)
data_pred <- data_pred[order(data_pred$ke), ]
rownames(data_pred) <- NULL

# plot
plot(stock ~ ke, data = data_pred)
lines(data_pred$ke, data_pred$yhat, col = "red", lwd = 3)

# equation
# y = alpha * exp(beta * x) + theta
coef(model)
coef(model)[1]
coef(model)[2]
coef(model)[3]

# plot from equation
plot(data_pred$ke, data_pred$stock)
lines(data_pred$ke, 3.83 * exp(-723386098 * data_pred$ke) + 3.19, col = "red", lwd = 3)  


#################################
# exponential models per county
# https://stackoverflow.com/questions/72883317/plot-separate-curves-for-non-linear-nls-model-with-categorical-variable-and-se
#################################
data2 <- data[data$county %in% c("Arkansas", "Craighead"), ]
data2$county <- factor(data2$county)
ncounties <- length(unique(data2$county))
rownames(data2) <- NULL
theta0 <- min(data2$stock) * 0.01
model0 <- lm(log(stock - theta0) ~ ke, data = data2)
alpha0 <- exp(coef(model0)[1])
beta0 <- coef(model0)[2]
start <- list(alpha = rep(alpha0, ncounties), beta = rep(beta0, ncounties), theta = rep(theta0, ncounties))
model_county <- nls(stock ~ alpha * exp(beta[county] * ke) + theta, data = data2, start = start)
summary(model_county)

a <- coef(model_county)[c(1, 3)]
b <- coef(model_county)[c(2, 4)]
c <- coef(model_county)[c(3, 5)]
data2$fit <- a[data2$county] * exp(b[data2$county] * data2$ke) + c[data2$county]
ggplot(data2, aes(x = ke, y = stock, color = data2$county)) + 
  geom_point() + 
  geom_line(aes(y = fit)) + 
  theme_classic() + 
  labs(colour = "County")
  