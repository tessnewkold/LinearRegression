set.seed(7052)
x <- rnorm(100, 2, 0.1)
error <- rnorm(100, 0, 0.5)
Y <- (10 + (5*x) + error)


summary(x)
summary(Y)
boxplot(x, main = "Boxplot of Predictor Variable (x)")
boxplot(Y, main = "Boxplot of Response Variable (Y)")
cor(Y,x)
plot(x, Y, main = "Scatter Plot")


fit <- lm(Y~x)
summary(fit)
abline(fit)


xbar <- mean(x)
ybar <- mean(Y)
xbar
ybar

plot(x, Y, main = "Scatter Plot", xlim = c(1.7, 2.3), ylim = c(18, 22))
par(new = TRUE)
plot(xbar, ybar, col = 'red', xlab = '', ylab = '', pch = 22, bg = 'red', xlim = c(1.7, 2.3), ylim = c(18, 22))
abline(fit, col = 'blue')


