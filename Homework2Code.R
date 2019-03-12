library(DT)
library(investr)
url <- "https://bgreenwell.github.io/uc-bana7052/data/alumni.csv"
alumni <- read.csv(url)
DT::datatable(alumni)  # requires DT package

#   A   #
x_under20 <- alumni$percent_of_classes_under_20
y_giving <- alumni$alumni_giving_rate
plot(x_under20, y_giving)
fit <- lm(y_giving ~ x_under20, data = alumni)
coef(fit)
summary(fit)

#   B   #
anova(fit)

#   C   #
summary(fit)$r.squared

#   D   #
cor(x_under20, y_giving)

#   E   #
xbar <- mean(alumni$percent_of_classes_under_20)
xbar
ybar <- mean(alumni$alumni_giving_rate)
ybar

plot(x_under20, y_giving)
abline(fit)

plotFit(fit, interval = "confidence", 
        data = data.frame(x_under20 = x_under20,
                          y_giving = y_giving), 
        cex = 1.4, shade = TRUE,
        col.conf = adjustcolor("red2", alpha.f = 0.5),
        main = "Giving in Relation to Class Size",
        ylab = "Percent of Giving",
        xlab = "Percent of Classes Under 20 Students")


