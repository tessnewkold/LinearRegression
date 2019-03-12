library(DT)
library(investr)
library(GGally)
library(dplyr)
url <- "https://bgreenwell.github.io/uc-bana7052/data/alumni.csv"
alumni <- read.csv(url)
DT::datatable(alumni)  # requires DT package

x1_under20 <- alumni$percent_of_classes_under_20
x2_studentFaculty <- alumni$student_faculty_ratio
x3_private <- alumni$private
y_giving <- alumni$alumni_giving_rate
fit5 <- lm(y_giving ~ x1_under20 + x2_studentFaculty + private, data = alumni)
fit5

library(leaps)
a1 <- regsubsets(y ~ ., data = alumni, nbest = 6, nvmax = 4)



summary(fit5)
summary(x1_under20)
summary(x2_studentFaculty)
#summary(x3_private) - not valuable 

hist(x1_under20, main = "Histogram of x1 - Classes under 20 Students")
hist(x2_studentFaculty, main = "Histogram of x2 - Student/Facutly Ratio")
hist(x3_private, main = "Histogram of x3 - Private/Public")
hist(y_giving, main = "Histogram of Y - Giving Rate")

boxplot(x1_under20, main = "Boxplot of X1 Predictor Variable")
boxplot(x2_studentFaculty, main = "Boxplot of X2 Predictor Variable")
boxplot(x3_private, main = "Boxplot of X3 Predictor Variable")
boxplot(y_giving, main = "Boxplot of Response Variable Y")

plot(x1_under20, y_giving, main = "Scatter Plot")
plot(x2_studentFaculty, y_giving, main = "Scatter Plot")
plot(x3_private, y_giving, main = "Scatter Plot")

cor(x1_under20,y_giving)
cor(x2_studentFaculty,y_giving)
cor(x3_private,y_giving)


anova(fit5)
summary(fit5)$r.squared






















