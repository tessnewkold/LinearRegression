library(DT)
library(investr)
library(GGally)
library(dplyr)
url <- "https://bgreenwell.github.io/uc-bana7052/data/alumni.csv"
alumni <- read.csv(url)
DT::datatable(alumni)  # requires DT package


#   A   #
x1_under20 <- alumni$percent_of_classes_under_20
x2_studentFaculty <- alumni$student_faculty_ratio
y_giving <- alumni$alumni_giving_rate
fit4 <- lm(y_giving ~ x1_under20 + x2_studentFaculty, data = alumni)
summary(fit4)


sigma(fit4)^2
rstud <- rstandard(fit4)
GGally::ggpairs(data = m.data)
aug_fit3 <- fit4 %>% broom::augment() %>% mutate(row_num = 1:n())
aug_fit3

# Plots for l.model without the quadratic term ---------------------------------------------------

ggplot(data = aug_fit3, aes(x = .fitted,y = .std.resid)) + geom_point() + geom_smooth(se = FALSE) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2", size = 1) + 
  geom_hline(yintercept = c(-2, 2), linetype = "dotted", size = 1) +
  xlab("Fitted value") + ylab("Standardized residual") + ggtitle("Standardized Residuals vs Fitted Values") + 
  theme(plot.title = element_text(hjust = 0.5))



# QQ Plot
ggplot(data = aug_fit3, aes(sample = .std.resid)) +
  geom_qq() +
  geom_qq_line(linetype = "dashed", color = "red2") +
  xlab("Theoretical quantile") +
  ylab("Sample quantile") + ggtitle("Q-Q Plot") + 
  theme(plot.title = element_text(hjust = 0.5))


#Sr vs index1
ggplot(data = aug_fit3, aes(x = row_num, y = .std.resid)) + 
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  xlab("Index1") +
  ylab("Standardized residual")  + ggtitle(" Standardized Residuals vs Index") + 
  theme(plot.title = element_text(hjust = 0.5))

# Sr vs predictor

ggplot(data = aug_fit3, aes(x = x1_under20, y = .std.resid)) + 
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
  geom_smooth(color = "forestgreen", alpha = 0.1, se = FALSE) +
  ylab("Standardized residual") + ggtitle(" Standardized Residuals vs Predictor1") + 
  theme(plot.title = element_text(hjust = 0.5))

install.packages("car")
library(car)
vif(fit3)


under_20 <- 40
studentFaculty <- 5
predictedAlumniGiving <- 39.6556 + ((0.1662)*(under_20)) - ((1.7021)*(studentFaculty))
predictedAlumniGiving

