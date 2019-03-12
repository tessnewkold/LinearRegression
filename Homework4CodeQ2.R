
# LR: Hw4 -----------------------------------------------------------------
library(GGally)
library(dplyr)
set.seed(7052)
x1 <- rnorm(100,3,0.5)
x2 <- (x1)^2
y <- rnorm(100,10+5*x1-2*x2,0.5)
m.data <- data.frame(x1,y) 
l.model<- lm(y~x1, m.data)
summary(l.model)
#Mean Sq Error:
sigma(l.model)^2
r.stud <- rstandard(l.model)
GGally::ggpairs(data = m.data)
aug.l.model<- l.model %>% broom::augment() %>% mutate(row_num=1:n())
aug.l.model
# Plots for l.model without the quadratic term ---------------------------------------------------

ggplot(data=aug.l.model, aes(x=.fitted,y=.std.resid))+geom_point() + geom_smooth(se=FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2", size=1) +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted", size=1) +
  xlab("Fitted value") + ylab("Standardized residual") + ggtitle("Standardized Residuals vs Fitted Values")+
  theme(plot.title = element_text(hjust = 0.5))

# QQ Plot
ggplot(data=aug.l.model, aes(sample = .std.resid)) +
  geom_qq() +
  geom_qq_line(linetype = "dashed", color = "red2") +
  xlab("Theoretical quantile") +
  ylab("Sample quantile") + ggtitle("Q-Q Plot")+
  theme(plot.title = element_text(hjust = 0.5))


#Sr vs index1
ggplot(data=aug.l.model, aes(x = row_num, y = .std.resid)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  xlab("Index1") +
  ylab("Standardized residual")  + ggtitle(" Standardized Residuals vs Index")+
  theme(plot.title = element_text(hjust = 0.5))

# Sr vs predictor

ggplot(data=aug.l.model, aes(x = x1, y = .std.resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
  geom_smooth(color = "forestgreen", alpha = 0.1, se = FALSE) +
  ylab("Standardized residual") +ggtitle(" Standardized Residuals vs Predictor1")+
  theme(plot.title = element_text(hjust = 0.5))


# Including the Quadratic Term ---------------------------------------------------
set.seed(7052)
m.data <- data.frame(x1,x2,y) 
l.model<- lm(y~x1+I(x2), m.data)
summary(l.model)
#Mean Sq Error:
sigma(l.model)^2
r.stud <- rstandard(l.model)
GGally::ggpairs(data = m.data)
aug.l.model<- l.model %>% broom::augment() %>% mutate(row_num=1:n())
aug.l.model
# Standardized Residuals vs Fitted Values ---------------------------------------------------

ggplot(data=aug.l.model, aes(x=.fitted,y=.std.resid))+geom_point() + geom_smooth(se=FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2", size=1) +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted", size=1) +
  xlab("Fitted value") + ylab("Standardized residual") + ggtitle("Standardized Residuals vs Fitted Values")+
  theme(plot.title = element_text(hjust = 0.5))

# QQ Plot
ggplot(data=aug.l.model, aes(sample = .std.resid)) +
  geom_qq() +
  geom_qq_line(linetype = "dashed", color = "red2") +
  xlab("Theoretical quantile") +
  ylab("Sample quantile") + ggtitle("Q-Q Plot")+
  theme(plot.title = element_text(hjust = 0.5))

#Sr vs index1
ggplot(data=aug.l.model, aes(x = row_num, y = .std.resid)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  xlab("Index1") +
  ylab("Standardized residual")  + ggtitle(" Standardized Residuals vs Index")+
  theme(plot.title = element_text(hjust = 0.5))

# Sr vs predictor

ggplot(data=aug.l.model, aes(x = x2, y = .std.resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red2") +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted") +
  geom_smooth(color = "forestgreen", alpha = 0.1, se = FALSE) +
  ylab("Standardized residual") +ggtitle(" Standardized Residuals vs Predictor2")+
  theme(plot.title = element_text(hjust = 0.5))

library("car")
vif(l.model)  
cor(m.data)

fit_centered <- lm(y ~ I(x1 - mean(x1)) + I((x1 - mean(x1))^2), data = m.data)
vif(fit_centered)
x1c <- x1-mean(x1)
x2c<- (x1-mean(x1))^2
plot(x2~x1, main="X vs X^2 - Without Centering")
plot(x2c~x1c, main="X vs X^2 - After Centering")
cor(x2c,x1c)
