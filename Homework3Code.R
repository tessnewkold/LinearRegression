library(DT)
library(investr)
url <- "https://bgreenwell.github.io/uc-bana7052/data/alumni.csv"
alumni <- read.csv(url)
DT::datatable(alumni)  # requires DT package

#   A   #
x1_under20 <- alumni$percent_of_classes_under_20
x2_studentFaculty <- alumni$student_faculty_ratio
y_giving <- alumni$alumni_giving_rate
fit3 <- lm(y_giving ~ x1_under20 + x2_studentFaculty, data = alumni)
fit3

#   B   #
under_20 <- 50
studentFaculty <- 10
predictedAlumniGiving <- 39.6556 + ((0.1662)*(under_20)) - ((1.7021)*(studentFaculty))
predictedAlumniGiving

#   C, D, E  #
summary(fit3)

#   F   #
cor(x1_under20, y_giving)
cor(x2_studentFaculty, y_giving)
