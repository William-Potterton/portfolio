# Data Analysis Project
# POL 20050: Research Methods in Political Science
# 19403336: Eimear Lawless

load("~/R - Studio files/project (3).RData")
View(project)

# Part 1 Question 1b
# Gender and Party affiliation chi squared test
table(project$gender, project$partyid3)
chisq.test(project$gender, project$partyid3, correct=FALSE)

# Part 1 Question 2
# Party affiliation and opinion on Marijuana legalisation 
table(project$partyid3, project$grass)
chisq.test(project$partyid3, project$grass, correct=FALSE)

# Part 2 Question 1b
# Gender and socioeconomic status
t.test(sei ~ gender, data = project)

# Part 2 Question 2
# Race (white or black) and socioeconomic status
t.test(sei ~ race2, data = project)

# Part 2 Question 3
# Education level (low or high) and socioeconomic status
t.test(sei ~ educ2, data = project)

# Part 3 Question 1
x <- project$educ
y <- project$sei
plot(x, y, main = "Years of Education vs Socioeconomic status",
     xlab = "Years of Education", ylab = "Socioeconomic status",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = mtcars), col = "blue")

# Part 3 Question 2
# Pearsons correlation x=educ, y=sei
correlation <- cor(project$educ, project$sei, method = 'pearson', use = "complete.obs")
correlation

# Part 4 Question 1
# Regression analysis - scatter plot
scatter.smooth(x=project$educ, y = project$sei, main="sei ~ educ")

# Part 4 Question 2
# Regression model - sei ~ educ
EducLevel <- lm(sei~educ, data = project)
print(EducLevel)
summary(EducLevel)

# Linear model
linear_model <- lm(sei~educ, data = project)
print(linear_model)
# Diagnosis
summary(linear_model)
# Standard Error
Model_Summary <- summary(linear_model)
Model_Coefficients <- Model_Summary$coefficients
std_error <- Model_Coefficients["educ", "Std. Error"]
print(std_error)
# F-statistic
f_stat <- summary(linear_model)$fstatistic
f_stat

# Part 5 Question 1
# Regression model - sei ~ childs
NoOfChildren <- lm(sei~childs, data = project)
print(NoOfChildren)
summary(NoOfChildren)

# Part 5 Question 2
# Regression model - sei ~ tvhours
AvgTvTime <- lm(sei~tvhours, data = project)
print(AvgTvTime)
summary(AvgTvTime)

# Part 5 Question 3
# Multiple linear regression model - sei ~ childs and tvhours
NoOfChildren_AvgTvTime <- lm(project$sei ~ project$childs + project$tvhours, data=project)
print(NoOfChildren_AvgTvTime)
summary(NoOfChildren_AvgTvTime)
