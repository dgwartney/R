# Statistics One, 2013, Lab 4

# Lab goals
#   Read a datafile into R
#   Print summary statistics
#   Conduct correlation analyses
#   Conduct regression analyses, unstandardized
#   Conduct regression analyses, standardized

# Example
#   A correlational study investigating predictors of physcial endurance in adults
#     Outcome variable (Y) is physical endurance
#     Predictors (X) are age and number of years actively engaged in exercise/sports
#     Sample size is N = 200

# Check your working directory
# getwd()
# If necessary, set your working directory
# setwd("/Volumes/Conway/R")

# If necessary, install packages
# install.packages("psych")

# Load packages
library(psych)

# Read data into a dataframe called PE (physical endurance)
PE <- read.table("Stats1.13.HW.04.txt", header = T)

# If you want to view the data
#View(PE)
#edit(PE)

# Summary statistics
describe(PE) 

# Correlation analysis 
cor(PE[2:4]) # Omit column 1 because it contains participant id numbers

round(cor(PE[2:4]), 2) # Round to 2 decimal places 

cor.test(PE$salary, PE$courses)
cor.test(PE$salary, PE$years)

# Histograms
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
hist(PE$courses)
hist(PE$years)
hist(PE$salary)
layout(matrix(c(1,1), 1, 1, byrow = TRUE))
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
boxplot(PE$courses)
boxplot(PE$years)
boxplot(PE$salary)
layout(matrix(c(1,1), 1, 1, byrow = TRUE))


# Regression analyses, unstandardized
model1 <- lm(PE$salary ~ PE$years)
summary(model1)
plot(PE$salary ~ PE$years, main = "Scatterplot", ylab = "Salary", xlab = "Years")
abline(lm(PE$salary ~ PE$years), col="blue")

model2 <- lm(PE$salary ~ PE$courses)
summary(model2)
plot(PE$salary ~ PE$courses, main = "Scatterplot", ylab = "Salary", xlab = "Courses")
abline(lm(PE$salary ~ PE$years), col="blue")

model3 <- lm(PE$salary ~ PE$years + PE$courses)
summary(model3)

# To visualize model3, save the predicted scores as a new variable and then plot with endurance
PE$predicted <- fitted(model3)
mean(PE$predicted)

plot(PE$salary ~ PE$predicted, main = "Scatterplot", ylab = "Salary", xlab = "Model 3 Predicted Scores")
abline(lm(PE$salary ~ PE$predicted), col="blue")

# The function fitted returns predicted scores whereas the function resid returns residuals
PE$e <- resid(model3)
mean(PE$e)

hist(PE$e)
plot(PE$predicted ~ PE$e, main = "Scatterplot", ylab = "Model 3 Predicted Scores", xlab = "Model 3 Residuals")
abline(lm(PE$predicted ~ PE$e), col="blue")

# Regression analyses, standardized
# In simple regression, the standardized regression coefficient will be the same as the correlation coefficient

round(cor(PE[2:4]), 2) # Round to 2 decimal places 

model1.z <- lm(scale(PE$salary) ~ scale(PE$years))
summary(model1.z)

model2.z <- lm(scale(PE$salary) ~ scale(PE$courses))
summary(model2.z)

model3.z <- lm(scale(PE$salary) ~ scale(PE$courses) + scale(PE$years))
summary(model3.z)

