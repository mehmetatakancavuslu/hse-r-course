## Day 6 Assignment
install.packages("pander")
library(pander)
library(car)
data("Salaries")

## Task 1
hist(Salaries$salary)
qqnorm(Salaries$salary, pch = 1, frame = FALSE)
qqline(Salaries$salary, col = "steelblue", lwd = 2)
# Log transforming to normalize
Salaries$logSalary <- log(Salaries$salary)
hist(Salaries$logSalary)
qqnorm(Salaries$logSalary, pch = 1, frame = FALSE)
qqline(Salaries$logSalary, col = "steelblue", lwd = 2)

## Task 2
m1 <- lm(logSalary ~ sex + rank + discipline + yrs.since.phd, data = Salaries)
summary(m1)
pander(m1)
# Rank and discipline is statistically significant at 0.05 level

## Task 3
m2 <- lm(logSalary ~ sex + rank + discipline + yrs.since.phd + sex:yrs.since.phd,
         data = Salaries)
summary(m2)
# Not exactly, the new regressor has p-value of 0.7, so insigficant

## Task 4
library(coefplot)
coefplot(m1, intercept = FALSE, outerCI = 1.96, lwdOuter = 1.5,
         ylab = "Rating of Feature", xlab = "Association with LogSalary")

## Task 5
library(relaimpo)
calc.relimp(m1, type = c("lmg"), rela = TRUE)
# Create dummy variables
Salaries2 <- model.matrix(logSalary ~ sex + rank + discipline + yrs.since.phd,
                          data = Salaries)
Salaries2 <- as.data.frame(cbind(Salaries$salary, Salaries2))
colnames(Salaries2)[1] <- "salary"
Salaries2 <- Salaries2[, -2] # remove intercept
# Rebuild the regression
m3 <- lm(log(salary) ~ ., data = Salaries2)
summary(m3)
# Check relative importance with car metric
calc.relimp(m3, type = c("car"), rela = TRUE)
