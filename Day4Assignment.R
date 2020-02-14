## Day 4 Assignment
par(mfrow = c(1,1))

## Task 1
# Basic data pre-processing and summary
library(car)
data("Salaries")
summary(Salaries)
# Check Salary if normal, if not normalize
hist(Salaries$salary, breaks = 30)
# Use Box-Cox to normalize to be used in linear model
lambda <- coef(powerTransform(Salaries$salary))
Salaries$transformedSalary <- bcPower(Salaries$salary, lambda)
hist(Salaries$transformedSalary, breaks = 30)

## Task 2
model <- lm(transformedSalary ~ sex + rank + discipline + yrs.since.phd,
            data = Salaries)
summary(model)
# Rank and discipline are statistically significant since their p-value is
# lower than our confidence interval of 0.01

## Task 3
install.packages("coefplot")
library(coefplot)
coefplot(model, intercept = F, outerCI = 1.96, lwdOuter = 2,
         ylab = "Rating of Feature", xlab = "Association with Salary")

## Task 4
coefplot(model, intercept = F, outerCI = 1.96, lwdOuter = 2,
         ylab = "Rating of Feature", xlab = "Association with Salary",
         coefficients = c("disciplineB", "rankProf", "rankAssocProf"))
coefplot(model, intercept = F, outerCI = 1.96, lwdOuter = 2,
         ylab = "Rating of Feature", xlab = "Association with Salary",
         predictors = c("discipline", "rank"))

## Task 5
model2 <- lm(transformedSalary ~ rank + discipline,
             data = Salaries)
summary(model)
# R-squared:  0.5322,	Adjusted R-squared:  0.5262
summary(model2)
# R-squared:  0.5293,	Adjusted R-squared:  0.5257
anova(model, model2)
## Task 6
par(mfrow = c(2,2))
plot(model)
plot(model2)
