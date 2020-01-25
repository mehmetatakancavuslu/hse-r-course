## Seminar Assignment 2
library(car)
data(Salaries)

## Task 1
aggregate(salary ~ rank + sex, data = Salaries, mean)

## Task 2
library(lattice)
bwplot(salary ~ rank | sex, data = Salaries, layout = c(2,1))

## Task 3
cross_tab <- with(Salaries, table(rank, sex))
chisq.test(cross_tab)
# p-value is lover than 0.05, so we reject the null hypothesis (In 95%)
# So, propotion of women differs by profession

## Task 4
aggregate(salary ~ sex, data = Salaries, mean)

## Task 5
aov.inc.seg <- aov(salary ~ sex, data = Salaries)
anova(aov.inc.seg)
# P value is 0.006 < 0.05 which means we reject the null hypothesis
# Which means salaries are not same for different genders

## Task 6
t.test(salary ~ sex, data = Salaries)
# It is same for t-test, p-value = 0.003 < 0.05

## Task 7
library(multcomp)
aov.inc.seg <- aov(salary ~ - 1 + sex, data = Salaries)
by.seg <- glht(aov.inc.seg)
par(mar = c(5, 10, 5, 5))
plot(by.seg, xlab = "Salary", main = "Mean Salary by Sex (95% CI)")
