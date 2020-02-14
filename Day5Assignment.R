## Day5 Assignment
library(car)
data(Salaries)

# Task 1
median(Salaries[Salaries$sex == "Female",]$salary)

# Task 2
median(Salaries[Salaries$sex == "Male",]$salary)

# Task 3
boxplot(salary ~ sex, data = Salaries, horizontal = TRUE)

# Task 4
tapply(Salaries$salary,Salaries$sex,summary)

# Task 5
Salaries$salary.high <- ifelse(Salaries$salary > median(Salaries$salary),
                               "Yes", "No")

# Task 6
prop.table(table(Salaries$salary.high, Salaries$sex), margin = 2)

# Task 7
plot(Salaries$yrs.service, Salaries$salary, main = "Salary vs Years of Service",
     ylab = "Salary", xlab = "Years of Service")

# Task 8
cor.test(Salaries[Salaries$sex == "Male",]$salary,
         Salaries[Salaries$sex == "Male",]$yrs.service)
cor.test(Salaries[Salaries$sex == "Female",]$salary,
         Salaries[Salaries$sex == "Female",]$yrs.service)

# Task 9
## Both are significant since p-values are lower than 0.05

# Task 10
nrow(Salaries[Salaries$yrs.service < 10 & Salaries$salary > 100000,])
