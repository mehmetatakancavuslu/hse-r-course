## Seminar Day7
## Logistics Regression
options(scipen = 999)

# Import data as a dataframe
acquisition <- read.csv("https://goo.gl/JG74jZ")
head(acquisition)

# Find theproportion of actually acquired customers
prop.table(table(acquisition$Acquisition))

# Build a logistic regression model
model1 <- glm(Acquisition ~ Acq_Expense + Acq_Expense_SQ + Industry +
                Revenue + Employees, family = binomial(link = "logit"),
              data = acquisition)
summary(model1)

# Save predicted probability and outcome for each prospect
acquisition$Acq_Prob <- model1$fitted.values
acquisition$Acq_Pred <- ifelse(model1$fitted.values > 0.5, 1, 0)

# Create a confusion matrix
confusionmatrix <- xtabs(~ acquisition$Acq_Pred + acquisition$Acquisition)
confusionmatrix

# Find accuracy
nrow(acquisition[(acquisition$Acq_Pred == 1 & acquisition$Acquisition == 1) |
              (acquisition$Acq_Pred == 0 & acquisition$Acquisition == 0),])/
  nrow(acquisition)
# Shorter way by using confusion matrix
(confusionmatrix[1,1] + confusionmatrix[2,2])/sum(confusionmatrix)

# Create a madeup dataframe with new prospects
newprospects <- data.frame(Acq_Expense = c(500, 500, 500),
                           Industry = c(0, 1, 0), Revenue = c(20, 30, 40),
                           Employees = c(400, 1000, 300))
newprospects$Acq_Expense_SQ <- newprospects$Acq_Expense^2
newprospects$Acq_Prob <- predict(model1, newprospects, type = "response")
newprospects$Acq_Pred <- ifelse(newprospects$Acq_Prob > 0.5, 1, 0)
