## Day 7 Assignment
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

# Build model with significant variables
model2 <- glm(Acquisition ~ Acq_Expense + Acq_Expense_SQ + Revenue + Employees,
              family = binomial(link = "logit"),
              data = acquisition)
summary(model2)

# Save predicted probability and outcome for each prospect
acquisition$Acq_Prob <- model2$fitted.values
acquisition$Acq_Pred <- ifelse(model1$fitted.values > 0.5, 1, 0)

# Create a confusion matrix
confusionmatrix <- xtabs(~ acquisition$Acq_Pred + acquisition$Acquisition)
confusionmatrix

# Accuracy by using confusion matrix
(confusionmatrix[1,1] + confusionmatrix[2,2])/sum(confusionmatrix)

# Create a madeup dataframe with new prospects
newprospects <- data.frame(Acq_Expense = c(500, 500, 500),
                           Industry = c(0, 1, 0), Revenue = c(20, 30, 40),
                           Employees = c(400, 1000, 300))
newprospects$Acq_Expense_SQ <- newprospects$Acq_Expense^2

# Predictions on new dataset
newprospects$Acq_Prob <- predict(model1, newprospects, type = "response")
newprospects$Acq_Pred <- ifelse(newprospects$Acq_Prob > 0.5, 1, 0)
newprospects$Acq_Prob2 <- predict(model2, newprospects, type = "response")
newprospects$Acq_Pred2 <- ifelse(newprospects$Acq_Prob > 0.5, 1, 0)

# TASK 2
confusionmatrix[1,1] + confusionmatrix[2,2]

# TASK 3
acquired_expense <- seq(from = 0, to = 2000, by = 1)
summary(model2)
Z <- -26.1989 + 0.06726*acquired_expense - 0.00004*(acquired_expense^2) +
  0.03175*20 + 0.00496*20
plot(x = acquired_expense, y = Z)
temp <- data.frame(matrix(NA, nrow = length(Z), ncol = 2))
colnames(temp) <- c("acquired_expense", "Z")
temp$acquired_expense <- acquired_expense
temp$Z <- Z
temp[temp$Z == max(temp$Z),]
