## Seminar Day 2
# Import the dataset
cust.df <- read.csv("http://gOO.gl/PmPkaG")

# Check the structure of the data and some statistics
str(cust.df)
summary(cust.df$age)
median(cust.df$age)
quantile(cust.df$age, 0.5)

# Create a secondary variable, checking whether person is older than 35
cust.df$age35 <- ifelse(cust.df$age <= 35, "<=35", ">35")
# Factorize the variable
cust.df$age35 <- factor(cust.df$age35, ordered = TRUE)
levels(cust.df$age35)
