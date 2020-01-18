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

# Cross tabulation
table(cust.df$age35, cust.df$email)
prop.table(table(cust.df$age35, cust.df$email), margin = 1) * 100
prop.table(table(cust.df$age35, cust.df$email), margin = 2) * 100
xtabs(~ cust.df$age35 + cust.df$email)

# Summary statistics of data subsets
aggregate(store.spend ~ age35 + email, data = cust.df, mean)
aggregate(online.spend ~ age35 + email, data = cust.df, mean)
aggregate(cbind(store.spend, online.spend) ~ age35 + email,
          data = cust.df, mean)

# Boxplot
boxplot(online.spend ~ age35, data = cust.df, horizontal = TRUE,
        main = "Online spendings distribution between age groups",
        xlab = "Online Spend ($)", ylab = "Age Groups")

# Correlations - Pearson, Kendall, Spearman
cor(cust.df[, c("age", "sat.service", "sat.selection")], use = "complete.obs")
cor(cust.df[, c("age", "sat.service", "sat.selection")], use = "complete.obs",
    method = "kendall")
cor(cust.df[, c("age", "sat.service", "sat.selection")], use = "complete.obs",
    method = "spearman")

# Correlation tests
cor.test(cust.df$age, cust.df$sat.service)
