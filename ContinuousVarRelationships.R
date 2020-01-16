# Simulating Customer Data
set.seed(21821)
ncust <- 1000
cust.df <- data.frame(cust.id = as.factor(c(1:ncust)))
cust.df$age <- rnorm(n = ncust, mean = 35, sd = 5)
cust.df$credit.score <- rnorm(n = ncust, mean = 3 * cust.df$age + 620, sd = 50)
cust.df$email <- factor(sample(c("Yes", "No"), size = ncust, replace = TRUE,
                               prob = c(0.8, 0.2)))
# Creating distance vector with lognormal distribution
cust.df$distance.to.store <- exp(rnorm(n = ncust, mean = 2, sd = 1.2))
summary(cust.df)
# Creating online visit with negative binomial distribution
cust.df$online.visits <- rnbinom(ncust, size = 0.3,
                                 mu = 15 + ifelse(cust.df$email == "Yes", 15, 0)
                                 - 0.7 * (cust.df$age - median(cust.df$age)))
# Assume 30% chance of online transaction, with lognormal spendings
cust.df$online.trans <- rbinom(ncust, size = cust.df$online.visits, prob = 0.3)
cust.df$online.spend <- rlnorm(ncust, meanlog = 3,
                               sdlog = 0.1) * cust.df$online.trans
# Model in-store transaction with negative binomial and spending lognormally
cust.df$store.trans <- rnbinom(ncust, size = 5,
                               mu = 3 / sqrt(cust.df$distance.to.store))
cust.df$store.spend <- rlnorm(ncust, meanlog = 3.5,
                              sdlog = 0.4) * cust.df$store.trans
summary(cust.df)
# Generate survey responses
sat.overall <- rnorm(ncust, mean = 3.1, sd = 0.7)
summary(sat.overall)
# Create two satisfaction scores from this halo variable
sat.service <- floor(sat.overall + rnorm(ncust, mean = 0.5, sd = 0.4))
sat.selection <- floor(sat.overall + rnorm(ncust, mean = -0.2, sd = 0.6))
summary(cbind(sat.service, sat.selection))
# Floor above 5 to 5, and ceil below 1 to 1
sat.service[sat.service > 5] <- 5
sat.service[sat.service < 1] <- 1
sat.selection[sat.selection > 5] <- 5
sat.selection[sat.selection < 1] <- 1
summary(cbind(sat.service, sat.selection))
# Simulate no response data
no.response <- as.logical(rbinom(ncust, size = 1, prob = cust.df$age / 100))
sat.service[no.response] <- NA
sat.selection[no.response] <- NA
summary(cbind(sat.service, sat.selection))
# Add survey results to the main dataframe
cust.df$sat.service <- sat.service
cust.df$sat.selection <- sat.selection
summary(cust.df)
# Clean the environment
rm(ncust, sat.overall, sat.service, sat.selection, no.response)

# Data is ready, explore associations
str(cust.df)
plot(x = cust.df$age, y = cust.df$credit.score)
plot(x = cust.df$age, y = cust.df$credit.score, col = "blue",
     xlim = c(15, 55), ylim = c(500, 900),
     main = "Active Customers as of June 2014",
     xlab = "Customer Age (Years)", ylab = "Customer Credit Score")
abline(h = mean(cust.df$credit.score), col = "darkblue", lty = "dotted")
abline(v = mean(cust.df$age), col = "darkblue", lty = "dotted")
