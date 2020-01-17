# Simulating Customer Data
set.seed(21821)
ncust <- 1000
cust.df <- data.frame(cust.id = as.factor(c(1:ncust)))
cust.df$age <- rnorm(n = ncust, mean = 35, sd = 5)
cust.df$credit.score <- rnorm(n = ncust, mean = 3 * cust.df$age + 620, sd = 50)
cust.df$email <- factor(sample(c("yes", "no"), size = ncust, replace = TRUE,
                               prob = c(0.8, 0.2)))
# Creating distance vector with lognormal distribution
cust.df$distance.to.store <- exp(rnorm(n = ncust, mean = 2, sd = 1.2))
summary(cust.df)
# Creating online visit with negative binomial distribution
cust.df$online.visits <- rnbinom(ncust, size = 0.3,
                                 mu = 15 + ifelse(cust.df$email == "yes", 15, 0)
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
# Do online buyers buy less in store?
plot(cust.df$store.spend, cust.df$online.spend,
     main = "Customers as of June 2014",
     xlab = "Prior 12 months in-store sales ($)",
     ylab = "Prior 12 months online sales ($)", cex = 0.7)
hist(cust.df$store.spend, breaks = (0:ceiling(max(cust.df$store.spend) / 10)) * 10,
     main = "Customers as of June 2014",
     xlab = "Prior 12 months in-store sales ($)",
     ylab = "Count of Customers")
# Color coding email campaign in overal sales scatter plot
my.col <- c("black", "green3")
my.pch <- c(1, 19) # Solid and open circles
head(cust.df$email)
as.numeric(head(cust.df$email))
my.col[as.numeric(head(cust.df$email))]
my.col[head(cust.df$email)]
plot(cust.df$store.spend, cust.df$online.spend, cex = 0.7,
     col = my.col[cust.df$email], pch = my.pch[cust.df$email],
     main = "Customers as of June 2014",
     xlab = "Prior 12 months in-store sales ($)",
     ylab = "Prior 12 months online sales ($)")
# Adding legend
legend(x = "topright", legend = paste("email on file:", levels(cust.df$email)),
       col = my.col, pch = my.pch)
# Logorithmic scaling to get a better view
plot(cust.df$store.spend + 1, cust.df$online.spend + 1, cex = 0.7, log = "xy",
     col = my.col[cust.df$email], pch = my.pch[cust.df$email],
     main = "Customers as of June 2014",
     xlab = "Prior 12 months in-store sales ($)",
     ylab = "Prior 12 months online sales ($)")
legend(x = "topright", legend = paste("email on file:", levels(cust.df$email)),
       col = my.col, pch = my.pch)

# Multiple plots in the same graphical object
par(mfrow = c(2,2))
plot(cust.df$distance.to.store, cust.df$store.spend, main = "store")
plot(cust.df$distance.to.store, cust.df$online.spend, main = "online")
plot(cust.df$distance.to.store, cust.df$store.spend + 1, log = "xy",
     main="store, log")
plot(cust.df$distance.to.store, cust.df$online.spend + 1, log = "xy",
     main="online, log")
par(mfrow=c(1,1))

# Scatterplot Matrices
pairs(formula = ~ age + credit.score + email + distance.to.store +
        online.visits + online.trans + online.spend + store.trans + store.spend,
      data = cust.df)
# scatterPlotMatrix using "car" package
install.packages("car")
library(car)
scatterplotMatrix(formula = ~ age + credit.score + email + distance.to.store +
                    online.visits + online.trans + online.spend + store.trans +
                    store.spend, data = cust.df, diagonal = "histogram")
install.packages("gpairs")
library(gpairs)
gpairs(cust.df[, c(2:10)])

# Instead of plots, covariance can be used
cov(cust.df$age, cust.df$credit.score)
# Scaled version, pearson-correlation
cor(cust.df$age, cust.df$credit.score)
# According to Cohen's Rules of Thumb, in social sciences (normal dist):
# r = 0.1 -> Weak correlation
# r = 0.3 -> Medium correlation
# r > 0.5 -> Strong correlation
# Check if the correlation is statistically significant
cor.test(cust.df$age, cust.df$credit.score)
# Correlation matrix
cor(cust.df[, c(2, 3, 5:12)])
# Rather than numbers, corrplot offers nice view
install.packages("corrplot")
installed.packages("gplots")
library(corrplot)
corrplot(corr = cor(cust.df[, c(2, 3, 5:12)], use = "complete.obs"),
         upper = "ellipse", tl.pos = "lt")

# Transforming variables before calculating correlations
cor(cust.df$distance.to.store, cust.df$store.spend)
cor(1/cust.df$distance.to.store, cust.df$store.spend)
plot(cust.df$distance.to.store, cust.df$store.spend)
plot(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)

# Box-Cox Transformations
library(car)
powerTransform(cust.df$distance.to.store)
lambda <- coef(1/powerTransform(cust.df$distance.to.store))
bcPower(cust.df$distance.to.store, lambda)
par(mfrow = c(1, 2))
hist(cust.df$distance.to.store, xlab = "Distance to nearest store",
     ylab = "Count of customers", main = "Original Distribution")
hist(bcPower(cust.df$distance.to.store, lambda), ylab = "Count of customers",
     xlab = "Box-Cox transform of distance", main = "Transformed Distribution")

# Check correlations with transformed variables
l.dist <- coef(powerTransform(cust.df$distance.to.store))
l.spend <- coef(powerTransform(cust.df$store.spend + 1))
cor(bcPower(cust.df$distance.to.store, l.dist),
    bcPower(cust.df$store.spend + 1, l.spend))

# Associations in survey responses
par(mfrow = c(1, 1))
plot(cust.df$sat.service, cust.df$sat.selection,
     xlab = "Customer satisfaction with services",
     ylab = "Custoemr satisfaction with selections",
     main = "Customers as of June 2014")
