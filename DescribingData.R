# Create the dataframe of initially missing values
k.stores <- 20
k.weeks <- 104
store.df <- data.frame(matrix(NA, ncol = 10, nrow = k.stores * k.weeks))
names(store.df) <- c("storeNum", "year", "week", "p1sales", "p2sales",
                     "p1price", "p2price", "p1prom", "p2prom", "country")

# Create the data to fill the dataframe
store.num <- 101:(100+k.stores)
store.cty <- c(rep("US", 3), rep("DE", 5), rep("GB", 3), rep("BR", 2),
               rep("JP", 4), rep("AU", 1), rep("CN", 2))
store.df$storeNum <- rep(store.num, each = k.weeks)
store.df$country <- rep(store.cty, each = k.weeks)
rm(store.num, store.cty)

# Do the same for week and year columns
store.df$week <- rep(1:52, times = k.stores * 2)
store.df$year <- rep(rep(1:2, each = k.weeks / 2), times = k.stores)
str(store.df)

# Categorizing (Factoring) the appropriate columns
store.df$storeNum <- factor(store.df$storeNum)
store.df$country <- factor(store.df$country)
str(store.df)
head(store.df)
tail(store.df)

# Filling the remaning data with randomized numbers
set.seed(98250)
store.df$p1prom <- rbinom(n = nrow(store.df), size = 1, prob = 0.1)
store.df$p2prom <- rbinom(n = nrow(store.df), size = 1, prob = 0.15)
head(store.df)
store.df$p1price <- sample(x = c(2.19, 2.29, 2.49, 2.79, 2.99),
                           size = nrow(store.df), replace = TRUE)
store.df$p2price <- sample(x = c(2.29, 2.49, 2.59, 2.99, 3.19),
                           size = nrow(store.df), replace = TRUE)
head(store.df)

# Randoming sales data using Poisson distribution
# First the default sales in the absence of promotion
tmp.sales1 <- rpois(n = nrow(store.df), lambda = 120)
tmp.sales2 <- rpois(n = nrow(store.df), lambda = 100)

# Then scale price according to log of price
tmp.sales1 <- tmp.sales1 * log(store.df$p2price) / log(store.df$p1price)
tmp.sales2 <- tmp.sales2 * log(store.df$p1price) / log(store.df$p2price)

# Final sales get 30% or 40% lift when promoted
store.df$p1sales <- floor(tmp.sales1 * (1 + store.df$p1prom * 0.3))
store.df$p2sales <- floor(tmp.sales2 * (1 + store.df$p2prom * 0.4))
head(store.df)

# We have created the dataset, now summarize it
table(store.df$p1price)
p1.table <- table(store.df$p1price)
plot(p1.table)
table(store.df$p1price, store.df$p1prom)
p1.table2 <- table(store.df$p1price, store.df$p1prom)
p1.table2[, 2] / (p1.table2[, 1] + p1.table2[, 2])

# Summorize the statistics in a distinct dataframe
mysummary.df <- data.frame(matrix(NA, nrow = 2, ncol = 2))
names(mysummary.df) <- c("Median Sales", "IQR")
rownames(mysummary.df) <- c("Product 1", "Product 2")
mysummary.df["Product 1", "Median Sales"] <- median(store.df$p1sales)
mysummary.df["Product 2", "Median Sales"] <- median(store.df$p2sales)
mysummary.df["Product 1", "IQR"] <- IQR(store.df$p1sales)
mysummary.df["Product 2", "IQR"] <- IQR(store.df$p2sales)
mysummary.df

# All around summary for the dataframe
summary(store.df)
summary(store.df$year)
summary(store.df, digits = 2)

# Describe with "psych" package
install.packages("psych")
library(psych)
describe(store.df)
describe(store.df[, c(2, 4:9)])

# Using apply() function
apply(store.df[, 2:9], MARGIN = 2, FUN = mean)
apply(store.df[, 2:9], 2, sum)

# Applying anonymous function
apply(store.df[, 2:9], 2, function(x) {mean(x) - median(x)})

# Visualization - Histogram
hist(store.df$p1sales, main = "Product 1 Weekly Sales Frequencies, All Stores",
     xlab = "Product 1 Sales (Units)", ylab = "Count",
     breaks = 30, col = "lightblue", freq = FALSE, xaxt = "n")
# Add customized x-axis
axis(side = 1, at = seq(60, 300, by = 20))
# Add smoothed density line
lines(density(store.df$p1sales, bw = 10), col = "darkred", lwd = 2)

# Boxplots
boxplot(store.df$p2sales, xlab = "Weekly Sales", ylab = "Product 2",
        main = "Weekly Sales of Product 2, All Stores", horizontal = TRUE)
boxplot(store.df$p2sales ~ store.df$storeNum, horizontal = TRUE,
        ylab = "Store", xlab = "Weekly Unit Sales", las = 1,
        main = "Weekly Sales of Product 2 by Stores")
boxplot(p2sales ~ p2prom, data = store.df, horizontal = TRUE, yaxt = "n",
        ylab = "Product 2 Promoted in Store?", xlab = "Weekly Sales",
        main = "Weekly Sales of Product 2 with and without Promotion")
axis(side = 2, at = c(1,2), labels = c("No", "Yes"))

# Check normality with QQPlot
qqnorm(store.df$p1sales)
qqline(store.df$p1sales)
# Check normality after log transformation
qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))

# ECDF
plot(ecdf(store.df$p1sales),
     main = "Cumulative Distribution of Product 1 Weekly Sales",
     ylab = "Cumulative Proportion",
     xlab = c("P1 Weekly Sales, All Stores", "90% of Weeks Sold <= 171 Units"),
     yaxt = "n")
axis(side = 2, at = seq(0, 1, by = 0.1), las = 1,
     labels = paste(seq(0, 100, by = 10), "%", sep = ""))
abline(h = 0.9, lty = 3)
abline(v = quantile(store.df$p1sales, pr = 0.9), lty = 3)

# by() and aggregate()
by(store.df$p1sales, store.df$storeNum, mean)
by(store.df$p1sales, list(store.df$storeNum, store.df$year), mean)
# by() is not useful for saving and reusing the data
aggregate(store.df$p1sales, by = list(country = store.df$country), sum)
p1sales.sum <- aggregate(list(sales = store.df$p1sales),
                         by = list(country = store.df$country), sum)

# Showing the data on the world map
install.packages(c("rworldmap", "RColorBrewer"))
library(rworldmap)
library(RColorBrewer)
p1sales.map <- joinCountryData2Map(p1sales.sum, joinCode = "ISO2",
                                   nameJoinColumn = "country")
mapCountryData(p1sales.map, nameColumnToPlot = "sales",
               mapTitle = "Total Product 1 Sales by Country",
               colourPalette = brewer.pal(7, "Greens"),
               catMethod = "fixedWidth", addLegend = FALSE)
