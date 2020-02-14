## Class Day3 - Linear Rregression
sat.df <- read.csv("http://goo.gl/HKnl74")
str(sat.df)

hist(sat.df$distance)
# Normalize to use in linear regression
sat.df$logdist <- log(sat.df$distance)
hist(sat.df$logdist)
# Check the pairs again
library(corrplot)
corrplot.mixed(cor(sat.df[, c(2, 4:9)]), upper="ellipse")

# Fitting a model with one predictor
plot(overall ~ rides, data = sat.df,
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")
lm(overall ~ rides, data = sat.df)
# If visitor gave 95 points for rides, what is the predicted overall sat.?
-94.962 + 1.703*95

# Create a lm object
m1 <- lm(formula = overall ~ rides, data = sat.df)
summary(m1)
plot(overall ~ rides, data = sat.df,
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")
abline(m1, col = "blue")

# Model assumptions
x <- rnorm(500)
y <- x^2 + rnorm(500)
toy.model <- lm(y ~ x)
# Plot early and often, see linear does not fit
plot(y ~ x)
abline(toy.model)

# Standard diagnostic plots (VERY IMPORTANT)
par(mfrow = c(2,2))
plot(m1)
