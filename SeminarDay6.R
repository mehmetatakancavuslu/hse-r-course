## Seminar day 6
## Regression Analysis

sat.df <- read.csv("http://goo.gl/HKnl74")

# Scaling the variables except distance such that
# mean is 0 and std is 1, with normal distribution
sat.std <- sat.df[, -3]
sat.std[, 3:7] <- scale(sat.std[, 3:7])
# Adding distance as applied log
sat.std$logdist <- log(sat.df$distance)
head(sat.std)
summary(sat.std)

## Is number of child is really a numerical variable?
## It can be also considered as having kids or not, maybe?
sat.std$num.child.factor <- factor(sat.std$num.child)

# Creating a linear model
m4 <- lm(overall ~ rides + games + wait + clean + weekend + logdist +
           num.child.factor, data = sat.std)
summary(m4)
# Convert child factor to yes and no
sat.std$has.child <- factor(sat.std$num.child > 0)
# Remodel again
m5 <- lm(overall ~ rides + games + wait + clean + weekend + logdist +
           has.child, data = sat.std)
summary(m5)

# Eliminate insignificant variable weekend, check wait:has.child
## use ":" if u use varaibles also solo, or use "*" if you dont include them
m7 <- lm(overall ~ rides + games + wait + clean + logdist + has.child +
           wait:has.child, data = sat.std)
summary(m7)

# Plotting
library(coefplot)
coefplot(m7, intercept = FALSE, outerCI = 1.96, lwdOuter = 1.5,
         ylab = "Rating of Feature", xlab = "Association with Overall Sat.")

# Relative importances of predictors
install.packages("relaimpo")
library(relaimpo)
calc.relimp(m7, type = c("lmg"), rela = TRUE)
