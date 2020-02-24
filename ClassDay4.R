## Class Day 4
sat.df <- read.csv("http://goo.gl/HKnl74")

# Standardizing many variables at once except distance
# Remove distance variable
sat.std <- sat.df[, -3]
sat.std[, 3:7] <- scale(sat.std[, 3:7])
sat.std$logdist <- log(sat.df$distance)
summary(sat.std)

# What is odd about this model?
m3 <- lm(overall ~ rides + games + wait + clean + weekend + logdist + num.child,
         data = sat.std)
summary(m3)
# Number of children is not a truly numeric predictor, all coefficiants same
sat.std$num.child.factor <- factor(sat.std$num.child)
m4 <- lm(overall ~ rides + games + wait + clean + weekend + logdist +
            num.child.factor, data = sat.std)
summary(m4)
# Use has.child variable instead
sat.std$has.child <- factor(sat.std$num.child > 0)
m5 <- lm(overall ~ rides + games + wait + clean + weekend + logdist + has.child,
         data = sat.std)
summary(m5)

# Interactions between predictors


