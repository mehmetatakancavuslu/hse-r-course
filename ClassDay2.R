## Class Day 2
# Comparing Groups / Tests
seg.df <- read.csv("http://goo.gl/qw303p")
summary(seg.df)

# Descriptives: Selecting by group
mean(seg.df$income[seg.df$Segment == "Moving up"])
mean(seg.df$income[seg.df$Segment == "Moving up" & seg.df$subscribe == "subNo"])
# This is tedious, use a formula: "aggregate"
# Breakout income by segment, computing the mean
aggregate(income ~ Segment, data = seg.df, mean)
# Extend to multiple dimensions
aggregate(income ~ Segment + ownHome, data = seg.df, mean)
# Aggregate returns a data frame
agg.data <- aggregate(income ~ Segment + ownHome, data = seg.df, mean)
agg.data[2,]

# Tables - Counts occurances of a single value
table(seg.df$Segment, seg.df$ownHome)
# Another way to write it with()
with(seg.df, table(Segment, ownHome))
# Prop.table to get the proportions
with(seg.df, prop.table(table(Segment, ownHome)))
with(seg.df, prop.table(table(Segment, ownHome), margin = 1))
# Doing math in a table, adding up the total number of kids in each segment
aggregate(kids ~ Segment, data = seg.df, sum)

# Histograms - Example of lattice package
library(lattice)
histogram(~subscribe | Segment, data = seg.df)
histogram(~subscribe | Segment, data = seg.df, type = "count", layout = c(4,1),
          col = c("burlywood", "darkolivegreen"))
# Histograms by two factors
histogram(~subscribe | Segment + ownHome, data = seg.df)

# Aggregate continuous data
seg.mean <- aggregate(income ~ Segment, data = seg.df, mean)
barchart(income ~ Segment, data = seg.mean, col = "grey")
# Continuous data by two factors
seg.agg <- aggregate(income ~ Segment + ownHome, data = seg.df, mean)
barchart(income ~ Segment, data = seg.agg, groups = ownHome, auto.key = TRUE,
         par.settings = simpleTheme(col = c("gray95", "gray50")))

# Boxplots - with lattice
bwplot(Segment ~ income, data = seg.df, horizontal = TRUE, xlab = "Income")
# Boxplots with two grouping
bwplot(Segment ~ income | ownHome, data = seg.df,
       horizontal = TRUE, xlab = "Income")

# Statistical Tests
#----------------------------------------------------------------------

# Chi-Square Test - Tests equality of marginal counts in groups
table(seg.df$subscribe, seg.df$ownHome)
# Do they have the same rate of subscription by ownership of home
chisq.test(table(seg.df$subscribe, seg.df$ownHome))

# Proportions: binomial test - whether the count matches expected proportion
binom.test(12, 20, p = 0.5)

# T-tests: Compares the means of two groups
# Null hypothesis: Two groups are different
bwplot(income ~ ownHome, data = seg.df)
t.test(income ~ ownHome, data = seg.df)
# T-test for a subset of data
t.test(income ~ ownHome, data = subset(seg.df, Segment == "Travelers"))
# For travelers, p value > 0.05 so we fail to reject null hypothesis

# ANOVA Basics: to compare more than 2 groups
# For two groups it is effectively the same
aov.inc.seg <- aov(income ~ Segment, data = seg.df)
anova(aov.inc.seg)

# Means plot
install.packages("multcomp")
library(multcomp)
aov.inc.seg <- aov(income ~ -1 + Segment, data = seg.df) # model w/o intercept
by.seg <- glht(aov.inc.seg) # means and CIs
par(mar = c(5, 10, 5, 5)) # margins, bot, left, top, right
plot(by.seg, xlab = "Income", main = "Mean income by segment")

# Compare medians - If distribution is far from being normal, median may be
# more meaningful than mean
install.packages("RVAideMemoire")
library(RVAideMemoire)
mood.medtest(income ~ Segment, data = seg.df)
mood.medtest(income ~ ownHome, data = seg.df)
