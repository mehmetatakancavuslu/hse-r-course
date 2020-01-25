## Seminar Day 3 - Plots & Tests
seg.df <- read.csv("http://goo.gl/qw303p")

# Two way tables
table(seg.df$Segment, seg.df$ownHome)
with(seg.df, table(Segment, ownHome))
xtabs(~seg.df$Segment + seg.df$ownHome)

# Chi-square test
chisq.test(with(seg.df, table(Segment, ownHome)))

## Lattice package for fancy graphs and looks
library(lattice)
# Histogram
histogram(~subscribe | Segment, data = seg.df)
histogram(~subscribe | Segment, data = seg.df, type = "count", layout = c(4,1),
          col = c("burlywood", "darkolivegreen"))
histogram(~subscribe | Segment + ownHome, data = seg.df)
# Bar chart
seg.mean <- aggregate(income ~ Segment, data = seg.df, mean)
barchart(income ~ Segment, data = seg.mean, col = "grey")
# Bar chart by two factors
seg.mean2 <- aggregate(income ~ Segment + ownHome, data = seg.df, mean)
barchart(income ~ Segment, data = seg.mean2, groups = ownHome, auto.key = TRUE,
         par.settings = simpleTheme(col = c("grey95", "grey50")))
# Boxplot
bwplot(Segment ~ income, data = seg.df, horizontal = TRUE, xlab = "Income")
bwplot(Segment ~ income | ownHome, data = seg.df, horizontal = TRUE,
       xlab = "Income", layout = c(1,2))
bwplot(income ~ ownHome, data = seg.df, xlab = "Home Ownership")

# T-test
t.test(income ~ ownHome, data = seg.df)
# ANOVA Test
aov.inc.seg <- aov(income ~ Segment, data = seg.df)
anova(aov.inc.seg)
library(multcomp)
aov.inc.seg <- aov(income ~ - 1 + Segment, data = seg.df)
by.seg <- glht(aov.inc.seg)
par(mar = c(5, 10, 5, 5))
plot(by.seg, xlab = "Income", main = "Mean Income by Segments (95% CI)")

# If distribution is not normal, use median test instead of mean
library(RVAideMemoire)
mood.medtest(income ~ Segment, data = seg.df)
