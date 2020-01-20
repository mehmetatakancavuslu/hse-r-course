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
