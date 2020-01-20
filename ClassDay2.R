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
