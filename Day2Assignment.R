# Assignment for day2 seminar
store.df <- read.csv("http://goo.gl/QPDdMl")

# Task 1
aggregate(p1sales ~ p1price + p1prom, data = store.df, mean)

# Task 2
prop.table(xtabs(formula = ~ store.df$country + store.df$p1prom),
           margin = 1) * 100

# Task 3
store.df$p1p2sales <- store.df$p1sales + store.df$p2sales

# Task 4
aggregate(p1p2sales ~ country, data = store.df, sum)

# Task 5
aggregate(p1p2sales ~ storeNum, data = store.df, sum)
max(aggregate(p1p2sales ~ storeNum, data = store.df, sum))
XYZ <- 113 # Store with the highest sales number

# Task 6
library(dplyr)
store.XYZ.df <- store.df %>% filter(storeNum == XYZ)

# Task 7
store.XYZ.df$p1p2prom <- ifelse((store.XYZ.df$p1prom == 1 & store.XYZ.df$p2prom == 1),
                                 1, 0)
store.XYZ.df$p1p2prom <- factor(store.XYZ.df$p1p2prom)
summary(store.XYZ.df$p1p2prom)
(prop.table(summary(store.XYZ.df$p1p2prom)) * 100)[2]

# Task 8
cor(store.XYZ.df[, c("p1sales", "p2sales", "p1price", "p2price")],
    use = "complete.obs")
cor(store.XYZ.df[, c("p1sales", "p2sales", "p1price", "p2price")],
    use = "complete.obs", method = "kendall")
cor(store.XYZ.df[, c("p1sales", "p2sales", "p1price", "p2price")],
    use = "complete.obs", method = "spearman")
library(corrplot)
corrplot(corr = cor(store.XYZ.df[, c("p1sales", "p2sales", "p1price", "p2price")],
                    use = "complete.obs"), method = "ellipse")
## prices are not correlated, but sales are strongly correlated with r = -0.55

# Task 9
scatterplotMatrix(formula = ~ p1price + p2price + p1sales + p2sales,
                  data = store.XYZ.df, diagonal = "histogram")

# Task 10
library(corrplot)
corrplot(corr = cor(store.XYZ.df[, c("p1sales", "p2sales", "p1price", "p2price")],
                    use = "complete.obs"), method = "number")
