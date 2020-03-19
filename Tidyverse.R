### Tidyverse Datacamp Course Notes
### Manipulating and preprocessing the data
install.packages("gapminder")
library(gapminder)
library(dplyr)
library(ggplot2)

# --------------------------------------------------------------------------- #

### CHAPTER 1: DPLYR

# Filter function: To filter out a subset
gapminder %>% filter(year == 2007)
gapminder %>% filter(country == "United States")
gapminder %>% filter(year == 2007, country == "United States")

# Arrange function: To sort based on a variable
# Note that by default in sorts in ascending order, use "desc" for descending
gapminder %>% arrange(gdpPercap)
gapminder %>% arrange(desc(gdpPercap))
# Can combine filter and arrange
gapminder %>% filter(year == 2007) %>% arrange(desc(gdpPercap))

# Mutate function: To change or add new variables
gapminder %>% mutate(pop = pop/1000000)
gapminder %>% mutate(gdp = gdpPercap * pop)
# All can be combined
gapminder %>% mutate(gdp = gdpPercap * pop) %>% filter(year == 2007) %>%
  arrange(desc(gdp))

# --------------------------------------------------------------------------- #

## CHAPTER 2: VISUALIZING WITH GGPLOT2

# Variable assignment
gapminder_2007 <- gapminder %>% filter(year == 2007)

# Scatterplot with ggplot2
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) + geom_point()
# To add logscale in one axis, add the following
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) + geom_point() +
  scale_x_log10()
# Additional aesthetics: Color
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() + scale_x_log10()
# Additional aesthetics: Size
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp,
                           color = continent, size = pop)) +
  geom_point() + scale_x_log10()

# Faceting: Multiple graphs for variables
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) + geom_point() +
  scale_x_log10() + facet_wrap(~ continent)

# --------------------------------------------------------------------------- #

### CHAPTER 3: SUMMARIZE FUNCTION

# Summarize function: turns many rows into one
gapminder %>% summarize(meanLifeExp = mean(lifeExp))
# Can be combined with other functions
gapminder %>% filter(year == 2007) %>% summarize(meanLifeExp = mean(lifeExp))
# Can be summarized into multiple columns
gapminder %>% filter(year == 2007) %>% summarize(meanLifeExp = mean(lifeExp),
                                                 totalPop = sum(pop))
# Following functions can be used in "summarize" function:
# sum median min max

# Group by: before summarize to group the data
gapminder %>% group_by(year) %>% summarize(meanLifeExp = mean(lifeExp),
                                           totalPop = sum(pop))
# Summarizing by continent, in filtered year
gapminder %>% filter(year == 2007) %>% group_by(continent) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(pop))
# Summarize by both year and continent
gapminder %>% group_by(year, continent) %>%
  summarize(totalPop = sum(pop), meanLifeExp = mean(lifeExp))

# Visualizing summarized data with ggplot2
# Create new dataframe, summarized by years
by_year <- gapminder %>% group_by(year) %>%
  summarize(totalPop = sum(pop), meanLifeExp = mean(lifeExp))
# Plot with ggplot
ggplot(by_year, aes(x = year, y = totalPop)) + geom_point()
# To start y-axis at zero, use "expand_limits" function
ggplot(by_year, aes(x = year, y = totalPop)) + geom_point() +
  expand_limits(y = 0)

# Summarizing with two variables: year and continent
by_year_continent <- gapminder %>% group_by(year, continent) %>%
  summarize(totalPop = sum(pop), meanLifeExp = mean(lifeExp))
# Visualize by year and continent with "color"
ggplot(by_year_continent, aes(x = year, y = totalPop, color = continent)) +
  geom_point() + expand_limits(y = 0)

# --------------------------------------------------------------------------- #

## CHAPTER 4: DIFFERENT TYPES OF PLOTS

# Line plots: with "geom_line"
# Used in mainly timeseries datas (years vs something)
ggplot(year_continent, aes(x = year, y = meanLifeExp, color = continent)) +
  geom_line() + expand_limits(y = 0)

# Bar plots: with "geom_col"
# Used in mainly grouped data by categorical variable
ggplot(by_continent, aes(x = continent, y = meanLifeExp)) + geom_col()

# Histograms: with "geom_histogram"
# Used mainly to show frequency and distribution of one variable
ggplot(gapminder_2007, aes(x = lifeExp)) + geom_histogram()
# Bins can be adjusted
ggplot(gapminder_2007, aes(x = lifeExp)) + geom_histogram(binwidth = 5)

# Boxplots: with "geom_boxplot"
ggplot(gapminder_2007, aes(x = continent, y = lifeExp)) + geom_boxplot()

