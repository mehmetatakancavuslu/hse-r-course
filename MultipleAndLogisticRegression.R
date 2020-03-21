### Multiple and Logistic Regressiong
### Datacamp Course Notes
library(dplyr)
library(ggplot2)
install.packages("broom")
library(broom)

# --------------------------------------------------------------------------- #

### CHAPTER 1: PARALLEL SLOPES MODEL

# General formula as follows
# hwy= β0 + β1*displ + β2*year
# If its already categorical (binary) variable, do not use "factor" function
lm(hwy ~ displ + factor(year), data = mpg)

# Visualizing parallel slopes
mod <- lm(hwy ~ displ + factor(year), data = mpg)
# Getting the fitted values with "augment" function from broom package
augment(mod)
# Adding the parallel slopes to dataspace we created before (not shown here)
data_space +
  geom_line(data = augment(mod), aes(y = .fitted, color = factor.year))

# --------------------------------------------------------------------------- #

### CHAPTER 2: PREDICTIONS, MODEL FITS

# Fitting the model to the given data
# returns a vector
predict(mod)
# returns a data.frame
augment(mod)

# Predicting new observation
new_obs <- data.frame(displ = 1.8, year = 2008)
predict(mod, newdata = new_obs)
augment(mod, newdata = new_obs)

# Interactions
ggplot(data = mpg, aes(x = displ, y = hwy, color = factor(year))) +
  geom_point() + geom_smooth(method = "lm", se = 0)
# add interaction term manually with ":"
lm(hwy ~ displ + factor(year) + displ:factor(year), data = mpg)

# --------------------------------------------------------------------------- #

### CHAPTER 3: ADDING NUMERICAL EXPLANATORY VARIABLE

# No longer 2d problem, ggplot by default does not work
# We need to use "tiling" method to represent it in 2D plot
data_space <- ggplot(babies, aes(x = gestation, y = age)) +
  geom_point(aes(color = bwt))
# Creating grids from data
grid <- babies %>% data_grid(gestation = seq_range(gestation, by = 1),
                             age = seq_range(age, by = 1) )
mod <- lm(bwt ~ gestation + age, data = babies)
# Predicting the grids
bwt_hats <- augment(mod, newdata = grid)
# Scale fill the grid
data_space +
  geom_tile(data = bwt_hats, aes(fill = .fitted, alpha = 0.5)) +
  scale_fill_continuous("bwt", limits = range(babies$bwt))

# Another method to use plot.ly to draw 3D
plot_ly(data = babies, z = ~bwt, x = ~gestation, y = ~age, opacity = 0.6) %>%
  add_markers(text = ~case, marker = list(size = 2)) %>%
  add_surface(x = ~x, y = ~y, z = ~plane, showscale = FALSE,
              cmax = 1, surfacecolor = color1, colorscale = col1)

# Adding a third, categorical varaible
lm(bwt ~ gestation + age + smoke, data = babies)
# Drawing parallel planes in 3D
plot_ly(data = babies, z = ~bwt, x = ~gestation, y = ~age, opacity = 0.6) %>%
  add_markers(color = ~factor(smoke), text = ~case, marker = list(size = 2)) %>%
  add_surface(x = ~x, y = ~y, z = ~plane0, showscale = FALSE,
              cmin = 0, cmax = 1, surfacecolor = color1, colorscale = col1) %>%
  add_surface(x = ~x, y = ~y, z = ~plane1, showscale = FALSE,
              cmin = 0, cmax = 1, surfacecolor = color2, colorscale = col1)

# Higher dimensions
lm(bwt ~ gestation + age + smoke + height + weight + parity, data = babies)
# . means all of the variables, here "minus case"
lm(bwt ~ . - case, data = babies)

# --------------------------------------------------------------------------- #

### CHAPTER 4: LOGISTIC REGRESSION

# A categorical response variable, adding jitter for better visual
ggplot(data = heartTr, aes(x = age, y = survived)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.5)

# Making categorical varaible a binary one
heartTr <- heartTr %>%
  mutate(is_alive = ifelse(survived == "alive", 1, 0))
data_space <- ggplot(data = heartTr, aes(x = age, y = is_alive)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.5)
# Regression to this binarial response
data_space + geom_smooth(method = "lm", se = FALSE)

# Instead of linear regression, use logistic, with glm
glm(is_alive ~ age, data = heartTr, family = binomial)
# Visualizing both lm and glm
data_space +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "glm", se = FALSE, color = "red",
              method.args = list(family = "binomial"))
# Additionally we can use binned data
# Create binned data with geom_point() and geom_line()
data_binned_space + geom_line(data = augment(mod, type.predict = "response"),
                              aes(y = .fitted), color = "blue")

## 3 SCALES to INTERPRETATION

# Probability Scale - y_hat is the probability
heartTr_plus <- mod %>% augment(type.predict = "response") %>%
  mutate(y_hat = .fitted)
# Probability visualization
ggplot(heartTr_plus, aes(x = age, y = y_hat)) + geom_point() + geom_line() +
  scale_y_continuous("Probability of being alive", limits = c(0, 1))

# Odds Scale
heartTr_plus <- heartTr_plus %>% mutate(odds_hat = y_hat / (1 - y_hat))
# Visualization
ggplot(heartTr_plus, aes(x = age, y = odds_hat)) + geom_point() + geom_line() +
  scale_y_continuous("Odds of being alive")

# Log-Odds Scale
heartTr_plus <- heartTr_plus %>% mutate(log_odds_hat = log(odds_hat))
# Visualization
ggplot(heartTr_plus, aes(x = age, y = log_odds_hat)) + geom_point() +
  geom_line() +
  scale_y_continuous("Log(odds) of being alive")

# Using a logistic model
mod <- glm(is_alive ~ age + transplant, data = heartTr, family = binomial)
# Using augment to fit:
# log-odds scale
augment(mod)
# probability scale
augment(mod, type.predict = "response")

# Out of sample predictions
cheney <- data.frame(age = 71, transplant = "treatment")
augment(mod, newdata = cheney, type.predict = "response")

# Making binary predictions
mod_plus <- augment(mod, type.predict = "response") %>%
  mutate(alive_hat = round(.fitted))
mod_plus %>%
  select(is_alive, age, transplant, .fitted, alive_hat)

# Confusion Matrix
mod_plus %>% select(is_alive, alive_hat) %>% table()
