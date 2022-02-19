library(tidyverse)
library(broom)

diamonds %>% glimpse()

# Warning...this will take a couple minutes to run:
# Plot each selected variable against each other in a single plot
pairs(diamonds %>% select(price, carat, color, depth))


model_fit <- lm(price ~ carat, data=diamonds)

tidy(model_fit)
glance(model_fit)
model_actuals <- augment(model_fit)

glimpse(model_actuals)

ggplot(model_actuals) +
  geom_point(aes(x=carat, y=price)) +
  geom_line(aes(x=carat, y=.fitted), col="red")

ggplot(model_actuals) +
  geom_point(aes(x=carat, y=price)) +
  geom_smooth(aes(x=carat, y=price), method = 'lm', se = F)

# Another way to specify this:
ggplot(model_actuals, aes(x=carat)) +
  geom_point(aes(y=price)) +
  geom_line(aes(y=.fitted), col="red")


# Squared variable
model_fit2 <- lm(price ~ carat + carat**2, data=diamonds)

tidy(model_fit2)
glance(model_fit2)
model_actuals2 <- augment(model_fit2)

ggplot(model_actuals2, aes(x=carat**2)) +
  geom_point(aes(y=price)) +
  geom_line(aes(y=.fitted), col="red")

# Two continuous variables
model_fit3 <- lm(price ~ carat + depth, data=diamonds)

tidy(model_fit3)
glance(model_fit3)
model_actuals3 <- augment(model_fit3)

# Use ggnewscale library for two color schemes
ggplot(model_actuals3, aes(x=carat)) +
  geom_point(aes(y=price, col=depth)) +
  scale_color_gradient(low = "blue", high = "green", na.value = NA) +
  ggnewscale::new_scale_color() +
  geom_line(aes(y=.fitted, col=depth), size=2) +
  scale_color_gradient(low = "yellow", high = "red", na.value = NA)



