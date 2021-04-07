library(ggplot2)
library(tidyverse)
library(gapminder)

# Start with data object
ggplot(data=gapminder)

# Map axis to plot
ggplot(data=gapminder, 
       aes(x=gdpPercap, y=lifeExp))

# Add points layer
ggplot(data=gapminder, 
       aes(x=gdpPercap, y=lifeExp)) +
  geom_point() 

# Add rug plot layer with points plot layer
ggplot(data=gapminder, 
       aes(x=gdpPercap, y=lifeExp)) +
  geom_point() +
  geom_rug() 

# Map color to points
ggplot(data=gapminder, 
       aes(x=gdpPercap, y=lifeExp,
           color = continent)) +
  geom_point() 


# Switch to density plot for same x variable
ggplot(data=gapminder, 
       aes(x=gdpPercap)) +
         geom_density() 

# Seperate groups by color in density plot
ggplot(data=gapminder, 
       aes(x=gdpPercap, color=continent)) +
         geom_density() 

# Using "fill" instead of "color"
ggplot(data=gapminder, 
       aes(x=gdpPercap, fill=continent)) +
         geom_density() 

# Change x axis to log scale
ggplot(data=gapminder, aes(x=gdpPercap, y=lifeExp, color = continent)) +
  geom_point() +
  geom_rug() +
  scale_x_log10()

# Filter to one year
ggplot(data=filter(gapminder, year==1952), aes(x=gdpPercap, y=lifeExp, color = continent)) +
  geom_point() +
  geom_rug() +
  scale_x_log10()

# Filter to one different year
ggplot(data=filter(gapminder,year==2007), aes(x=gdpPercap, y=lifeExp, color = continent)) +
  geom_point() +
  geom_rug() +
  scale_x_log10()

# Add modeled line (Generalized additive model for n > 1000, loess for n < 1000)
ggplot(data=gapminder,aes(x=year, y=lifeExp, color = continent)) +
  geom_point() +
  geom_smooth()

# Add linear modeled line
ggplot(data=gapminder, aes(x=year, y=lifeExp, color = continent)) +
  geom_point() +
  geom_smooth(method="lm")

# Add jitter point plot
ggplot(data=gapminder, aes(x=year, y=lifeExp, color = continent)) +
  geom_jitter() +
  geom_smooth(method="lm")

# Increase transparency  of points
ggplot(data=gapminder,aes(x=year, y=lifeExp, color = continent)) +
  geom_jitter(alpha=0.25) +
  geom_smooth(method="lm")


# Filter to set of years
ggplot(data=filter(gapminder, year %in% c(1952, 1962, 1972, 1992, 2002)), 
       aes(x=gdpPercap, y=lifeExp, color = continent)) +
  geom_point() +
  geom_rug() +
  scale_x_log10()

# Add facet wrap with one dimension
ggplot(data= filter(gapminder, year %in% c(1952, 1962, 1972, 1992, 2002)), 
       aes(x=gdpPercap, y=lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(.~year) 

# Add facet wrap with two dimensions
ggplot(data=filter(gapminder, year %in% c(1952, 1962, 1972, 1992, 2002)), 
       aes(x=gdpPercap, y=lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(continent~year) 

# Add facet grid
ggplot(data=filter(gapminder, year %in% 
                  c(1952, 1962, 1972, 1992, 2002)), 
       aes(x=gdpPercap, y=lifeExp,color = continent)) +
  geom_point() +
  scale_x_log10() +
  facet_grid(~year)

# Move legend to the bottom
ggplot(data= filter(gapminder, year %in% c(1952, 1962, 1972, 1992, 2002)), 
       aes(x=gdpPercap, y=lifeExp,color = continent)) +
  geom_point() +
  scale_x_log10() +
  facet_grid(~year) +
  theme(legend.position = "bottom")

# Change angle of x axis labels
ggplot(data=filter(gapminder, year %in% 
                  c(1952, 1962, 1972, 1992, 2002)), 
       aes(x=gdpPercap, y=lifeExp,
           color = continent)) +
  geom_point() +
  scale_x_log10() +
  facet_grid(~year) +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=90))

# Format labels for x axis
ggplot(data=
         filter(gapminder, year %in% 
                  c(1952, 1962, 1972, 1992, 2002)), 
       aes(x=gdpPercap, y=lifeExp,
           color = continent)) +
  geom_point() +
  scale_x_log10(labels = 	scales::trans_format("log10", 	scales::math_format(10^.x))) +
  facet_grid(~year) +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=90))

# Add titles to chart
ggplot(data=
         filter(gapminder, year %in% 
                  c(1952, 1962, 1972, 1992, 2002)), 
       aes(x=gdpPercap, y=lifeExp,
           color = continent)) +
  geom_point() +
  scale_x_log10(labels = 	scales::trans_format("log10", 	scales::math_format(10^.x))) +
  facet_grid(~year) +
  theme(legend.position = "bottom",
        axis.text.x=element_text(angle=90))+
  labs(title = "Life Expectency vs GDP per Capita Over Time", 
       x = "GDP per Capita (log)", 
       y = "Average Life Expectancy")
















       
       
