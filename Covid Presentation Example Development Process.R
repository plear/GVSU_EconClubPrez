library(tidyverse)

# URL location of data
url_counties <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
url_college <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/colleges/colleges.csv"

covid_data <- read_csv(url_counties)  # This is over 2 million records so it may take a while to download
covid_dataCollege <- read_csv(url_college)

glimpse(covid_dataCollege)

mi_covid_college <- covid_dataCollege %>% filter(state == "Michigan")

# Static comparisons
ggplot(data=mi_covid_college) +
  geom_col(aes(x=college, y=cases))

ggplot(data=mi_covid_college) +
  geom_col(aes(x=college, y=cases)) + 
  coord_flip()

ggplot(data=mi_covid_college) +
  geom_col(aes(x=fct_reorder(college, desc(college)), y=cases)) + 
  coord_flip()

ggplot(data=mi_covid_college) +
  geom_col(aes(x=fct_reorder(college, cases), y=cases)) + 
  coord_flip()

ggplot(data=mi_covid_college) +
  geom_col(aes(x=fct_reorder(college, cases), y=cases)) + 
  coord_flip() +
  ggtitle("Total Covid Cases - Michigan Colleges") +
  xlab("") + 
  ylab("Cases") +
  labs(caption = "Last Update: 2021-05-26") +
  theme_minimal()


# Time series data
glimpse(covid_data)

mi_data <- covid_data %>%
  filter(state=="Michigan") %>% 
  group_by(county) %>% 
  summarise(cases=sum(cases), deaths=sum(deaths))

ggplot(mi_data) +
  geom_col(aes(x=fct_reorder(county, cases), y=cases)) + 
  coord_flip()

ggplot(covid_data %>% filter(state=="Michigan")) +
  geom_col(aes(x=date, y=cases)) 

ggplot(covid_data %>% filter(state=="Michigan")) +
  geom_line(aes(x=date, y=cases, col=county)) +
  theme(legend.position = "none")

mi_data_10 <- covid_data %>%
  filter(state=="Michigan") %>% 
  group_by(county) %>% 
  summarise(cases=sum(cases), deaths=sum(deaths)) %>% 
  slice_max(order_by = cases, n = 10) %>% 
  ungroup()

mi_data_10_ts <- covid_data %>% 
  filter(state=="Michigan") %>% 
  filter(county %in% mi_data_10$county)

ggplot(mi_data_10_ts) +
  geom_line(aes(x=date, y=cases, col=county))

ggplot(mi_data_10_ts) +
  geom_col(aes(x=date, y=cases, fill=county))


ggplot(mi_data_10_ts) +
  geom_area(aes(x=date, y=cases, fill=county)) +
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  scale_fill_brewer(palette="Paired") +
  ggthemes::theme_economist() +
  ggtitle("Total Covid Cases by Country - Top 10 Michigan Counties") +
  labs(x ="Date", y = "Cases", fill = "County") +
  labs(caption = "Source: https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))






