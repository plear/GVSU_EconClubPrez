library(tidyverse)

# Additional libraries that we'll use but not load into global namespace:
# ggthemes
# scales


# URL location of data
url_counties <- 
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
url_college <- 
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/colleges/colleges.csv"

covid_data <- read_csv(url_counties)  
# This is over 2 million records so it may take a while to download
covid_dataCollege <- read_csv(url_college)

glimpse(covid_data)

covid_dataCollege %>% glimpse()

mi_covid_college <- covid_dataCollege %>% filter(state=="Michigan")

ggplot(data=mi_covid_college) +
  geom_col(aes(x=fct_reorder(college, cases), y=cases)) + 
  coord_flip() +
  ggtitle("Total Covid Cases - Michigan Colleges") +
  xlab("")+
  ylab("Cases") +
  labs(caption = "Last Update: 2021-05-26\nhttps://raw.githubusercontent.com/nytimes/covid-19-data/master/colleges/colleges.csv") +
  theme_minimal()


glimpse(covid_data)

ggplot(covid_data %>% filter(state=="Michigan")) +
  geom_line(aes(x=date, y=cases, col=county)) +
  theme(legend.position = "none")

mi_data_10 <- covid_data %>%
  filter(state=="Michigan") %>% 
  filter(date == max((covid_data %>%
                        filter(state=="Michigan"))$date)) %>% 
  slice_max(order_by = cases, n = 10)

mi_data_10_ts <- covid_data %>% 
  filter(state=="Michigan") %>% 
  filter(county %in% mi_data_10$county)

ggplot(mi_data_10_ts) +
  geom_col(aes(x=date, y=cases, fill=county)) +
  ggtitle("Total Covid Cases by Country Top 10 Michigan Counties") +
  labs(x ="Date", y = "Cases", fill = "County") +
  labs(caption = 
         "Source: https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  scale_fill_brewer(palette="Paired") +
  ggthemes::theme_economist() +
  theme(axis.text.y = element_text(size=10),
        plot.title=element_text(size=15))


mi_data_10_ts_new_cases <- mi_data_10_ts %>% 
  group_by(state, fips, county) %>% 
  arrange(date) %>% 
  mutate(new_cases = cases - lag(cases, default =0),
         new_deaths = deaths - lag(deaths, default =0)) 

mi_data_10_ts_new_cases <- mi_data_10_ts_new_cases %>% 
# https://tidyr.tidyverse.org/
  pivot_longer(cols = c("cases", "deaths", "new_cases","new_deaths"),
               names_to = "metric",
               values_to = "count") 


ggplot(data=mi_data_10_ts_new_cases %>% filter(county == "Kent", 
                                               metric %in% c("new_cases","new_deaths"))) +
  geom_line(aes(x=date, y=count, color=metric))


ggplot(data=mi_data_10_ts_new_cases %>% 
         filter(county == "Kent", metric %in% c("new_cases","new_deaths")) %>% 
         filter(date > max(mi_data_10_ts_new_cases$date) - 13*7)) +
  geom_line(aes(x=date, y=count, color=metric))

ggplot(data=mi_data_10_ts_new_cases %>% 
         filter(county == "Kent", metric %in% c("new_cases","new_deaths")) %>% 
         filter(date < min(mi_data_10_ts_new_cases$date) + 13*7)) +
  geom_line(aes(x=date, y=count, color=metric))


ggplot(data=mi_data_10_ts_new_cases %>% filter(metric == "new_cases")) +
  geom_line(aes(x=date, y=count, color=county)) +
  facet_grid(county~.) +
  ggthemes::theme_stata() +
  ggtitle("Total Covid Cases by Country\nTop 10 Michigan Counties") +
  labs(x="date", y="Cases")+
  labs(caption = 
         "Source: https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") +
  scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 1e-3, accuracy = 1)) +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(size = 5), 
  strip.text.y = element_text(size = 8),
  strip.text.x = element_text(size = 8),
  plot.title = element_text(size=15))



  



  




