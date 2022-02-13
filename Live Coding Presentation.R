library(tidyverse)

# Additional libraries that we'll use but not load into global namespace:
# ggthemes
# scales


# URL location of data
url_counties <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
url_college <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/colleges/colleges.csv"

covid_data <- read_csv(url_counties)  # This is over 2 million records so it may take a while to download
covid_dataCollege <- read_csv(url_college)