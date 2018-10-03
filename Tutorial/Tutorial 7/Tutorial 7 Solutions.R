options(scipen=999)
library(tidyverse)    #Essential Functions
library(rvest)        #Read Tables From Webpages

#1.1
URL.VIOLENT="https://en.wikipedia.org/wiki/List_of_United_States_cities_by_crime_rate#Crime_rates_per_100.2C000_people_.282012.29"
VIOLENT = URL.VIOLENT %>%
  read_html() %>%
  html_table(fill=T) %>%
  .[[2]]
head(VIOLENT)

#1.2
VIOLENT2=VIOLENT[-1,1:8]
colnames(VIOLENT2)=c("State","City","Population","Total","Murder","Rape","Robbery","Assault")
head(VIOLENT2)