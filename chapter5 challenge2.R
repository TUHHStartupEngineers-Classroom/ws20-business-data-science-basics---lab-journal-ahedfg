library(tidyverse)
library(dplyr)
library(scales)
library(maps)
library(ggplot2)

#wrangling data
world <- map_data("world")%>%
  rename(countriesAndTerritories = region)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

covid_data_tbl <- covid_data_tbl %>%
  select(dateRep, year, popData2019, deaths, countriesAndTerritories)%>%
  filter(year==2020)%>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  ))%>%
 
  
  group_by(countriesAndTerritories,popData2019)%>%
  
  mutate(total_deaths = sum(deaths))%>%
  
  slice(n())%>%
  mutate(mortality_rate_percent = 100*round((total_deaths/popData2019),digits=4))%>%
  ungroup()%>%
  
  select(mortality_rate_percent, countriesAndTerritories)

mortal_map <- left_join(covid_data_tbl, world, by = "countriesAndTerritories")
# write.csv(covid_data_tbl, "covid3.csv")
#visulization
library(RColorBrewer)

ggplot(mortal_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = mortality_rate_percent ), color = "white")+
  expand_limits(x=world$long, y = world$lat)+
  
  scale_fill_gradient(low = "#E31A1C", high = "#000000")
# display.brewer.all()
# brewer.pal.info
# brewer.pal(n = 9, name = "Greys")
