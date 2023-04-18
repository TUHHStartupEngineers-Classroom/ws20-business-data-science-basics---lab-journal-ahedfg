library(tidyverse)
library(dplyr)
library(scales)
#wrangling data
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

covid_data_tbl <- covid_data_tbl %>%
  select(dateRep, day, month, year, cases, countriesAndTerritories)%>%
  filter(year==2020)%>%
  filter(countriesAndTerritories %in% c("Germany","Spain","France","United_States_of_America","United_Kingdom"))%>%
  mutate(date=dmy(dateRep))%>%
  
  group_by(countriesAndTerritories)%>%
  arrange(date)%>%
  mutate(Cumulative_cases = cumsum(cases))%>%
  ungroup()%>%
  mutate(countriesAndTerritories = fct_reorder2(countriesAndTerritories,date,Cumulative_cases))
  

# write.csv(covid_data_tbl, "covid3.csv")
#visulization
covid_data_tbl%>%
  ggplot(aes(date, Cumulative_cases, color = countriesAndTerritories)) +
  geom_line(size=1, linetype = 1) +
  xlab("Year 2020")+
  scale_x_date(date_labels = "%b")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))
