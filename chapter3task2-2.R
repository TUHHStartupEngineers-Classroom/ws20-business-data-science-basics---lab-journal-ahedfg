library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)
url_home          <- "https://www.rosebikes.de/fahrr%C3%A4der"

# Read in the HTML for the entire webpage
html_home         <- read_html(url_home)

# Web scrape the ids for the families
bike_family_tbl <- html_home %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".catalog-navigation__list-item > a") %>%
  # ...and extract the information of the id attribute
  html_attr('title') %>%
  
  # Remove the product families Gear and Outlet and Woman 
  # (because the female bikes are also listed with the others)
  discard(.p = ~stringr::str_detect(.x,"Kinder|Sale|Bike-Finder")) %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "family_class") %>%
  
  # Add a hashtag so we can get nodes of the categories by id (#)
  mutate(
    family_id = str_glue("#{family_class}")
  )

family_id_css <- bike_family_tbl %>%
  pull(family_id) %>%
  stringr::str_c(collapse = ", ")

bike_family_tbl

?as.numeric
?gsub


# Extract the urls from the href attribute
bike_category_tbl <- html_home %>%
  
  # Selecting two classes makes it specific enough
  html_nodes(css = ".catalog-navigation__list-item > a") %>%
  html_attr('href') %>%
  discard(.p = ~stringr::str_detect(.x,"kinder|sale|zoovu")) %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "subdirectory") %>%
  
  # Add the domain, because we will get only the subdirectories
  mutate(
    url = glue("https://www.rosebikes.de{subdirectory}")
  ) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)


get_bike_data <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".catalog-category-bikes__list-item > a") %>%
    html_attr("href") %>%
    enframe(name = "position", value = "url")
  
  # Get the names
  bike_name_tbl <- html_bike_category %>%
    html_nodes(css = ".catalog-category-bikes__title-text") %>%
    html_text() %>%
    str_remove_all(pattern = "\n") %>%
    
    enframe(name = "position", value = "name")
    

  # Get the prices
  bike_price_tbl <- html_bike_category %>%
    html_nodes(css = ".catalog-category-bikes__price-title") %>%
    html_text() %>%
    sub("." , ",")%>%
    enframe(name = "position", value = "price") %>%
    left_join(bike_name_tbl) %>%
    left_join(bike_url_tbl)
}
?sub
?format
?nchar
?str_remove_all
?readr::parse_number()
?trunc
help("::")
?str_remove
??multiply
# 2.3.1b Alternative with a for loop

# Create an empty tibble, that we can populate
bike_data_tbl <- tibble()

# Loop through all urls
for (i in seq_along(bike_category_tbl$url)) {
  
  bike_category_url <- bike_category_tbl$url[i]
  bike_data_tbl     <- bind_rows(bike_data_tbl, get_bike_data(bike_category_url))
  
  # Wait between each request to reduce the load on the server 
  # Otherwise we could get blocked
  Sys.sleep(3)
  
  # print the progress
  print(i)
?format
  ?nchar
}
bike_data_tbl <- bike_data_tbl %>%
  rename("model" = "name")
bike_data_tbl
saveRDS(bike_data_tbl, "rosebikes_data_tbl.rds")

?replace
?formatC
?extract_numeric
