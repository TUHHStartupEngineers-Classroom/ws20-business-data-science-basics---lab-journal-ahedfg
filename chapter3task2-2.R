library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)
url_home          <- "https://www.rosebikes.de/fahrr%C3%A4der"
xopen(url_home) # Open links directly from RStudio to inspect them

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
## "#js-navigationList-ROAD, #js-navigationList-MOUNTAIN, #js-navigationList-EBIKES, #js-navigationList-HYBRID-CITY, #js-navigationList-YOUNGHEROES"





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

bike_category_tbl



# # 2.0 COLLECT BIKE DATA ----
# 
# # 2.1 Get URL for each bike of the Product categories
# 
# # select first bike category url
# bike_category_url <- bike_category_tbl$url[1]
# 
# # Alternatives for selecting values
# # bike_category_url <- bike_category_tbl %$% url %>% .[1]
# # bike_category_url <- bike_category_tbl %>% pull(url) %>% .[1]
# # bike_category_url <- deframe(bike_category_tbl[1,])
# # bike_category_url <- bike_category_tbl %>% first %>% first
# 
# xopen(bike_category_url)
# 
# html_bike_category  <- read_html(bike_category_url)
# bike_url_tbl        <- html_bike_category %>%
#   
#   # Get the 'a' nodes, which are hierarchally underneath 
#   # the class productTile__contentWrapper
#   html_nodes(css = ".productTile__contentWrapper > a") %>%
#   html_attr("href") %>%
#   
#   # Remove the query parameters of the URL (everything after the '?')
#   str_remove(pattern = "\\?.*") %>%
#   
#   # Convert vector to tibble
#   enframe(name = "position", value = "url")
# 
# # 2.1.2 Extract the descriptions (since we have retrieved the data already)
# bike_desc_tbl <- html_bike_category %>%
#   
#   # Get the nodes in the meta tag where the attribute itemprop equals description
#   html_nodes('.productTile__productSummaryLeft > meta[itemprop="description"]') %>%
#   
#   # Extract the content of the attribute content
#   html_attr("content") %>%
#   
#   # Convert vector to tibble
#   enframe(name = "position", value = "description")
# 
# # 2.1.3 Get even more data from JSON files
# bike_json_tbl  <- html_bike_category %>%
#   
#   html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
#   html_attr("data-gtm-impression") %>%
#   
#   # Convert the JSON format to dataframe
#   # map runs that function on each element of the list
#   map(fromJSON) %>% # need JSON ### need lists
#   
#   # Extract relevant information of the nested list
#   map(purrr::pluck, 2, "impressions") %>% # Need purrr and expl above
#   
#   # Set "not defined" and emtpy fields to NA (will be easier to work with)
#   map(na_if, "not defined") %>%
#   map(na_if, "") %>%
#   
#   # The class of dimension56 and price varies between numeric and char.
#   # This converts this column in each list to numeric
#   # across allows to perform the same operation on multiple columns
#   map(~mutate(., across(c("dimension56","price"), as.numeric))) %>%
#   
#   # Stack all lists together
#   bind_rows() %>%
#   # Convert to tibble so that we have the same data format
#   as_tibble() %>%
#   
#   # Add consecutive numbers so that we can bind all data together
#   # You could have also just use bind_cols()
#   rowid_to_column(var='position') %>%
#   left_join(bike_desc_tbl) %>%
#   left_join(bike_url_tbl)







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
    enframe(name = "position", value = "name")
    

  # Get the prices
  bike_price_tbl <- html_bike_category %>%
    html_nodes(css = ".catalog-category-bikes__price-title") %>%
    html_text() %>%
    enframe(name = "position", value = "price") %>%
    left_join(bike_name_tbl) %>%
    left_join(bike_url_tbl)
}

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
  
}
bike_data_tbl
saveRDS(bike_data_tbl, "rosebikes_data_tbl.rds")


