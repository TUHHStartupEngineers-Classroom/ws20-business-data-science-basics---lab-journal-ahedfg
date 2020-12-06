library(tidyverse)
library(vroom)
library(tibble)
# Data Table
library(data.table)
library(dplyr)

col_types <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file       = "Patent_data_reduced/patent.tsv",
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

patent_tbl <- patent_tbl %>%
  
  rename(patent_id = id)%>%
  separate(col = date,
           into = c("year", "month", "date"),
           sep = "-")



col_types2 <-list(
  patent_id = col_character(),
  assignee_id = col_character()
)

patent_assignee_tbl <-vroom(
  file = "Patent_data_reduced/patent_assignee.tsv",
  delim = "\t",
  col_types = col_types2,
  na = c("","NA","NULL")
)


col_types3 <- list(
  patent_id = col_character(),
  mainclass_id = col_double()
  
)
uspc_tbl <- vroom(
  file       = "Patent_data_reduced/uspc.tsv",
  delim      = "\t",
  col_types  = col_types3,
  na         = c("", "NA", "NULL")
)


setDT(uspc_tbl)
setDT(patent_tbl)
setDT(patent_assignee_tbl)
combined_data <- merge(x = patent_tbl, y = patent_assignee_tbl, 
                       by    = "patent_id", 
                       all.x = TRUE, 
                       all.y = FALSE)


setDT(combined_data)

combined_data_2 <- merge(x =combined_data, y = uspc_tbl,
                         by = "patent_id",
                         all.x = TRUE,
                         all.y = FALSE)

setDT(combined_data_2)
setkey(combined_data_2, "patent_id")


combined_data_2 %>%
  group_by(mainclass_id)%>%
  summarise(number_patents = n())%>%
  ungroup()%>%
  arrange(desc(number_patents))
