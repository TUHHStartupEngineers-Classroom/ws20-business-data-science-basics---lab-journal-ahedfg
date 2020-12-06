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
  file       = "patent.tsv",
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

patent_tbl <- patent_tbl %>%
  select(id)%>%
  rename(patent_id = id)
  



col_types2 <-list(
  patent_id = col_character(),
  assignee_id = col_character()
)

patent_assignee_tbl <-vroom(
  file = "patent_assignee.tsv",
  delim = "\t",
  col_types = col_types2,
  na = c("","NA","NULL")
)
setDT(patent_tbl)
setDT(patent_assignee_tbl)
combined_data <- merge(x = patent_tbl, y = patent_assignee_tbl, 
                       by    = "patent_id", 
                       all.x = TRUE, 
                       all.y = FALSE)


setDT(combined_data)
setkey(combined_data, "patent_id")


setorderv(combined_data, c("patent_id", "assignee_id"))

keep_cols <- c("patent_id","assignee_id")
combined_data <-combined_data[, ..keep_cols]

combined_data%>%
  group_by(assignee_id)%>%
  summarise(number_patents = n())%>%
  ungroup()%>%
  arrange(desc(number_patents))
# setDT(combined_data)
# 
# col_types3 <- list(
#   id = col_character(),
#   type = col_double(),
#   organization = col_character()
# 
# )
# 
# assignee_tbl <- vroom(
#   file       = "assignee.tsv",
#   delim      = "\t",
#   col_types  = col_types3,
#   na         = c("", "NA", "NULL")
# )
# assignee_tbl <-assignee_tbl%>%
#   select(id) %>%
#   rename(assignee_id = id)
# 
# setDT(assignee_tbl)
# combined_data_final<-merge(x=combined_data, y = assignee_tbl,
#                            by = "assignee_id",
#                            all.x = TRUE,
#                            all.y = FALSE)
# setDT(combined_data_final)
# setkey(combined_data_final, "assignee_id")



