library(XML)
library("rvest")
library(tidyr)
library(lubridate)
library(dplyr)


### tidy links (football)

raw_data = read_html("footballlinksraw2.html")
raw_links = raw_data %>%  html_nodes("a") %>% html_attr("href")
saveRDS(raw_links, "football_links2.rds")
write.csv(raw_links, "football_links2.csv")
