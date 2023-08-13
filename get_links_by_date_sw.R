library(httr)
library(jsonlite)
library(dplyr)
library(XML)
library("rvest")
library(tidyr)

library(dplyr)
library(httr)
library(parsedate)
library(data.table)

date = "2023/08/12/"

#Need only for 1 pager
URL = paste0("https://el.soccerway.com/matches/",date)

# get page with all links
page <- GET(URL, add_headers('user-agent' = 'Web scrap for personal project ([[krukovskiy.ignat@gmail.com]])'))

# get links
links = page %>% read_html() %>% html_nodes(".scores a") %>% html_attr("href")

#get full link
full_link = paste0("https://el.soccerway.com", links)

# save to csv
filename = paste0("data/links", gsub("/", "", date), ".csv")
write.csv2(full_link, filename)
