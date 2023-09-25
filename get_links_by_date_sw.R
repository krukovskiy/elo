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

date = "2023/08/18"
getLinksByDate <- function(date)
{
date2 = gsub("/", "-", date)
#date = paste0(date, "/")

#Need only for 1 pager
URL = paste0("https://el.soccerway.com/matches/",date)

# get page with all links
page <- GET(URL, add_headers('user-agent' = 'Web scrap for personal project ([[krukovskiy.ignat@gmail.com]])'))

while(http_status(page)$category != "Success"){
  Sys.sleep(300)
}

### Expand blocks
# inicialize links
links_expanded = c()
# Detect blocks
blocks = page %>% read_html() %>% html_nodes("div") %>% html_attr("data-comp")
blocks = na.omit(blocks)

# Make a loop      length(blocks)
for (i in 1:length(blocks))
{
# Make links to expand
base_url = "http://el.soccerway.com/a/block_livescores?"

# Make params
params = paste0("{\"d\":\"", date2, "\",\"c\":\"", blocks[i], "\"}")

# Get data camps numbers
#links_to_expand <- GET(base_url, query = list("callback_params" = "{}", "block_id" = "comp", "action" = "loadcomp", "params" = "{\"d\":\"2023-08-18\",\"c\":\"1255\"}"), add_headers('user-agent' = 'Web scrap for personal project ([[krukovskiy.ignat@gmail.com]])'))
links_to_expand <- GET(base_url, query = list("callback_params" = "{}", "block_id" = "comp", "action" = "loadcomp", "params" = params), add_headers('user-agent' = 'Web scrap for personal project ([[krukovskiy.ignat@gmail.com]])'))
links_to_expand = links_to_expand %>% read_html(options = "HUGE") %>% html_nodes("div > a ") %>% html_attr("href")

# clear backslashes and quotes
links_to_expand = gsub("([\\])","", links_to_expand)
links_to_expand = gsub('"', "", links_to_expand)
# only unique links
links_to_expand = unique(links_to_expand)

# add to a vector of links
links_expanded =  c(links_expanded, links_to_expand)

# Be polite
Sys.sleep(1)
}
#get full link
full_link = paste0("https://el.soccerway.com", links_expanded)
full_link
}

## END OF THE FUNCTION



# SAVE
#links = page %>% read_html() %>% html_nodes(".scores a") %>% html_attr("href")



# save to csv
filename = paste0("data/links", gsub("/", "", date), ".csv")
write.csv2(full_link, filename)
