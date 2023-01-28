library(httr)
library(jsonlite)

#Need only for 1 pager
URL = "https://int.soccerway.com/national/argentina/primera-division/2022/s21099/final-stages/"
# Initialize null vector
url_2_save = NULL  
# page number
p = 1
# GET page with links
page <- GET(URL, add_headers('user-agent' = 'Web scrap for personal project ([[krukovskiy.ignat@gmail.com]])'))

for (p in 1:14)
{

# prepare params to get different pages
r = "https://int.soccerway.com"

# This link you have to find with inspector becaus of weird pagination on javascript
path = "/a/block_competition_matches_summary"
# 67189 for 1 stage
cbp2 = as.character(htmltools::urlEncodePath(paste0('{\"page\":\"',p,'\",\"block_service_id\":\"competition_summary_block_competitionmatchessummary\",\"round_id\":\"67189\",\"outgroup\":\"\",\"view\":\"1\",\"competition_id\":\"87\"}')))
query = paste0("block_id=",
               "page_competition_1_block_competition_matches_summary_9",
               "&callback_params=",
               cbp2,
               "&action=",
               "changePage",
               "&params=",
               htmltools::urlEncodePath(paste0('{"page":',p-1,'}')))

g = GET(r,path = path, query = query, add_headers('user-agent' = 'Web scrap for personal project ([[krukovskiy.ignat@gmail.com]])'))
# for one-pager
# g = GET(URL, add_headers('user-agent' = 'Web scrap for personal project ([[krukovskiy.ignat@gmail.com]])'))
rl = g %>% read_html() %>% html_nodes("a") %>% html_attr("href")
rl2 = gsub("\\\\","", rl)
rl3 = gsub("^\"","", rl2)
rl4 = gsub("\"$","", rl3)
rl5 = subset(rl4, grepl("matches", rl4)) 
rl6 = unique(gsub("#events$","", rl5))

cur_whole_link = paste0(r,rl6)
url_2_save = append(url_2_save, cur_whole_link)
}

saveRDS(url_2_save, "data/urls_f_22_sw.RData")

