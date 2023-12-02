library(XML)
library("rvest")
library(tidyr)
library(lubridate)
library(dplyr)
library(httr)
library(parsedate)
library(data.table)

# Get link of the match
URL = "https://el.soccerway.com/matches/2023/02/19/argentina/primera-division/newell-s-old-boys/ca-banfield/3982591/"
#"https://el.soccerway.com/matches/2023/02/19/argentina/primera-division/newell-s-old-boys/ca-banfield/3982591/"
#https://fbref.com/en/matches/8dbd30f4/San-Lorenzo-Independiente-June-4-2022-Primera-Division?utm_source=direct&utm_medium=Share&utm_campaign=ShareTool
# https://fbref.com/en/matches/b71024e7/Caracas-Atletico-Paranaense-April-5-2022-Copa-Libertadores
# https://el.soccerway.com/matches/2023/01/25/south-america/sudamericano-u20/argentina-youth/peru-under-20/4007293/
# https://int.soccerway.com/matches/2022/10/01/argentina/primera-division/club-atletico-san-lorenzo-de-almagro/club-atletico-huracan/3791029/

getGameFrameSW <- function(URL, competition_name)
{
## Select country
#competition_name = "International"

# Read the page. Use GET from httr package
page <- GET(URL, add_headers('user-agent' = 'Web scrap for personal project ([[krukovskiy.ignat@gmail.com]])'))

#while(http_status(page)$category != "Success"){
  #Sys.sleep(300)
#}

if (http_status(page)$category == "Success")
{

# Parse html page
# Find home and away teams
teams = page %>% read_html() %>% html_nodes(".team-title")  %>% html_text2()
team = teams[1]
rival = teams[2]

#skip if ther is no players

# Find home players
rank_home = page %>% read_html() %>% html_nodes(".left .large-link") 
if (length(rank_home) != 0 )
{
t_home = data.frame(link = rank_home %>% html_node("a") %>% html_attr("href"),
               name = rank_home %>% html_text2(),
               min = NA
  
)

# Find away players
rank_away = page %>% read_html() %>% html_nodes(".right .large-link") 
t_away = data.frame(link = rank_away %>% html_node("a") %>% html_attr("href"),
                    name = rank_away %>% html_text2(),
                    min = NA)

# Find player which were substituted
# Get index of subs players
sub_index = which(!is.na(rank_home %>% html_node(".substitute-out") %>% html_node("a") %>% html_attr("href")))

# Create list of subs
sub_players =  data.frame(link_out = rank_home %>% html_node(".substitute-out") %>% html_node("a") %>% html_attr("href"),
                          name_out = rank_home %>% html_node(".substitute-out") %>% html_node("a") %>% html_text2(),
                          min_out = str_extract((rank_home %>% html_node(".substitute-out") %>% html_text()), "([0-9])+", group = NULL),
                          link_in = rank_home %>% html_node(".substitute a") %>% html_attr("href"),
                          name_in = rank_home %>% html_node(".substitute a")  %>% html_text2(),
                          min_in = 90 - as.numeric(str_extract((rank_home %>% html_node(".substitute-out") %>% html_text()), "([0-9])+", group = NULL))
                          )
                          
# Delete NA
sub_players = na.omit(sub_players)

# Add and clean minuts of subs
t_home[1:11,3] = 90
t_home = left_join(t_home, sub_players, by = c("name"="name_out"))
tosub_index = which(!is.na(t_home$min_out[1:11]))
t_home$min[tosub_index] = t_home$min_out[tosub_index]
t_home[sub_index,] = data.frame(sub_players$link_in, sub_players$name_in, sub_players$min_in)
t_home = t_home[,1:3]
# Fill other guys with 0 mins
t_home[is.na(t_home)] = 0
#Count red card time of the exit from the pitch
t_home$rc=0
event <- page %>%
  read_html() %>%
  html_nodes(".left .bookings img[src*='Y2C.png'], .left .bookings img[src*='RC.png']") 
name = html_nodes(event, xpath = "../../preceding-sibling::td[1]/a | ../../preceding-sibling::td[1]/p[1]/a") %>% html_text2()
e = event %>% html_attr("src")
min = as.numeric(str_extract(html_nodes(event, xpath = "..") %>% html_text2(), "([0-9])+", group = NULL))
hrm = data.frame(name, e, min)
merged_df <- merge(t_home, hrm, by.x = "name", by.y = "name", all.x = TRUE)
merged_df$min.x[!is.na(merged_df$min.y)] <- merged_df$min.y[!is.na(merged_df$min.y)]
merged_df$rc <- ifelse(is.na(merged_df$e), 0, 1)
# Make final home frame
t_home = merged_df[,1:4]
colnames(t_home)[3] <- "min"

## Make rival!
# Find player which were substituted
# Get index of subs players
sub_index_aw = which(!is.na(rank_away %>% html_node(".substitute-out") %>% html_node("a") %>% html_attr("href")))

# Create list of subs
sub_players_aw =  data.frame(link_out = rank_away %>% html_node(".substitute-out") %>% html_node("a") %>% html_attr("href"),
                          name_out = rank_away %>% html_node(".substitute-out") %>% html_node("a") %>% html_text2(),
                          min_out = str_extract((rank_away %>% html_node(".substitute-out") %>% html_text()), "[0-9]+", group = NULL),
                          link_in = rank_away %>% html_node(".substitute a") %>% html_attr("href"),
                          name_in = rank_away %>% html_node(".substitute a")  %>% html_text2(),
                          min_in = 90 - as.numeric(str_extract((rank_away %>% html_node(".substitute-out") %>% html_text()), "[0-9]+", group = NULL))
)

# Delete NA
sub_players_aw = na.omit(sub_players_aw)

# Add and clean minuts of subs
t_away[1:11,3] = 90
t_away = left_join(t_away, sub_players_aw, by = c("name"="name_out"))
tosub_aw_index = which(!is.na(t_away$min_out[1:11]))
t_away$min[tosub_aw_index] = t_away$min_out[tosub_aw_index]
t_away[sub_index_aw,] = data.frame(sub_players_aw$link_in, sub_players_aw$name_in, sub_players_aw$min_in)
t_away = t_away[,1:3]
# Fill other guys with 0 mins
t_away[is.na(t_away)] = 0
# Count time whe red card
t_away$rc=0
aevent <- page %>%
  read_html() %>%
  html_nodes(".right .bookings img[src*='Y2C.png'], .right .bookings img[src*='RC.png']") 
aname = html_nodes(aevent, xpath = "../../preceding-sibling::td[1]/a | ../../preceding-sibling::td[1]/p[1]/a") %>% html_text2()
ae = aevent %>% html_attr("src")
amin = as.numeric(str_extract(html_nodes(aevent, xpath = "..") %>% html_text2(), "([0-9])+", group = NULL))
ahrm = data.frame(aname, ae, amin)
amerged_df <- merge(t_away, ahrm, by.x = "name", by.y = "aname", all.x = TRUE)
amerged_df$min[!is.na(amerged_df$amin)] <- amerged_df$amin[!is.na(amerged_df$amin)]
amerged_df$rc <- ifelse(is.na(amerged_df$ae), 0, 1)
# Final away frame
t_away = amerged_df[,1:4]
colnames(t_away)[3] <- "min"

## 2. Scrap date, result
## Find date from "page"
game_date_raw = page %>% read_html %>% html_nodes(".details") %>% html_text2()
## Find competition name
competition_name = page %>% read_html %>% html_nodes(".divider+ a") %>% html_text2() 
## Set english locale to parse month by its name
Sys.setlocale("LC_TIME", "English")
game_date = game_date = strptime(game_date_raw, "%d/%m/%Y")
#game_date = as_date(as_datetime(game_date))
## Scrap result
game_result_raw = page %>%  read_html() %>% html_nodes(".bidi") %>% html_text()
game_result_raw = as.numeric(str_split(game_result_raw, " - ", simplify = TRUE))


# Determine the winner (or loser)
result = ifelse(game_result_raw[1] > game_result_raw[2], "w",
                ifelse(game_result_raw[1] == game_result_raw[2], "d", "l")
                )
## !!!!!!!!!!!!!!!!! DONT FORGET !!!!!!!!!!!!!
result_a = ifelse(game_result_raw[2] > game_result_raw[1], "w",
                  ifelse(game_result_raw[2] == game_result_raw[1], "d", "l"))

## Work with cups (penalties)
pen_win = page %>%  read_html() %>% html_nodes(".scoretime") %>% html_children() %>% html_attr("class")
pen_win_index = str_detect(pen_win, "addition-visible")
### result home

result = case_when(
  pen_win_index[2] == TRUE & pen_win_index[3] == FALSE ~ "p1",
  pen_win_index[2] == FALSE & pen_win_index[3] == TRUE ~ "p0",
  game_result_raw[1] > game_result_raw[2] ~ "w",
  game_result_raw[2] == game_result_raw[1] ~ "d", 
  game_result_raw[1] < game_result_raw[2] ~ "l"
  
)

### result away
result_a = case_when(
  pen_win_index[2] == FALSE & pen_win_index[3] == TRUE ~ "p1",
  pen_win_index[2] == TRUE & pen_win_index[3] == FALSE ~ "p0",
  game_result_raw[2] > game_result_raw[1] ~ "w",
  game_result_raw[2] == game_result_raw[1] ~ "d", 
  game_result_raw[2] < game_result_raw[1] ~ "l"
  
)              

## Aggregate data to one data frame
game_frame = data.frame(game_date,
                   team, 
                   player = t_home$name, 
                   link = t_home$link, 
                   min = t_home$min, 
                   rc = t_home$rc,
                   rival = rival, 
                   score_h = game_result_raw[1] ,
                   score_a = game_result_raw[2], 
                   result,
                   is_home = 1) # is_home - if the team plays at home


#### Now we make the same with the second team, but RIVAL will be change
game_frame2 = data.frame(game_date,
                        team = rival, 
                        player = t_away$name, 
                        link = t_away$link, 
                        min = t_away$min, 
                        rc = t_away$rc,
                        rival = team, 
                        score_h = game_result_raw[1] ,
                        score_a = game_result_raw[2], 
                        result = result_a,
                        is_home = 0)

### AND NOW (!!!) aggregate all data to one dataframe
game_frame = rbind(game_frame, game_frame2)

# Add country name
game_frame = cbind(game_frame, competition = competition_name)

game_frame
}
}
}