library(XML)
library("rvest")
library(tidyr)
library(lubridate)
library(dplyr)
library(httr)
library(parsedate)

# Get link of the match
URL = "https://int.soccerway.com/matches/2022/10/01/argentina/primera-division/club-atletico-san-lorenzo-de-almagro/club-atletico-huracan/3791029/"
#https://fbref.com/en/matches/43e41f41/Boca-Juniors-Arsenal-June-5-2022-Primera-Division
#https://fbref.com/en/matches/fa531c8b/Barracas-Central-CC-Cordoba-June-3-2022-Primera-Division?utm_source=direct&utm_medium=Share&utm_campaign=ShareTool
#https://fbref.com/en/matches/8a5c877e/Tucuman-Colon-June-4-2022-Primera-Division?utm_source=direct&utm_medium=Share&utm_campaign=ShareTool
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

# Parse html page
# Find home and away teams
teams = page %>% read_html() %>% html_nodes(".team-title")  %>% html_text2()
team = teams[1]
rival = teams[2]


# Find home players
rank_home = page %>% read_html() %>% html_nodes(".left .large-link") 
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
                          min_out = str_extract((rank_home %>% html_node(".substitute-out") %>% html_text()), "([0-9]).", group = NULL),
                          link_in = rank_home %>% html_node(".substitute a") %>% html_attr("href"),
                          name_in = rank_home %>% html_node(".substitute a")  %>% html_text2(),
                          min_in = 90 - as.numeric(str_extract((rank_home %>% html_node(".substitute-out") %>% html_text()), "([0-9]).", group = NULL))
                          )
                          
# Delete NA
sub_players = na.omit(sub_players)

# Add and clean minuts of subs
t_home = left_join(t_home, sub_players, by = c("name"="name_out"))
t_home$min = t_home$min_out
t_home[sub_index,] = data.frame(sub_players$link_in, sub_players$name_in, sub_players$min_in)
t_home = t_home[,1:3]
# Fill other guys with 90 mins
t_home[is.na(t_home)] = 90


## Make rival!
# Find player which were substituted
# Get index of subs players
sub_index_aw = which(!is.na(rank_away %>% html_node(".substitute-out") %>% html_node("a") %>% html_attr("href")))

# Create list of subs
sub_players_aw =  data.frame(link_out = rank_away %>% html_node(".substitute-out") %>% html_node("a") %>% html_attr("href"),
                          name_out = rank_away %>% html_node(".substitute-out") %>% html_node("a") %>% html_text2(),
                          min_out = str_extract((rank_away %>% html_node(".substitute-out") %>% html_text()), "([0-9]).", group = NULL),
                          link_in = rank_away %>% html_node(".substitute a") %>% html_attr("href"),
                          name_in = rank_away %>% html_node(".substitute a")  %>% html_text2(),
                          min_in = 90 - as.numeric(str_extract((rank_away %>% html_node(".substitute-out") %>% html_text()), "([0-9]).", group = NULL))
)

# Delete NA
sub_players_aw = na.omit(sub_players_aw)

# Add and clean minuts of subs
t_away = left_join(t_away, sub_players_aw, by = c("name"="name_out"))
t_away$min = t_away$min_out
t_away[sub_index_aw,] = data.frame(sub_players_aw$link_in, sub_players_aw$name_in, sub_players_aw$min_in)
t_away = t_away[,1:3]
# Fill other guys with 90 mins
t_away[is.na(t_away)] = 90


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

## Aggregate data to one data frame
game_frame = data.frame(game_date,
                   team, 
                   player = t_home$name, 
                   link = t_home$link, 
                   min = t_home$min, 
                   rival = rival, 
                   score_h = game_result_raw[1] ,
                   score_a = game_result_raw[2], 
                   result)

## A little weird way to rename columns


## !!!!!!!!!!!!!!!!! DONT FORGET !!!!!!!!!!!!!
result_a = ifelse(game_result_raw[2] > game_result_raw[1], "w",
                  ifelse(game_result_raw[2] == game_result_raw[1], "d", "l")
)
#### Now we make the same with the second team, but rival will be change
game_frame2 = data.frame(game_date,
                        team = rival, 
                        player = t_away$name, 
                        link = t_away$link, 
                        min = t_away$min, 
                        rival = team, 
                        score_h = game_result_raw[1] ,
                        score_a = game_result_raw[2], 
                        result = result_a)

### AND NOW (!!!) aggregate all data to one dataframe
game_frame = rbind(game_frame, game_frame2)

# Add country name
game_frame = cbind(game_frame, competition = competition_name)

game_frame
}