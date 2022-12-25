library(XML)
library("rvest")
library(tidyr)
library(lubridate)
library(dplyr)

# Get link of the match
URL = "https://fbref.com/en/matches/b71024e7/Caracas-Atletico-Paranaense-April-5-2022-Copa-Libertadores"
#https://fbref.com/en/matches/43e41f41/Boca-Juniors-Arsenal-June-5-2022-Primera-Division
#https://fbref.com/en/matches/fa531c8b/Barracas-Central-CC-Cordoba-June-3-2022-Primera-Division?utm_source=direct&utm_medium=Share&utm_campaign=ShareTool
#https://fbref.com/en/matches/8a5c877e/Tucuman-Colon-June-4-2022-Primera-Division?utm_source=direct&utm_medium=Share&utm_campaign=ShareTool
#https://fbref.com/en/matches/8dbd30f4/San-Lorenzo-Independiente-June-4-2022-Primera-Division?utm_source=direct&utm_medium=Share&utm_campaign=ShareTool
# https://fbref.com/en/matches/b71024e7/Caracas-Atletico-Paranaense-April-5-2022-Copa-Libertadores




getGameFrame <- function(URL)
{
## Select country
country_name = "Argentina"

# Read the page
page = read_html(URL)

# Parse html page

## 1. Scrap player list
## Find a table
rank_data_html = page %>%  html_nodes("table") 

## Looking for a table with the squad lineup. Number 4 is found manually
t = html_nodes(rank_data_html[[4]], "tbody") 
t2 = html_table(t)[[1]]

## Take player names and mins
## select 1 (player) and 6 (min) column
players = t2[,c(1,6)]


## 2. Scrap date, team, rival, result
## Find date from "page"
game_date_raw = page %>%  html_nodes(".scorebox_meta strong a") %>% html_text()

## Set english locale to parse month by its name
Sys.setlocale("LC_TIME", "English")
game_date = strptime(game_date_raw, "%A %B %d, %Y")

## Scrap team name
team_raw = page %>%  html_nodes("div:nth-child(1) div strong a") %>% html_text()
team = team_raw[1]
rival = team_raw[2]


## Scrap result
game_result_raw = page %>%  html_nodes(".score") %>% html_text()

# Determine the winner (or loser)
result = ifelse(game_result_raw[1] > game_result_raw[2], "w",
                ifelse(game_result_raw[1] == game_result_raw[2], "d", "l")
                )

## Aggregate data to one data frame
game_frame = cbind(game_date, team, players, rival = rival, result)

## A little weird way to rename columns
colnames(game_frame)[c(3,4)] = c("player", "min")


## !!!!!!!!!!!!!!!!! DONT FORGET !!!!!!!!!!!!!

#### Now we make the same with the second team, but rival will be change
## Scrap player list. To scrap it we still use "rank_data_html", but with other index
## Looking for a table with the squad lineup. Number 6 is found manually
t = html_nodes(rank_data_html[[6]], "tbody") 
t2 = html_table(t)[[1]]

## Take player names and mins
## Null players
players = NULL
## select 1 (player) and 6 (min) column
players = t2[,c(1,6)]

# Determine the winner (or loser) (FOR OTHER TEAM WE CHANGE INDEXES)
result = ifelse(game_result_raw[2] > game_result_raw[1], "w",
                ifelse(game_result_raw[2] == game_result_raw[1], "d", "l")
)

## Now aggregate data for the rival. Attention with team and rival variables (they should be changed)
game_frame2 = cbind(game_date, team = rival, players, rival = team, result)

## A little weird way to rename columns
colnames(game_frame2)[c(3,4)] = c("player", "min")

### AND NOW (!!!) aggregate all data to one dataframe
game_frame = rbind(game_frame, game_frame2)

# Add country name
game_frame = cbind(game_frame, country = country_name)

game_frame
}