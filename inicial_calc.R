library(tidyverse)
library(httr)
library("rvest")

## Calculate iniciales

# Load base
rating_data = readRDS("data/calc_data12112023.RData")
all_last_p_rates = readRDS("data/lastSW18082023.RData")

## Date
game_date = "2023-10-21"

# the function
getTeamRates <- function(URL){
Sys.setlocale("LC_TIME", "English")
## Create empty frame
inicial_rates = rating_data[0,]
rival_rates = rating_data[0,]

# Read the page. Use GET from httr package
page <- GET(URL3, add_headers('user-agent' = 'Web scrap for personal project ([[krukovskiy.ignat@gmail.com]])'))

teams = page %>% read_html() %>% html_nodes(".team-title")  %>% html_text2()
team = teams[1]
rival = teams[2]

# Find home players
rank_home = page %>% read_html() %>% html_nodes(".left:first-child .lineups .large-link") 
if (length(rank_home) != 0 )
{
  t_home = data.frame(link = rank_home %>% html_node("a") %>% html_attr("href"),
                      name = rank_home %>% html_text2(),
                      min = NA
                      
  )
  
  # Find away players
  rank_away = page %>% read_html() %>% html_nodes(".right .table[class='playerstats lineups table'] .large-link") 
  t_away = data.frame(link = rank_away %>% html_node("a") %>% html_attr("href"),
                      name = rank_away %>% html_text2(),
                      min = NA)
}

## Calculate player rate before
for(i in 1:nrow(t_home)) {       # for-loop over columns
  
  ## Add row for next player
  inicial_rates[i,] <- NA
  inicial_rates$game_date[i] = game_date
  inicial_rates$team[i] = team
  inicial_rates$rival[i] = rival
  inicial_rates$link[i] = t_home$link[i]
  inicial_rates$player[i] = t_home$name[i]
 
  # Find before occurrences
  rate_before = all_last_p_rates %>% filter(link == t_home$link[i]) %>% arrange(desc(game_date))
  
  # If there is now rating then starting rating is 1500
  if (nrow(rate_before) == 0)
  {
    inicial_rates$PlayerRateBefore[i] = 1500
  } else {
    inicial_rates$PlayerRateBefore[i] = rate_before$PlayerRateAfter[1]
  }
} # end of the FOR loop

## This formula is ok just for starting lineups!                    11 is because 11 players
inicial_rates$TeamRateBefore = sum(inicial_rates$PlayerRateBefore) / 11



######## RIVAL FRAME !!! ######

## Calculate player rate before
for(i in 1:nrow(t_away)) {       # for-loop over columns
  
  ## Add row for next player
  rival_rates[i,] <- NA
  rival_rates$game_date[i] = game_date
  rival_rates$team[i] = rival
  rival_rates$rival[i] = team
  rival_rates$link[i] = t_away$link[i]
  rival_rates$player[i] = t_away$name[i]
  
  # Find before occurrences
  rate_before = all_last_p_rates %>% filter(link == t_away$link[i]) %>% arrange(desc(game_date))
  
  # If there is now rating then starting rating is 1500
  if (nrow(rate_before) == 0)
  {
    rival_rates$PlayerRateBefore[i] = 1500
  } else {
    rival_rates$PlayerRateBefore[i] = rate_before$PlayerRateAfter[1]
  }
} # end of the FOR loop

## This formula is ok just for starting lineups!                    11 is because 11 players
rival_rates$TeamRateBefore = sum(rival_rates$PlayerRateBefore) / 11

## Add rival to dataframe!
inicial_rates = rbind(inicial_rates, rival_rates)

# final team rates
inicial_team_rates = unique(inicial_rates[c("team", "TeamRateBefore")])
return(list(inicial_rates, inicial_team_rates))
}

URL3 = "https://el.soccerway.com/matches/2023/11/23/mexico/liga-de-ascenso/club-atlante/cd-tapatio/4265287/"
qq = getTeamRates(URL3)
qq2 = qq[[1]]
qq[[2]]

