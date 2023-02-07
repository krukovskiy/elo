library(dplyr)
library(tibble)
library(PlayerRatings)
library(tidyr)
library(reactable)
library(htmltools)
library(stringr)

########### HERE START AGG WITH PREPARED DATA FRAME!!!!!!!! ##################


###########                               ####################
## Create empty dataframe with same columns to make furthermore calculated table
## Attention!!!!!
games = readRDS("data/LPFA22.RDS")
calc_data <- games[0,]

## Define all game pairs (team * date)
game_pairs = unique(games[c("game_date", "team")])

for (k in 1:nrow(game_pairs))
{
  ## Select current game frame                           [k,1]
  current_game = games %>% filter(game_date == game_pairs[k,1] & team == game_pairs[k,2])
  current_game$team = as.character(current_game$team)
  game_min = as.numeric(max(current_game$min))
  ## Select game date
  current_date = current_game$game_date[1]
  
  ## Select team
  current_team = as.character(current_game$team[1])
  
  ## Calculate player rate before
  for(i in 1:nrow(current_game)) {       # for-loop over columns
    
    ## Select player
    current_player = current_game$player[i]
    current_link = current_game$link[i]
    # Find before occurrences
    rate_before = calc_data %>% filter(game_date < current_date & link == current_link) %>% arrange(desc(game_date))
    
    # If there is now rating then starting rating is 1500
    if (nrow(rate_before) == 0)
    {
      current_game$PlayerRateBefore[i] = 1500
    } else {
      current_game$PlayerRateBefore[i] = rate_before$PlayerRateAfter[1]
    }
  } # end of the FOR loop
  ## Calculate team rate before. DONT FORGET ABOUT GAME_MIN
  current_game$min = as.numeric(current_game$min)
  current_game$TeamRateBefore = sum(current_game$PlayerRateBefore * (current_game$min/game_min))/ sum(current_game$min/game_min)
  
  ## !!!!!! RIVAL FRAME !!!!!!  Calculate rival rate before
  ## Select RIVAL
  current_rival = as.character(current_game$rival[1])
  
  ## Change current game to the RIVAL
  current_rival_game = games %>% filter(game_date == current_date & team == current_rival)
  
  ## Calculate player rate before
  for(i in 1:nrow(current_rival_game)) {       # for-loop over columns
    
    ## Select player
    current_player = current_rival_game$player[i]
    current_link = current_rival_game$link[i]
    
    # Find before occurrences
    rate_before = calc_data %>% filter(game_date < current_date & link == current_link) %>% arrange(desc(game_date))
    
    # If there is now rating then starting rating is 1500
    if (nrow(rate_before) == 0)
    {
      current_rival_game$PlayerRateBefore[i] = 1500
    }  else {
      current_rival_game$PlayerRateBefore[i] = rate_before$PlayerRateAfter[1]
    }
  }
  ## Calculate rival rate before. This formula could be changed. Not sure for 100%
  current_rival_game$min = as.numeric(current_rival_game$min)
  current_game$RivalRateBefore = sum(current_rival_game$PlayerRateBefore * (current_rival_game$min/game_min))/ sum(current_rival_game$min/game_min)
  
  
  ## !!!! Calculate TEAM  elo
  ## Create a dataframe with team results history
  
  # team_history =  games %>% filter(game_date < current_date & team == current_team) %>% arrange(game_date) # no history
  th_short = select(current_game, game_date, team, rival, result)
  
  ## Convert letters to points
  th_short$score <- gsub('w', 1,
                         gsub('d', .5,
                              gsub('l', 0, th_short$result)))
  th_short$score = as.numeric(th_short$score)
  ## Add game number
  th_short$ID <- seq.int(nrow(th_short))
  th_short2 = select(th_short[1,], ID, team, rival, score)
  
  ## Create current status frame
  cur_status = data.frame(Player= c(current_team, current_rival), 
                          Rating = c(current_game$TeamRateBefore[1],
                                     current_game$RivalRateBefore[1]), 
                          Deviation = c(100,100), 
                          Volatility = 0.06)
  
  # Calculate glicko
  team_elo = glicko2(th_short2, status = cur_status, init = c(1500, 50, 0.15))$ratings
  
  ## Filter by team name
  team_elo = filter(team_elo, Player == current_team)
  
  ## Fill team rate after
  current_game$TeamRateAfter = team_elo$Rating
  
  ## Fill player rate after
  current_game$PlayerRateAfter = 
    current_game$PlayerRateBefore + ((current_game$TeamRateAfter - current_game$TeamRateBefore)*current_game$min) / game_min
  
  
  calc_data = rbind(calc_data, current_game)
  #saveRDS(calc_data, "data/calc_data.RData")
}

## Add to history
hist_data = readRDS("arg_season_2022_23_rating")
hist_data = rbind(hist_data, calc_data)

# SAVE ONLY MANUAL
#saveRDS(hist_data, "data/arg_season_2022_23_rating")

# Filter duplicated
# Filter by team
players_unique = unique(hist_data[c("player", "team", "link")])

# Init data frame
all_last_p_rates = hist_data[0,]

# Make a loop      nrow(players_unique)
for (i in 1:nrow(players_unique))
{
  # Find player last rating
  player_link = players_unique$link[i]
  
  # Filter by player
  player_rate = hist_data %>% filter(link == player_link) %>%  arrange(desc(game_date))
  
  # Add player i rate
  last_player_rate = player_rate[1,]
  
  # Take the most recent rating
  all_last_p_rates = rbind(all_last_p_rates, last_player_rate)
}

## Let only unique players
all_last_p_rates = all_last_p_rates[!duplicated(all_last_p_rates), ]

all_last_p_rates$team = as.factor(all_last_p_rates$team)

# save only manual
#saveRDS(all_last_p_rates, "data/last_22_SW.RData")


calc_data <- readRDS("calc_data.RData")
