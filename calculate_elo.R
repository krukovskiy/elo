library(dplyr)
library(tibble)
library(PlayerRatings)


# Select a game

## Load data frame
games = readRDS("all_games_2021")

## Select country
country = "Argentina"

# Code to change Gozalez
### games$player[games$player == "Francisco González" & games$team == 'CC Córdoba'] <- "Francisco González 2"

## Probably this should be deleted 
games <- games %>%
  add_column(PlayerRateBefore = NA,
             PlayerRateAfter = NA,
             TeamRateBefore = NA,
             TeamRateAfter = NA,
             RivalRateBefore = NA)

## Create empty dataframe with same columns to make furthermore calculated table
calc_data <-games[0,]

## Define all game pairs (team * date)
game_pairs = unique(games[c("game_date", "team")])

## Here starts a global loop   # 1:nrow(game_pairs)
for (k in 1:nrow(game_pairs))
{


## Select current game frame                           [k,1]
current_game = games %>% filter(game_date == game_pairs[k,1] & team == game_pairs[k,2])

## Select game date
current_date = current_game$game_date[1]

## Select team
current_team = current_game$team[1]

## Calculate player rate before
for(i in 1:nrow(current_game)) {       # for-loop over columns
 
## Select player
current_player = current_game$player[i]

# Find before occurrences
rate_before = calc_data %>% filter(game_date < current_date & player == current_player) %>% arrange(desc(game_date))

# If there is now rating then starting rating is 1500
if (nrow(rate_before) == 0)
  {
  current_game$PlayerRateBefore[i] = 1500
  } else {
  current_game$PlayerRateBefore[i] = rate_before$PlayerRateAfter[1]
}
} # end of the FOR loop

## Calculate team rate before. This formula could be changed. Not sure for 100%
current_game$TeamRateBefore = sum(current_game$PlayerRateBefore * (current_game$min/90))/ sum(current_game$min/90)

## !!!!!! RIVAL FRAME !!!!!!  Calculate rival rate before
## Select RIVAL
current_rival = current_game$rival[1]

## Change current game to the RIVAL
current_rival_game = games %>% filter(game_date == current_date & team == current_rival)

## Calculate player rate before
for(i in 1:nrow(current_rival_game)) {       # for-loop over columns
  
  ## Select player
  current_player = current_rival_game$player[i]
  
  # Find before occurrences
  rate_before = calc_data %>% filter(game_date < current_date & player == current_player) %>% arrange(desc(game_date))
  
  # If there is now rating then starting rating is 1500
  if (nrow(rate_before) == 0)
  {
    current_rival_game$PlayerRateBefore[i] = 1500
  }  else {
    current_rival_game$PlayerRateBefore[i] = rate_before$PlayerRateAfter[1]
}
}
## Calculate rival rate before. This formula could be changed. Not sure for 100%
current_game$RivalRateBefore = sum(current_rival_game$PlayerRateBefore * (current_rival_game$min/90))/ sum(current_rival_game$min/90)


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
  current_game$PlayerRateBefore + ((current_game$TeamRateAfter - current_game$TeamRateBefore)*current_game$min) / 90


calc_data = rbind(calc_data, current_game)
}


# save results to RDS
saveRDS(calc_data, "arg_season_2021-22_rating")
