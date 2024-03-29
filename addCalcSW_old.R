library(glicko2)

## Add rating to a saved dataset



# 1. First get links
# We initialize calc_data with history data because in this file we make add calculations
calc_data <- readRDS("data/calc_data17022023.RData")
#URL = "https://int.soccerway.com/matches/2023/02/05/argentina/primera-division/boca-juniors/club-atletico-central-cordoba-de-santiago/3982559/"
URL = read.csv2("data/links18022023.csv", header = FALSE)$V1

# Add game logs to the one big frame
i <- 1

# Be polite
# Call these variables cause we use them furthermore
cur_frame <- c()
game_frames <- c()
while(i <= length(URL)) {
  cur_frame  <- getGameFrameSW(URL[i], competition_name)
  game_frames[[i]] <- cur_frame
  i <- i + 1
  Sys.sleep(2)
}

big_data = do.call(rbind,  game_frames)
big_data = big_data %>% arrange(game_date)
games = big_data


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
  
  ## Convert letters to points !!! INCLUDE "pw" (penalty win) AND "pl"
  th_short$score <- gsub('w', 1,
                         gsub('p1', 1,
                              gsub('d', .5,
                                   gsub('p0', 0, 
                                        gsub('l', 0, th_short$result)))))
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
  ## SAVE ONLY MANUALLY
  #saveRDS(calc_data, "data/calc_data18022023.RData")
}


# Filter duplicated
# Filter by team
players_unique = unique(calc_data[c("player", "team", "link")])

# Init data frame
all_last_p_rates = calc_data[0,]

# Make a loop      nrow(players_unique)
for (i in 1:nrow(players_unique))
{
  # Find player last rating
  player_link = players_unique$link[i]
  
  # Filter by player
  player_rate = calc_data %>% filter(link == player_link) %>%  arrange(desc(game_date))
  
  # Add player i rate
  last_player_rate = player_rate[1,]
  
  # Take the most recent rating
  all_last_p_rates = rbind(all_last_p_rates, last_player_rate)
}

## Let only unique players
all_last_p_rates = all_last_p_rates[!duplicated(all_last_p_rates), ]

all_last_p_rates$team = as.factor(all_last_p_rates$team)

## SAVE ONLY MANUALLY
#saveRDS(all_last_p_rates, "data/lastSW18022023.RData")
