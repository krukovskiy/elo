library(dplyr)
library(tibble)
library(PlayerRatings)
library(tidyr)
library(reactable)
library(htmltools)
library(stringr)

# Select a game
## Load data with older calculations
#all_data = read.csv(file = "all.csv")
all_data = readRDS("all_last_22_23.RData")
all_data = all_data %>% as.data.frame()
all_data$game_date = as.POSIXct(all_data$game_date,format="%Y-%m-%d")
#### ADD RATES AFTER A GAME

# select how long was the game
game_min = 113
## Team #1 Input data manually
current_date = "2023-01-24"
team = "Patronato"
rival = "Gimnasia y Tiro"
result1 = "w"
country1 = "Argentina"
country2 = "Argentina"
after_rates1 = all_data[0,]
players_after = c("Lautaro Geminiani",
                  "Sergio Maximiliano Ojeda",
                  "Matías Budiño",
                  "Facundo Cobos",
                  "Alexander Sosa",
                  "Lucas Kruspzky",
                  "Jorge Valdez Chamorro",
                  "Ignacio Russo",
                  "Lautaru Comas",
                  "Enzo Roberto Díaz",
                  "Nicolás Domingo",
                  "Brian Nievas",
                  "Nazareno Solís",
                  "Kevin González",
                  "Cristian González",
                  "Gastón Novero")
mins_after = c("113", "113", "113", "39", "39", "113", "85", "85", "28", "74", "113", "28", "57", "74", "113", "56")

## Calculate player rate before
for(i in 1:length(players_after)) {       # for-loop over columns
  
  ## Add row for next player
  after_rates1[i,] <- NA
  after_rates1$game_date[i] = current_date
  after_rates1$team[i] = team
  after_rates1$rival[i] = rival
  after_rates1$min[i] = mins_after[i]
  after_rates1$player[i] = players_after[i]
  after_rates1$result[i] = result1
  after_rates1$country[i] = country1
}

## Team #2 Input data manually
current_date = "2023-01-24"
team2 = "Gimnasia y Tiro"
rival2 = "Patronato"
result2 = "l"
after_rates2 = all_data[0,]
after_rates2$team = as.character(after_rates2$team)
players_after2 = c("Federico Cosentino",
                   "Fabricio Rojas",
                   "Ignacio Sanabria",
                   "Adolfo Tallura",
                   "Wálter Busse",
                   "Ezequiel Cérica",
                   "Daniel Carrasco",
                   "Matías Birge",
                   "Guido Milán",
                   "Daniel Abello",
                   "Ruben Villarreal",
                   "Facundo Heredia",
                   "Emiliano Blanco",
                   "Ivo Chavés",
                   "Adrián Toloza",
                   "Joel Martínez")
mins_after2 = c("113","72","41","113","113","113","85", "28", "113", "85", "28","72", "61", "113", "52", "61")

## Calculate player rate before
for(i in 1:length(players_after2)) {       # for-loop over columns
  
  ## Add row for next player
  after_rates2[i,] <- NA
  after_rates2$game_date[i] = current_date
  after_rates2$team[i] = team2
  after_rates2$rival[i] = rival2
  after_rates2$min[i] = mins_after2[i]
  after_rates2$player[i] = players_after2[i]
  after_rates2$result[i] = result2
  after_rates2$country[i] = country2
}

### Aggregate all
games = rbind(after_rates1, after_rates2)




########### HERE START AGG WITH PREPARED DATA FRAME!!!!!!!! ##################


###########                               ####################
## Create empty dataframe with same columns to make furthermore calculated table
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
    
    # Find before occurrences
    rate_before = all_data %>% filter(game_date < current_date & player == current_player) %>% arrange(desc(game_date))
    
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
    
    # Find before occurrences
    rate_before = all_data %>% filter(game_date < current_date & player == current_player) %>% arrange(desc(game_date))
    
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
  saveRDS(calc_data, "calc_data.RData")
}

## Add to history
hist_data = readRDS("arg_season_2022_23_rating")
hist_data = rbind(hist_data, calc_data)

# save only manual
#saveRDS(hist_data, "arg_season_2022_23_rating")

# Filter duplicated
# Filter by team
players_unique = unique(hist_data[c("player", "team")])

# Init data frame
all_last_p_rates = hist_data[0,]

# Make a loop      nrow(players_unique)
for (i in 1:nrow(players_unique))
{
  # Find player last rating
  player_name = players_unique$player[i]
  
  # Filter by player
  player_rate = hist_data %>% filter(player == player_name) %>%  arrange(desc(game_date))
  
  # Add player i rate
  last_player_rate = player_rate[1,]
  
  # Take the most recent rating
  all_last_p_rates = rbind(all_last_p_rates, last_player_rate)
}

## Let only unique players
all_last_p_rates = all_last_p_rates[!duplicated(all_last_p_rates), ]

all_last_p_rates$team = as.factor(all_last_p_rates$team)

saveRDS(all_last_p_rates, "all.RData")


calc_data <- readRDS("calc_data.RData")
