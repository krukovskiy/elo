library(tidyverse)

## Calculate iniciales

# Load base
rating_data = readRDS("arg_season_2022_rating")

## Create empty frame
inicial_rates = rating_data[0,]

rival_rates = rating_data[0,]

## Team #1
current_date = "2023-01-07"
team = "Independiente"
rival = "Boca Juniors"
players_inicio = c("Rodrigo Rey",
                   "Luciano Gómez",
                   "Sergio Barreto",
                   "Edgar Elizalde",
                   "Ayrton Costa",
                   "Lucas González",
                   "Iván Marcone",
                   "Agustín Mulet",
                   "Juan Cazares",
                   "Rodrigo Márquez",
                   "Matías Giménez Rojas")
## Team #2
rival_inicio = c("Agustín Rossi",
                 "Luis Advíncula",
                 "Facundo Roncaglia",
                 "Agustín Sández",
                 "Frank Fabra",
                 "Juan Ramírez",
                 "Alan Varela",
                 "Guillermo Matías Fernández",
                 "Norberto Briasco",
                 "Nicolás Orsini",
                 "Sebastián Villa")


## Calculate player rate before
for(i in 1:length(players_inicio)) {       # for-loop over columns
  
  ## Add row for next player
  inicial_rates[i,] <- NA
  inicial_rates$game_date[i] = game_date
  inicial_rates$team[i] = team
  inicial_rates$rival[i] = rival
  inicial_rates$player[i] = players_inicio[i]
  
  ## Select player
  current_player = players_inicio[i]
  
  # Find before occurrences
  rate_before = all_last_p_rates %>% filter(game_date < current_date & player == current_player) %>% arrange(desc(game_date))
  
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
for(i in 1:length(rival_inicio)) {       # for-loop over columns
  
  ## Add row for next player
  rival_rates[i,] <- NA
  rival_rates$game_date[i] = game_date
  rival_rates$team[i] = rival
  rival_rates$rival[i] = team
  rival_rates$player[i] = rival_inicio[i]
  
  ## Select player
  current_player = rival_inicio[i]
  
  # Find before occurrences
  rate_before = all_last_p_rates %>% filter(game_date < current_date & player == current_player) %>% arrange(desc(game_date))
  
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
