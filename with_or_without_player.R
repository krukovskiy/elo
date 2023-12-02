library(dplyr)
library(tidyverse)

calc_data <- readRDS("data/calc_data16082023.RData")

# Select team
team = "Boca Juniors"
seleccion = "Argentina U20"

# Select a player or link
player = "V. Barco"
link = "/players/nicolas-valentini/698375/"
# Select all games of the team
games = calc_data %>% filter(game_date > "2023-01-01" &  team == !!team | team == !!seleccion)
games = games %>% distinct(game_date, team, .keep_all=TRUE)


# Select all games of a player
#pg = calc_data %>% filter(player == !!player & game_date > "2023-01-01" & min > 0)
#pg = pg %>% distinct(game_date, player, .keep_all=TRUE)

# Select all games of a link
pg = calc_data %>% filter(link == !!link & game_date > "2023-01-01" & min > 0)
pg = pg %>% distinct(game_date, link, .keep_all=TRUE)

# Found games without a player
games_w = anti_join(games, pg, by = c("game_date", "team"))

# Count occurrencies
team_stat = table(games$result)
player_stat = table(pg$result)
team_w_stat = table(games_w$result)

as.numeric(team_stat["p1"]) + as.numeric(team_stat["d"])
# stats with the player
table(pg$result)

# stats without the player
table(games_w$result)
