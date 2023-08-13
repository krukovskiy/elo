library(tidyverse)


# Load rating data
rating_data = readRDS("data/calc_data16072023.RData")


## !!!!!!!!!!!!!! Boxplot animation  (for Flourish too start here)  !!!!!!!!!!!!!!!!!!!!!!

## PLAYER TIMELINE ANIMATED PLOT ##
# select team
current_team = "River Plate"
players_rates = rating_data %>% filter(team == current_team) %>% arrange(desc(game_date)) 
#players_rates$game_date = as.factor(players_rates$game_date)

# Get vector of all players
players_cur_unique = unique(players_rates$player)

# Find players in each date
game_cur_dates = unique(players_rates$game_date) 
game_cur_dates = sort(game_cur_dates)

# Use other variable to not break the original
players_rates2 = players_rates

for (i in 1:length(game_cur_dates)) {
  players_rates_cur = players_rates %>% filter(game_date == game_cur_dates[i])
  tt = players_cur_unique %in% players_rates_cur$player
  tt2 = data.frame(game_date =game_cur_dates[i], player = players_cur_unique[!tt])
  
  players_rates2 = rbind.fill(players_rates2, tt2) ## agregamos chicos que no estaban alla
  players_rates2 = players_rates2 %>% group_by(player) %>% arrange(game_date) %>% fill(PlayerRateAfter) 
  players_rates2 = players_rates2 %>% fill(c("team", "result", "rival", 'TeamRateBefore', "TeamRateAfter", "RivalRateBefore"))
  
}

## Define current rank
game_cur_dates = unique(players_rates2$game_date) 
game_cur_dates = sort(game_cur_dates)

# Init variable
players_rates_ranked = players_rates2[0,]
#players_rates_ranked = cbind(players_rates_ranked, cur_rank = "")

# Make Player Rates Ranked
for (i in 1:length(game_cur_dates)) {
  players_rates_cur = players_rates2 %>% drop_na(PlayerRateAfter) %>% filter(game_date == game_cur_dates[i])
  players_rates_cur = players_rates_cur %>% arrange (desc(PlayerRateAfter)) %>% rowid_to_column()
  players_rates_ranked = rbind(players_rates_ranked, players_rates_cur)
}

# Preparing dataset for Flourish
pre = data.frame(player = players_cur_unique, team = current_team)
# Make Player Rates for Export Flourish
for (i in 1:length(game_cur_dates)) {
  players_rates_cur = players_rates2 %>% drop_na(PlayerRateAfter) %>% filter(game_date == game_cur_dates[i])
  #pre = left_join(pre, players_rates_cur[, c("player", "PlayerRateAfter")], by = "player", all = TRUE)
  pre = left_join(pre, players_rates_cur[, c("player", "PlayerRateAfter")], by = "player")
  pre[is.na(pre)] = ""
  names(pre)[names(pre) == 'PlayerRateAfter'] <- as.character(game_cur_dates[i])
  
}
# Save to csv
write_csv(pre, "pre.csv")








## TEAM TIMELINE ANIMATED PLOT ##

## Filter by teams
team_data = rating_data %>% filter(game_date > "2023-01-01" & competition == "Liga Profesional Argentina" & min > 0)
team_data$diff = team_data$TeamRateAfter - team_data$TeamRateBefore
t_data2 = unique(team_data[c("game_date", "team", "TeamRateAfter","diff", "result")])

## Define current rank
game_cur_dates = unique(t_data2$game_date) 
game_cur_dates = sort(game_cur_dates)

# Preparing dataset for Flourish
# Get vector of all players
t_cur_unique = unique(t_data2$team)
# Use other variable to not break the original
t_rates2 = t_data2

for (i in 1:length(game_cur_dates)) {
  t_rates_cur = t_data2 %>% filter(game_date == game_cur_dates[i])
  tt = t_cur_unique %in% t_rates_cur$team
  tt2 = data.frame(game_date =game_cur_dates[i], team = t_cur_unique[!tt])
  
  t_rates2 = rbind.fill(t_rates2, tt2) ## agregamos chicos que no estaban alla
  t_rates2 = t_rates2 %>% group_by(team) %>% arrange(game_date) %>% fill(TeamRateAfter) 
  t_rates2 = t_rates2 %>% fill(c("team", "result", "TeamRateAfter"))
  
}

### FILTER TOP-7 ###

top7 = t_rates2 %>% arrange(desc(game_date))  
last_date = top7$game_date[1]
top7 = top7 %>% filter(game_date == last_date) %>% arrange(desc(TeamRateAfter))  
top7 = top7[1:7,]
t_rates2 = t_rates2 %>% filter(team %in% top7$team)

# Get vector of all players
t_cur_unique = unique(t_rates2$team)
t_pre = data.frame(team = t_cur_unique)

## Change columns and rows
for (i in 1:length(game_cur_dates)) {
  t_rates_cur = t_rates2 %>% drop_na(TeamRateAfter) %>% filter(game_date == game_cur_dates[i])
  #pre = left_join(pre, players_rates_cur[, c("player", "PlayerRateAfter")], by = "player", all = TRUE)
  t_pre = left_join(t_pre, t_rates_cur[, c("team", "TeamRateAfter")], by = "team")
  t_pre[is.na(t_pre)] = ""
  names(t_pre)[names(t_pre) == 'TeamRateAfter'] <- as.character(game_cur_dates[i])
  
}

# Save to csv
write_csv(t_pre, "data/t_pre.csv")
