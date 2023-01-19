# Load rating data
rating_data = readRDS("arg_season_2022_rating")

# Add necessary fonts
font_add_google("Open Sans", family = "special")

## We need all_last_p_rates from plot_ratings.R
top10 = all_last_p_rates %>% arrange(desc(PlayerRateAfter))
top10 = top10[1:10,]

game_cur_dates = unique(rating_data$game_date) 

players_rates_cur = rating_data[0,]

# Preparing dataset for Flourish
# Make Player Rates for Export Flourish

pre = data.frame(player = top10$player, team = top10$team)
for (i in 1:length(game_cur_dates)) {
  pre0 = NULL
  for (j in 1:nrow(top10)) {
  players_rates_cur = rating_data %>% filter(game_date == game_cur_dates[i] & player == top10$player[j])
  rate_cur = players_rates_cur$PlayerRateAfter
  if (nrow(players_rates_cur) == 0 & i>1){
    players_rates_cur =  rbind(players_rates_cur, data.frame(
      game_date = as.character(game_cur_dates[i]),
      team = as.character(top10$team[j]),
      player = top10$player[j],
      min = NA,
      rival = NA,
      result = NA,
      PlayerRateBefore = NA,
      PlayerRateAfter = pre[j,i+1],
      TeamRateBefore = NA,
      TeamRateAfter = NA,
      RivalRateBefore = NA,
      country = NA))
    }  
  pre0 = rbind(pre0, players_rates_cur)
  
  }
  pre = left_join(pre, pre0[, c("player", "PlayerRateAfter")], by = "player", all = TRUE)
  pre[is.na(pre)] = ""
  names(pre)[names(pre) == 'PlayerRateAfter'] <- as.character(game_cur_dates[i])
  
}

# Save to csv
write_csv(pre, "top10.csv")
