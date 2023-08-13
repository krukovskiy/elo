library(dplyr)
library(stringr)


# First read actual ratings
all_last_p_rates = readRDS("data/lastSW05062023.RData.RData")

# Read player profiles
playerProfile = readRDS("data/PlayersProfiles_07052023.RDS")

# Filter by actual date
filteredRatings = all_last_p_rates %>% filter(game_date > "2023-05-31"  & min > 0)

# Merge data frames
mergedData = merge(filteredRatings, playerProfile, by.x  = "link", by.y = "shortURL") 
mergedData = arrange(mergedData, desc(PlayerRateAfter)) %>% mutate(rank = row_number())

# Select data fo flourish
flourishData = data.frame(Name = word(mergedData$Apellidos, 1), 
                          Rating = mergedData$PlayerRateAfter,
                          rank = mergedData$rank,
                          team = mergedData$team,
                          age = mergedData$Edad,
                          image = "")
flourishData = flourishData[1:15,]

# Add market value
value = c("€0.2m","€8m","€4m","€0.8m","€3.5m",
          "€0.7m","€2m","€4m","€4.5m","€6.5m",
          "€4m","€0.5m","€4.5m","€4m","€3.5m")

flourishData$value = value
# Save to csv
write.csv(flourishData, "plots/allBubbles_flourish_players.csv")


## Now make teams
t_data = all_last_p_rates %>% filter(game_date > "2023-05-31" & competition == "Liga Profesional Argentina" & min > 0)
t_data$diff = t_data$TeamRateAfter - t_data$TeamRateBefore
t_data2 = unique(t_data[c("team", "TeamRateAfter","diff", "result")])
t_data2 = t_data2 %>% arrange(desc(TeamRateAfter)) %>% mutate(rank = row_number()) %>% relocate(rank, .before = team)
write.csv(t_data2, "plots/flourishTeamBubbles.csv")
