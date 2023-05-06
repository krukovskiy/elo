library(ggplot2)
library(tidyverse)
library(geofacet)
library(ggh4x)
library(gganimate)
library('reactable')
library(ggbreak)
library(patchwork)
library(png)
library(showtext)
library(plyr)
library(ggpubr) 
library(ggimage)

# Load rating data
rating_data = readRDS("arg_season_2022_rating")

# Add necessary fonts
font_add_google("Open Sans", family = "special")

# Filter by team
players_unique = unique(rating_data[c("player", "team")])

# Init data frame
all_last_p_rates = rating_data[0,]

# Make a loop      nrow(players_unique)
for (i in 1:nrow(players_unique))
{
  # Find player last rating
  player_name = players_unique$player[i]
  
  # Filter by player
  player_rate = rating_data %>% filter(player == player_name) %>%  arrange(desc(game_date))
  
  # Add player i rate
  last_player_rate = player_rate[1,]
  
  # Take the most recent rating
  all_last_p_rates = rbind(all_last_p_rates, last_player_rate)
}

## Let only unique players
all_last_p_rates = all_last_p_rates[!duplicated(all_last_p_rates), ]

all_last_p_rates$team = as.factor(all_last_p_rates$team)

# Plot all season graph
ggplot() +
  geom_point(data = all_last_p_rates,
             aes(
               x = reorder(team, PlayerRateAfter),
               y = PlayerRateAfter,
               colour = reorder(team, -TeamRateAfter))
             , size = 3.8,
             alpha = 0.8
  ) +
  coord_flip() +
  labs(title = "Players ratings distribution in LPF Argentina '22",
       x = "Team",
       y = "Rating",
       colour = "Team",
  ) +
  theme(legend.position='none')


# Filter by date
last_data = rating_data %>% filter(game_date >= "2022-10-23")

# Make team as factor
last_data$team = as.factor(last_data$team)

#### COUNT PLAYER INDIVIDUAL GRAPH
player_name = "Emiliano Vecchio"
player_rate = rating_data %>% filter(player == player_name) %>%  arrange(desc(game_date))

### Draw with boca juniors colors

myPlot = ggplot(data = player_rate, aes(x = game_date)) +
  geom_line(aes(y = PlayerRateAfter,
                colour = player_name), size = 1) +
  geom_line(aes(y = TeamRateAfter,
                colour = "Boca Juniors"), size = 1) +
  stat_difference(aes(ymin = TeamRateAfter, ymax = PlayerRateAfter),
                  alpha = 0.4,
                  show.legend = F) +
  scale_color_manual(name='Player/Team',
                     breaks=c(player_name, 'Boca Juniors'),
                     values=c('Emiliano Vecchio'= "#fbd455", 'Boca Juniors'='#133b5b')) +
  scale_fill_manual(values = c("#fbd455", "#133b5b")) +
  ylim(c(1470,1720)) +
  labs(title = "Personal rating over team rating after regular season 2022, 
Liga Profesional de Fútbol",
       x = "Date",
       y = "Rating",
       colour = "Team"
  )

### Draw with River Plate colors
myPlot2 = ggplot(data = player_rate, aes(x = game_date)) +
  geom_line(aes(y = PlayerRateAfter,
                colour = player_name), size = 1) +
  geom_line(aes(y = TeamRateAfter,
                colour = "River Plate"), size = 1) +
  stat_difference(aes(ymin = TeamRateAfter, ymax = PlayerRateAfter),
                  alpha = 0.4,
                  show.legend = F) +
  scale_color_manual(name='Player/Team',
                     breaks=c(player_name, 'River Plate'),
                     values=c('Julián Álvarez'= "#eb192e", 'River Plate'='#000000')) +
  scale_fill_manual(values = c("#eb192e", "#000000")) +
  ylim(c(1470,1720)) +
  labs(title = "Personal rating over team rating after regular season 2022, 
Liga Profesional de Fútbol",
       x = "Date",
       y = "Rating",
       colour = "Team"
  )



### Draw with Racing Club colors
# Load Racing logo
im <- readPNG("logo/racing-logo-escudo.png")
im2 <- matrix(rgb(im[,,1],im[,,2],im[,,3], im[,,4] * 0.3), nrow=dim(im)[1]) ## you can change 0.5 to change the alpa

myPlot = ggplot(data = player_rate, aes(x = game_date)) +
  geom_line(aes(y = PlayerRateAfter,
                colour = player_name), size = 1) +
  geom_line(aes(y = TeamRateAfter,
                colour = "Racing Club"), size = 1) +
  stat_difference(aes(ymin = TeamRateAfter, ymax = PlayerRateAfter),
                  alpha = 0.4,
                  show.legend = F) +
  scale_color_manual(name='Player/Team',
                     breaks=c(player_name, 'Racing Club'),
                     values=c('Emiliano Vecchio'= "#029cdc", 'Racing Club'='#6f6f6f')) +
  scale_fill_manual(values = c("#029cdc", "#6f6f6f", "#ababab")) +
  ylim(c(1470,1720)) +
  labs(title = "Personal rating over team rating 
after regular season 2022, 
Liga Profesional de Fútbol Argentina",
       x = "Date",
       y = "Rating",
       colour = "Team"
  )


### Draw with San Lorenzo colors
myPlot = ggplot(data = player_rate, aes(x = game_date)) +
  geom_line(aes(y = PlayerRateAfter,
                colour = player_name), size = 1) +
  geom_line(aes(y = TeamRateAfter,
                colour = "San Lorenzo"), size = 1) +
  stat_difference(aes(ymin = TeamRateAfter, ymax = PlayerRateAfter),
                  alpha = 0.4,
                  show.legend = F) +
  scale_color_manual(name='Player/Team',
                     breaks=c(player_name, 'San Lorenzo'),
                     values=c('Name'= "#ec212d", 'San Lorenzo'='#273b56')) +
  scale_fill_manual(values = c("#ec212d", "#273b56", "#ababab")) +
  ylim(c(1470,1720)) +
  labs(title = "Personal rating over team rating after regular season 2022, 
Liga Profesional de Fútbol",
       x = "Date",
       y = "Rating",
       colour = "Team"
  )
