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
library(data.table)

# Load rating data
rating_data = readRDS("data/calc_data12072023.RData")

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
last_data = rating_data %>% filter(game_date >= "2023-01-01")

# Make team as factor
last_data$team = as.factor(last_data$team)

#### COUNT PLAYER INDIVIDUAL GRAPH
player_name = "M. Casco"
player_rate = rating_data %>% filter(player == player_name & game_date >= "2022-01-01") %>%  arrange(desc(game_date))

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
                     values=c('M. Casco'= "#fbd455", 'Boca Juniors'='#133b5b')) +
  scale_fill_manual(values = c("#fbd455", "#133b5b")) +
  ylim(c(1470,1890)) +
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
                     values=c('M. Casco'= "#eb192e", 'River Plate'='#000000')) +
  scale_fill_manual(values = c("#eb192e", "#000000")) +
  ylim(c(1470,1890)) +
  labs(title = "Personal rating over team rating after regular season 2022, 
Liga Profesional de Fútbol",
       x = "Date",
       y = "Rating",
       colour = "Team"
  )

#transition_reveal(game_date)
#animate(myPlot, duration = 5, fps = 20, width = 900, height = 800, renderer = gifski_renderer())
#anim_save("123.gif")

# Highlighting top 5%
ggplot() +
  geom_point(data = all_last_p_rates,
             aes(
               x = reorder(team, PlayerRateAfter),
               y = PlayerRateAfter,
               colour =  PlayerRateAfter > quantile(PlayerRateAfter, .95))
             , size = 3.8,
             alpha = 0.8
  ) +
  scale_color_manual(name='Player/Team',
                     values=c("#B2BEB5", '#43A1D5')) +
  coord_flip() +
  labs(title = "Top 5% players in LPF Argentina '22",
       x = "Team",
       y = "Rating",
       colour = "Team",
  ) +
  theme(legend.position='none')

## Plot Team Rate

# First we make filter by team and date
team_date = rating_data %>% filter(team == "Aldosivi") %>%  arrange(desc(game_date))
# Then filter by unique values (tr - teamrate)
tr = distinct(team_date, game_date, .keep_all = T)


## Plot Racing Top3 Players

# Count team
team_name = "River Plate"
rtop = all_last_p_rates %>% filter(team == team_name) %>% arrange(player)
barplot(rtop$PlayerRateAfter, names.arg=rtop$player, ylim = c(1420, 1880), xpd = F, 
        las = 2)
rtop$player = as.factor(rtop$player) # Make factor to arrange players

# Calculate last team average of Racing
last_team_average = rating_data %>% filter(team == team_name & game_date > "2022-10-21")
last_team_average = last_team_average$TeamRateAfter[1]

# Paths to logo files
path_boca = "logo/Boca Juniors.png"
img_boca <- readPNG(path_boca, native = TRUE)
path_racing = "logo/circles/Racing Club.png"
img_racing <- readPNG(path_racing, native = TRUE)
path_slorenzo = "logo/circles/San Lorenzo.png"
img_slorenzo <- readPNG(path_slorenzo, native = T)
path_lpfa = "logo/lpfa2.png"
img_lpfa <- readPNG(path_lpfa, native = T)
path_tigre = "logo/Tigre.png"
img_tigre <- readPNG(path_tigre)
path_river = "logo/River Plate.png"
img_river <- readPNG(path_river, native = T)
path_huracan = "logo/Huracán.png"
img_huracan <- readPNG(path_huracan, native = T)

# Draw Racing Club
ggplot(rtop)+
  geom_bar(mapping = aes(x = reorder(player,PlayerRateAfter),
                         y = PlayerRateAfter,
                         fill = PlayerRateAfter > last_team_average,
                         ), stat = "identity",
           position = "stack") +
  
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_manual(values=c("#6f6f6f", '#029cdc')) +
  coord_cartesian(ylim = c(1470, 1670)) +
  geom_hline(yintercept = last_team_average, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = last_team_average, col = '#1893c6', size = .4) +
  annotate("text", x=4.82, y=last_team_average + 6, label= "Racing Club Current", col = '#1893c6') +
  geom_hline(yintercept = 1621, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = 1621, col = '#133b5b', size = .4) +
  annotate("text", x=6.44, y=1621 + 6, label= "Boca Juniors Current (1st place)", col = '#133b5b') +
  geom_hline(yintercept = 1500, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = 1500, col = '#6f6f6f', size = .4) +
  annotate("text", x=3.1, y=1500 + 6, label= "League Average", col = '#6f6f6f') +
  annotation_raster(img_boca, ymin = 1623,ymax= 1631,xmin = 1.1,xmax = 1.8) +
  annotation_raster(img_racing, ymin = 1585,ymax= 1593,xmin = 1.1,xmax = 1.8) +
  labs(title = "Top players of Racing Club",
     y = "Rating",
     family = "special"
    ) +
  theme(legend.position='none',
        axis.title.x=element_blank())
  
## !!!!!!!!!!!!!! Boxplot animation  (for Flourish too start here)  !!!!!!!!!!!!!!!!!!!!!!
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

# Preparando personal conjunto de datos flourish
player_fr = data.frame(t(player_rate))
colnames(player_fr) <- as.character(player_rate$game_date)
player_fr = cbind(Name = rownames(player_fr), player_fr)
player_fr$Name[player_fr$Name == "PlayerRateAfter"] <- player_name    
player_fr$Name[player_fr$Name == "TeamRateAfter"] <- team_name
player_fr$Name[player_fr$Name == "TeamRateAfter"] <- team_name
player_fr = player_fr[c(8,10),]
write_csv(player_fr, "player_fr.csv")


# Rename column
names(players_rates_ranked)[names(players_rates_ranked) == "rowid"] = "rank"
# minus 1400 points
players_rates_ranked[,"PlayerRateAfterDec"] = players_rates_ranked[,"PlayerRateAfter"] - 1400

# Left top10
#players_rates_ranked = players_rates_ranked %>% filter(rank <= 10 )

# Barplot df = players_rates_ranked
im <- readPNG("logo/circles/Racing Club.png")
im2 <- matrix(rgb(im[,,1],im[,,2],im[,,3], im[,,4] * 0.3), nrow=dim(im)[1]) ## you can change 0.5 to change the alpa


p = ggplot(players_rates_ranked) +
  geom_col(aes(x=34 - rank, y=PlayerRateAfterDec,
               group=player,
               fill= rank == 1),
           width=0.7,
           show.legend = FALSE) +
  geom_text(size = 5, aes(x=34 - rank, y=0,
                          label=player, group=player),
            hjust=1.25) +
  theme_minimal()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(5,5,5,15),
                           'lines'),
        plot.title = element_text(size = 14, face = "bold")) +
  scale_fill_manual(values=c('#029cdc', "#6f6f6f" )) +
  coord_flip(clip='off') + 
  annotate("text", x  = -1, y = c(100, 200, 300), label = c("1500", "1600", "1700"), size = 5) +
  annotation_raster(im2, ymin = -70 ,ymax= -34,xmin = 35.2, xmax = 37.3) +
  ggtitle('PERSONAL PLAYER RATING
          RACING CLUB
          SEASON 2022
          Date: {closest_state}') +             # title with the timestamp period
  transition_states(game_date,
                    transition_length = 1,
                    state_length = 1) +
  exit_fly(x_loc = 0, y_loc = 0) +         # chart exit animation params
  enter_fly(x_loc = 0, y_loc = 0) 


## duration = 10, fps = 3, width = 720, height = 1280,
## duration = 30, fps = 60, width = 1080, height = 1920,
animate(p, duration = 12, fps = 3, width = 720, height = 1280, renderer = gifski_renderer())
anim_save("long.gif")


############ SAN LORENZO BLOCK #################


# Draw San Lorenzo barplot
ggplot(rtop)+
  geom_bar(mapping = aes(x = reorder(player,PlayerRateAfter),
                         y = PlayerRateAfter,
                         fill = PlayerRateAfter > last_team_average,
  ), stat = "identity",
  position = "stack") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_manual(values=c("#273b56", '#ec212d')) +
  coord_cartesian(ylim = c(1470, 1670)) +
  geom_hline(yintercept = last_team_average, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = last_team_average, col = '#ec212d', size = .4) +
  annotate("text", x=4.82, y=last_team_average + 6, label= "San Lorenzo Current", col = '#ec212d') +
  geom_hline(yintercept = 1621, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = 1621, col = '#133b5b', size = .4) +
  annotate("text", x=6.24, y=1621 + 6, label= "Boca Juniors Current (1st place)", col = '#133b5b') +
  geom_hline(yintercept = 1500, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = 1500, col = '#6f6f6f', size = .4) +
  annotate("text", x=3.1, y=1500 + 6, label= "League Average", col = '#6f6f6f') +
  annotation_raster(img_boca, ymin = 1623,ymax= 1631,xmin = 1.1,xmax = 1.8) +
  annotation_raster(img_slorenzo, ymin = 1585,ymax= 1593,xmin = 1.1,xmax = 1.8) +
  labs(title = "Top players of San Lorenzo",
       y = "Rating",
       family = "special"
  ) +
  theme(legend.position='none',
        axis.title.x=element_blank())


### Personal graph
ggplot(data = player_rate, aes(x = game_date)) +
  geom_line(aes(y = PlayerRateAfter,
                colour = player_name), size = 1) +
  geom_line(aes(y = TeamRateAfter,
                colour = "San Lorenzo"), size = 1) +
  stat_difference(aes(ymin = TeamRateAfter, ymax = PlayerRateAfter),
                  alpha = 0.4,
                  show.legend = F) +
  scale_color_manual(name='Jugador/Equipo',
                     breaks=c(player_name, 'San Lorenzo'),
                     values=c("Gastón Hernández"= "#ec212d", 'San Lorenzo'='#273b56')) +
  scale_fill_manual(values = c("#ec212d", "#273b56", "#ababab")) +
  ylim(c(1470,1650)) +
  labs(title = "Calificación Elo personal/equipo, 
Liga Profesional de Fútbol 2022, Argentina",
       x = "Fecha",
       y = "Calificación Elo",
       colour = "Team")


####  BOCA JUNIORS BLOCK!!!
ggplot(rtop)+
  geom_bar(mapping = aes(x = reorder(player,PlayerRateAfter),
                         y = PlayerRateAfter,
                         fill = PlayerRateAfter > last_team_average,
  ), stat = "identity",
  position = "stack") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_manual(values=c("#133b5b", '#fbd455')) +
  coord_cartesian(ylim = c(1425, 1710)) +
  geom_hline(yintercept = last_team_average, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = last_team_average, col = '#133b5b', size = .4) +
  annotate("text", x=6.6, y=1625 + 6, label= "Última calificación del equipo", col = '#133b5b') +
  geom_hline(yintercept = 1500, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = 1500, col = '#6f6f6f', size = .4) +
  annotate("text", x=5.1, y=1505 + 6, label= "Promedio de la Liga", col = '#6f6f6f') +
  annotation_raster(img_boca, ymin = 1625,ymax= 1638,xmin = 1.1,xmax = 1.8) +
  annotation_raster(img_lpfa, ymin = 1505,ymax= 1520,xmin = 1.1,xmax = 1.8) +
  labs(title = "Calificación Elo de jugadores de Boca Juniors",
       y = "Calificación",
       family = "special"
  ) +
  theme(legend.position='none',
        axis.title.x=element_blank())

### Personal graph
ggplot(data = player_rate, aes(x = game_date)) +
  geom_line(aes(y = PlayerRateAfter,
                colour = player_name), size = 1) +
  geom_line(aes(y = TeamRateAfter,
                colour = "Boca Juniors"), size = 1) +
  stat_difference(aes(ymin = TeamRateAfter, ymax = PlayerRateAfter),
                  alpha = 0.4,
                  show.legend = F) +
  scale_color_manual(name='Jugador/Equipo',
                     breaks=c(player_name, 'Boca Juniors'),
                     values=c("Óscar Romero"= "#fbd455", 'Boca Juniors'='#133b5b')) +
  scale_fill_manual(values = c("#fbd455", "#133b5b", "#ababab")) +
  ylim(c(1450,1720)) +
  labs(title = "Calificación Elo personal/equipo, 
Liga Profesional de Fútbol 2022, Argentina",
       x = "Fecha",
       y = "Calificación Elo",
       colour = "Team")


####  CLUB ATLETICO TIGRE BLOCK!!!
ggplot(rtop)+
  geom_bar(mapping = aes(x = reorder(player,PlayerRateAfter),
                         y = PlayerRateAfter,
                         fill = PlayerRateAfter > last_team_average,
  ), stat = "identity",
  position = "stack") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_manual(values=c("#2e2979", '#c91f28')) +
  coord_cartesian(ylim = c(1425, 1650)) +
  geom_hline(yintercept = 1621, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = 1621, col = '#133b5b', size = .4) +
  annotate("text", x=4.8, y=1625 + 6, label= "Boca Juniors (1 lugar)", col = '#133b5b') +
  geom_hline(yintercept = last_team_average, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = last_team_average, col = '#2e2979', size = .4) +
  annotate("text", x=5.8, y=last_team_average + 8, label= "Última calificación del equipo", col = '#133b5b') +
  geom_hline(yintercept = 1500, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = 1500, col = '#6f6f6f', size = .4) +
  annotate("text", x=4.52, y=1505 + 6, label= "Promedio de la Liga", col = '#6f6f6f') +
  annotation_raster(img_boca, ymin = 1625,ymax= 1638,xmin = 1.1,xmax = 1.9) +
  annotation_raster(img_tigre, ymin = last_team_average + 3,ymax= last_team_average + 20,xmin = 1.1,xmax = 1.8) +
  annotation_raster(img_lpfa, ymin = 1505,ymax= 1520,xmin = 1.1,xmax = 1.8) +
  labs(title = "Calificación Elo de jugadores de CA Tigre",
       y = "Calificación",
       family = "special"
  ) +
  theme(legend.position='none',
        axis.title.x=element_blank())

### Personal graph
ggplot(data = player_rate, aes(x = game_date)) +
  geom_line(aes(y = PlayerRateAfter,
                colour = player_name), size = 1) +
  geom_line(aes(y = TeamRateAfter,
                colour = "CA Tigre"), size = 1) +
  stat_difference(aes(ymin = TeamRateAfter, ymax = PlayerRateAfter),
                  alpha = 0.4,
                  show.legend = F) +
  scale_color_manual(name='Jugador/Equipo',
                     breaks=c(player_name, 'CA Tigre'),
                     values=c("Lucas Menossi"= "#c91f28", 'CA Tigre'='#2e2979')) +
  scale_fill_manual(values = c("#c91f28", "#2e2979", "#ababab")) +
  ylim(c(1450,1720)) +
  labs(title = "Calificación de Elo personal/equipo, 
Liga Profesional de Fútbol 2022, Argentina",
       x = "Fecha",
       y = "Calificación de Elo",
       colour = "Team")

####  RIVER PLATE BLOCK!!!
ggplot(rtop)+
  geom_bar(mapping = aes(x = reorder(player,PlayerRateAfter),
                         y = PlayerRateAfter,
                         fill = PlayerRateAfter > last_team_average,
                         color = PlayerRateAfter > last_team_average
  ), stat = "identity",
  position = "stack") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_manual(values=c("#ffffff", '#eb192e')) +
  scale_color_manual(values=c("#000000", '#eb192e')) +
  coord_cartesian(ylim = c(1425, 1680)) +
  geom_hline(yintercept = 1621, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = 1621, col = '#133b5b', size = .4) +
  annotate("text", x=4.35, y=1625 + 6, label= "Boca Juniors (1 lugar)", col = '#133b5b') +
  geom_hline(yintercept = last_team_average, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = last_team_average, col = '#000000', size = .4) +
  annotate("text", x=5.05, y=last_team_average + 8, label= "Última calificación del equipo", col = '#000000') +
  geom_hline(yintercept = 1500, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = 1500, col = '#6f6f6f', size = .4) +
  annotate("text", x=4.17, y=1505 + 6, label= "Promedio de la Liga", col = '#6f6f6f') +
  annotation_raster(img_boca, ymin = 1625,ymax= 1638,xmin = 1.3,xmax = 1.85) +
  annotation_raster(img_river, ymin = last_team_average + 3,ymax= last_team_average + 16,xmin = 1.3,xmax = 1.85) +
  annotation_raster(img_lpfa, ymin = 1505,ymax= 1520,xmin = 1.3,xmax = 1.85) +
  labs(title = "Calificación Elo de los jugadores de CA River Plate",
       y = "Calificación",
       family = "special"
  ) +
  theme(legend.position='none',
        axis.title.x=element_blank())

### Personal graph
ggplot(data = player_rate, aes(x = game_date)) +
  geom_line(aes(y = PlayerRateAfter,
                colour = player_name), size = 1) +
  geom_line(aes(y = TeamRateAfter,
                colour = "River Plate"), size = 1) +
  stat_difference(aes(ymin = TeamRateAfter, ymax = PlayerRateAfter),
                  alpha = 0.4,
                  show.legend = F) +
  scale_color_manual(name='Jugador/Equipo',
                     breaks=c(player_name, 'River Plate'),
                     values=c("Enzo Pérez"= "#eb192e", 'River Plate'='#000000')) +
  scale_fill_manual(values = c("#eb192e", "#000000", "#ababab")) +
  ylim(c(1450,1720)) +
  labs(title = "Calificación de Elo personal/equipo, 
Liga Profesional de Fútbol 2022, Argentina",
       x = "Fecha",
       y = "Calificación de Elo",
       colour = "Team")

####  CA Huracán BLOCK!!!
ggplot(rtop)+
  geom_bar(mapping = aes(x = reorder(player,PlayerRateAfter),
                         y = PlayerRateAfter,
                         fill = PlayerRateAfter > last_team_average,
                         color = PlayerRateAfter > last_team_average
  ), stat = "identity",
  position = "stack") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_manual(values=c("#ffffff", '#ff0000')) +
  scale_color_manual(values=c("#111111", '#ff0000')) +
  coord_cartesian(ylim = c(1425, 1680)) +
  geom_hline(yintercept = 1621, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = 1621, col = '#133b5b', size = .4) +
  annotate("text", x=4.35, y=1625 + 6, label= "Boca Juniors (1 lugar)", col = '#133b5b') +
  geom_hline(yintercept = last_team_average, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = last_team_average, col = '#ff0000', size = .4) +
  annotate("text", x=5.05, y=last_team_average + 8, label= "Última calificación del equipo", col = '#111111') +
  geom_hline(yintercept = 1500, col = '#ffffff', size = 1.7) +
  geom_hline(yintercept = 1500, col = '#6f6f6f', size = .4) +
  annotate("text", x=4.17, y=1505 + 6, label= "Promedio de la Liga", col = '#6f6f6f') +
  annotation_raster(img_boca, ymin = 1625,ymax= 1638,xmin = 1.3,xmax = 1.85) +
  annotation_raster(img_huracan, ymin = last_team_average + 3,ymax= last_team_average + 16,xmin = 1.3,xmax = 1.81) +
  annotation_raster(img_lpfa, ymin = 1505,ymax= 1520,xmin = 1.3,xmax = 1.85) +
  labs(title = "Calificación Elo de los jugadores de CA Huracán",
       y = "Calificación",
       family = "special"
  ) +
  theme(legend.position='none',
        axis.title.x=element_blank())
### Personal graph CA Huracán
ggplot(data = player_rate, aes(x = game_date)) +
  geom_line(aes(y = PlayerRateAfter,
                colour = player_name), size = 1) +
  geom_line(aes(y = TeamRateAfter,
                colour = "CA Huracán"), size = 1) +
  stat_difference(aes(ymin = TeamRateAfter, ymax = PlayerRateAfter),
                  alpha = 0.4,
                  show.legend = F) +
  scale_color_manual(name='Jugador/Equipo',
                     breaks=c(player_name, 'CA Huracán'),
                     values=c("Luca Orellano"= "#ff0000", 'CA Huracán'='#111111')) +
  scale_fill_manual(values = c("#ff0000", "#111111", "#ababab")) +
  ylim(c(1420,1700)) +
  labs(title = "Calificación de Elo personal/equipo, 
Liga Profesional de Fútbol 2022, Argentina",
       x = "Fecha",
       y = "Calificación de Elo",
       colour = "Team") +
  theme(legend.position = c(0.85, 0.12))


