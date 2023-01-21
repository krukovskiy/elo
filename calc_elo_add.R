library(dplyr)
library(tibble)
library(PlayerRatings)
library(tidyr)
library(reactable)
library(htmltools)

# Select a game
## Load data with older calculations
all_data = read.csv(file = "all.csv")
all_data = all_data %>% as.data.frame()
all_data$game_date = as.POSIXct(all_data$game_date,format="%Y-%m-%d")
#### ADD RATES AFTER A GAME

# select how long was the game
game_min = 104
## Team #1 Input data manually
current_date = "2023-01-20"
team = "Racing"
rival = "Boca Juniors"
result1 = "w"
after_rates1 = all_data[0,]
players_after = c("Gabriel Arias",
                  "Gonzalo Piovi",
                  "Emiliano Insúa",
                  "Leonardo Sigali",
                  "Iván Pillud",
                  "Juan Nardoni",
                  "Aníbal Moreno",
                  "Johan Carbonero",
                  "Maximiliano Moralez",
                  "Nicolás Oróz",
                  "Maximiliano Romero",
                  "Gabriel Hauche",
                  "Jonathan Galván",
                  "Nicolás Reniero",
                  "Jonathan David Gómez",
                  "Maico Quiroz")
mins_after = c("104", "104", "104", "104", "104", "73", "100", "100", "104", "73", "63", "41", "4", "51", "41", "4")

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
}

## Team #2 Input data manually
current_date = "2023-01-20"
team2 = "Boca Juniors"
rival2 = "Racing"
result2 = "l"
after_rates2 = all_data[0,]
players_after2 = c("Javier Hernán García",
                   "Luis Advíncula",
                   "Facundo Roncaglia",
                   "Agustín Sández",
                   "Frank Fabra",
                   "Guillermo Matías Fernández",
                   "Alan Varela",
                   "Juan Ramírez",
                   "Norberto Briasco",
                   "Darío Benedetto",
                   "Sebastián Villa",
                   "Ignacio Fernández",
                   "Luis Vázquez",
                   "Luca Langoni")
mins_after2 = c("104","104","104","104","104","104","104", "74", "46", "74", "104","58", "40", "40")

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
}

### Aggregate all
games = rbind(after_rates1, after_rates2)

## Create empty dataframe with same columns to make furthermore calculated table
calc_data <- games[0,]

## Define all game pairs (team * date)
game_pairs = unique(games[c("game_date", "team")])

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
  current_rival = current_game$rival[1]
  
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
    current_game$PlayerRateBefore + ((current_game$TeamRateAfter - current_game$TeamRateBefore)*current_game$min) / 90
  
  
  calc_data = rbind(calc_data, current_game)
  saveRDS(calc_data, "calc_data.RData")
}

## Add to history
hist_data = readRDS("arg_season_2022_rating")
hist_data = rbind(hist_data, calc_data)
saveRDS(hist_data, "arg_season_2022_23_rating")

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



calc_data <- readRDS("calc_data.RData")

# create special dataframe
ctd = calc_data[,c("team", "player", "game_date" , "min","PlayerRateAfter")]
ctd$diff = calc_data$PlayerRateAfter - calc_data$PlayerRateBefore
ctd$photo = ""
ctd$PlayerRateAfter = round(ctd$PlayerRateAfter,1)
ctd$diff = round(ctd$diff,1)
ctd = ctd %>% relocate(photo, .after = team)
ctd = ctd %>% arrange(desc(PlayerRateAfter))
ctd$photo = c("varela", "moreno", "insua", "galvan", "advincula", "arias", "romero", "sigali", "hauche", "fernandez", "carbonero", "pillud", "villa", "piovi", "vazquez", "oroz", "ifernandez", "fabra", "gomez", "benedetto", "langoni", "moralez", "reniero", "quiroz", "brasco", "ramirez", "roncaglia", "nardoni", "sandez", "garcia")
#                                       +1 because of hours difference
ctd$game_date = as.Date(ctd$game_date) +1
ctd$game_date = format(as.Date(ctd$game_date), "%d-%m")

## Drawing table

BuYlRd <- function(x) rgb(colorRamp(c("#7fb7d7", "#ffffbf", "#fc8d59"))(x), maxColorValue = 255)
dimnames <- list(start(nottem)[1]:end(nottem)[1], month.abb)
reactable(
  ctd[1:10,],
  columns = list(
    team = colDef(name = "Equipo",
      cell = function(value) {
        img_src <- knitr::image_uri(sprintf("logo/%s.png", value))
        image <- img(src = img_src, style = "height: 34px;", alt = value)
        tagList(
          div(style = "display: inline-block; width: 15px", image)
          )
      },
      maxWidth = 45
    ),
    photo = colDef(name = "",
                   cell = function(value) { img_src <- knitr::image_uri(sprintf("players/%s.jpg", value))
                   image <- img(src = img_src, style = "height: 34px; width: 30px;", alt = value)
                   tagList(
                     div(style = "display: inline-block; width: 15px", image))
                   },
                   maxWidth = 45
                   ),
    player = colDef(name = "Jugador",
                    maxWidth = 100),
    game_date = colDef(name = "Fecha",
                       maxWidth = 65),
    min = colDef(name = "Min.",
                 format = colFormat(digits = 0),
                 maxWidth = 55),
    PlayerRateAfter = colDef(name = "Pers. Elo",
                             maxWidth = 75,
                             align = "center"),
                             
    diff = colDef(name = "+/-",
                  align = "center",
                  cell = function(value) {
                    if (value >= 0) paste0("+", value) else value
                   },
                  style = function(value) {
                    if (!is.numeric(value)) return()
                    normalized <- (value - min(ctd$diff)) / (max(ctd$diff) - min(ctd$diff))
                    color <- BuYlRd(normalized)
                    list(background = color)
                  },
                  format = colFormat(digits = 1),
                  maxWidth = 70)
                 
  ),
  defaultColDef = colDef(format = colFormat(digits = 1),
  minWidth = 50),
  defaultSorted = "PlayerRateAfter",
  defaultSortOrder = "desc",
  #rowStyle = function(index) {
   #if (ctd[index, "team"] == "Racing") 
    # list(background = "rgba(2, 156, 220, 0.1)")
     #else list(background = "rgba(251, 212, 85, 0.2)")
 # },
  style = list(fontFamily = "-apple-system, Segoe UI, Helvetica"
              )
)

.spi-rating {
  display: flex;
  align-items: center;
  justify-content: center;
  margin: auto;
  width: 1.875rem;
  height: 1.875rem;
  border: 1px solid rgba(0, 0, 0, 0.1);
  border-radius: 50%;
  color: #000;
    font-size: 0.8125rem;
  letter-spacing: -1px;
}