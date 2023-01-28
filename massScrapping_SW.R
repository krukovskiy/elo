library(XML)
library("rvest")
library(tidyr)
library(lubridate)
library(dplyr)
library(polite)

## Initial
competition_name = "Liga Profesional De Fútbol Argentina"
URL = readRDS("data/urls_1_22_sw.RData")
u2 = readRDS("data/urls_f_22_sw.RData")
URL = append(URL,u2)

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
saveRDS(big_data, "data/LPFA22.RDS")
#qq = readRDS("data/ArgU20.RDS")
