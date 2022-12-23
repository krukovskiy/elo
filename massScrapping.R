library(XML)
library("rvest")
library(tidyr)
library(lubridate)
library(dplyr)


URL = read.csv("football_links.csv")

# Add game logs to the one big frame
i <- 1

# Call these variables cause we use them furthermore
cur_frame <- c()
game_frames <- c()
while(i <= nrow(URL)) {
  cur_frame  <- getGameFrame(URL[i,2])
  game_frames[[i]] <- cur_frame
  i <- i + 1
}

big_data = do.call(rbind,  game_frames)
saveRDS(big_data, "all_games_2022")
qq = readRDS("all_games_2022")
