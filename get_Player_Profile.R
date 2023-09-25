library(XML)
library("rvest")
library(tidyr)
library(lubridate)
library(dplyr)
library(httr)
library(parsedate)
library(data.table)
library(clock)
library(polite)


## shortURL can be taken from calc_data <- link
getPlayerProfile <- function(shortURL) {
# Create a link from 2 parts (second part is from calc_data)
#URL = "/players/gabriel-alejandro-gudino/401280/"
URL = paste("https://el.soccerway.com",shortURL, sep = "")
page <- GET(URL, add_headers('user-agent' = 'Web scrap for personal project ([[krukovskiy.ignat@gmail.com]])'))

# check status of the request
while(http_status(page)$category != "Success"){
  Sys.sleep(300)
}

# Create empty frame pprofile = Player´s Profile
pprofile = data.frame(Nombre = "",
                      Apellidos = "",
                      Nacionalidad = "",
                      `Fecha de nacimiento` = "",
                      Edad = "",
                      `País de nacimiento` = "",
                      `Lugar de nacimiento` = "",
                      Posición = "",
                      Altura = "",
                      Peso = "",
                      Pie = "")
# Name columns on target df
colnames(pprofile) <- c("Nombre", "Apellidos", "Nacionalidad", "Fecha de nacimiento", "Edad", "País de nacimiento",
                        "Lugar de nacimiento", "Posición", "Altura", "Peso", "Pie")

# Parse html page
# cn = Column names
cn = page %>% read_html() %>% html_nodes("dt") %>% html_text2()  
cn# cv = Column values
cv = page %>% read_html() %>% html_nodes("dd") %>% html_text2()  

# curprofile = Current profile
curprofile <- as.data.frame(rbind(cv))
colnames(curprofile) <- cn
 
# Merge
if (length(cn) > 0)
{
  pprofile = full_join(pprofile, curprofile)
  
  # Convert date of birthday
  clock_labels_lookup("es")
  pprofile$`Fecha de nacimiento` = date_parse(
    x = pprofile$`Fecha de nacimiento`,
    format = "%d %B %Y",
    locale = clock_locale(labels = "es")
  )
  
  pprofile = cbind(pprofile, shortURL)
  pprofile[2,]
}

}

# Get players that arent in the list
beforeProfiles <- readRDS("data/PlayersProfiles_20082023.RDS")
all_last_p_rates <- readRDS("data/lastSW20082023.RData")
newPlayers=subset(all_last_p_rates, !(link %in% beforeProfiles$shortURL))
URL2=newPlayers$link

## Mass scrapping
# Call these variables cause we use them furthermore
cur_pl <- c()
pl_frames <- c()
i=1
while(i <= length(URL2)) {
  cur_pl  <- getPlayerProfile(URL2[i])
  pl_frames[[i]] <- cur_pl
  i <- i + 1
  Sys.sleep(2)
}
big_pl_data = do.call(rbind,  pl_frames)

## Merge with
pprofiles <- beforeProfiles

big_pl_data = rbind(pprofiles,big_pl_data)

#saveRDS(big_pl_data, "data/PlayersProfiles_20082023.RDS")