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
library(tidygeocoder)


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
# cv = Column values
cv = page %>% read_html() %>% html_nodes("dd") %>% html_text2()  

# curprofile = Current profile
curprofile <- as.data.frame(rbind(cv))
colnames(curprofile) <- cn
 
# Merge
if (length(cn) > 0)
{
  pprofile = suppressMessages(full_join(pprofile, curprofile))
  
  # Convert date of birthday
  clock_labels_lookup("es")
  pprofile$`Fecha de nacimiento` = date_parse(
    x = pprofile$`Fecha de nacimiento`,
    format = "%d %B %Y",
    locale = clock_locale(labels = "es")
  )
  
  pprofile = cbind(pprofile, shortURL)
  pprofile = pprofile[2,]
  
  
  pprofile
}

}
####### END OF THE FUNCTION #########


# Get players that arent in the list
beforeProfiles <- readRDS("data/PlayersProfiles_31102023.RDS")
all_last_p_rates <- readRDS("data/lastSW26092023.RData")
newPlayers=subset(all_last_p_rates, !(link %in% beforeProfiles$shortURL))
URL2=newPlayers$link

## Mass scrapping
# Call these variables cause we use them furthermore
cur_pl <- c()
pl_frames <- c()

# Initialize a progress bar
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = length(URL2),
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)      # Width of the progress bar

i=1
while(i <= length(URL2)) {
  pb$tick() # progress bar
  cur_pl  <- getPlayerProfile(URL2[i])
  pl_frames[[i]] <- cur_pl
  i <- i + 1
  Sys.sleep(1)
}
big_pl_data = do.call(rbind,  pl_frames)

##
addr = paste0(big_pl_data$`País de nacimiento`, ", ",big_pl_data$`Lugar de nacimiento`)
coord = geo(addr, method = "arcgis")
big_pl_data = cbind(big_pl_data, coord)

## Merge with
pprofiles <- beforeProfiles
big_pl_data = rbind(pprofiles,big_pl_data)

#saveRDS(big_pl_data, "data/PlayersProfiles_02122023.RDS")
