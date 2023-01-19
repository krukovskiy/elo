library(RCurl)
library(XML)
library("rvest")
library(tidyr)
library(lubridate)
library(dplyr)
library(stringr)

# Load last rating data
all_data = read.csv(file = "all.csv")
#set_config(use_proxy(url="10.3.100.207",port=8080))

options(scipen=999)

# Initial players frame
players_profiles = data.frame(player_name = "first", pval = "100", currency = "euro", pdat = as.POSIXct("01/01/1970",format="%d/%m/%Y"))
players_profiles = players_profiles[0,]

# loop of players
for (k in 81:nrow(all_data))
{
  # Init
  links = NULL
  # Player name
  player_name = all_data$player[k]
  # Url of the search url
  URL = paste("https://www.transfermarkt.com.ar/schnellsuche/ergebnis/schnellsuche?query=",player_name, sep = "")
  
# Search request
r <- GET(URLencode(URL))

page = read_html(r)
player_data_html = page %>%  html_nodes(".hauptlink a") 
player_data_html[1] %>% html_attr('href') 
for(i in 1:length(player_data_html)){
  
  links[i] <- player_data_html[i] %>% html_attr("href")
}


for (j in 1:length(links))
{
# Now we open link of player stats
URL2 = paste("https://www.transfermarkt.com.ar",links[j], sep = "")

r2 =  GET(URLencode(URL2))
page2 = read_html(r2)

# Get player market value
pval = page2 %>%  html_nodes(".data-header__market-value-wrapper") %>% html_text()
val = page2 %>%  html_nodes(".data-header__market-value-wrapper") %>% html_text()
pval <- gsub(" mil €.*", "000", pval)
pval <- gsub(" mill. €.*", "0000", pval)
pval <- gsub(",", "", pval)
# Convert balue to a numeric
pval = as.numeric(pval)

if (length(pval)!=0)
{
# Get date
pdat = str_extract(val,regex("[0-9]{2}/[0-9]{2}/[0-9]{4}"))
pdat = as.POSIXct(pdat,format="%d/%m/%Y")
cur_player = data.frame(player_name, pval, currency = "euro", pdat)

# Add new value
players_profiles[nrow(players_profiles) + 1,] = cur_player
}
}
}

qpl = players_profiles %>% filter(as.numeric(pval) > 1000000) %>% arrange(player_name)
qpl2 = qpl[-c(13,15,16,88,123,138,139,140,203,175),]


pp = merge(qpl2,all_data, by.x = "player_name", by.y = "player")
pp$pval = as.numeric(pp$pval)
pp= pp %>% arrange(desc(pval))

pp = pp[-c(1,2,3),]
write.csv(pp, "pp.csv")
