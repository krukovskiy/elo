library(rvest)
library(dplyr)
library(ggplot2)
library(ISLR)
library(caret)
library(progress)
#Explorations

qq = calc_data %>% filter(game_date > "2023-02-23" & competition == "Liga Profesional Argentina" & min > 0)
qq$diff = qq$PlayerRateAfter - qq$PlayerRateBefore
qq2 = unique(qq[c("game_date", "team", "TeamRateAfter","diff")])
qq2 = qq2 %>% arrange(desc(TeamRateAfter)) %>% mutate(rank = row_number()) %>% relocate(rank, .before = game_date)

# Add team logo
qq2$logo = paste0(qq2$team, ".png")

BuYlRd <- function(x) rgb(colorRamp(c("#7fb7d7", "#ffffbf", "#fc8d59"))(x), maxColorValue = 255)
reactable(
  top10[1:5,],
  columns = list(
    team = colDef(name = "Equipo",
                  cell = function(value) {
                    img_src <- knitr::image_uri(sprintf("logo/%s.png", value))
                    image <- img(src = img_src, style = "height: 48px;", alt = value)
                    tagList(
                      div(style = "display: inline-block; width: 48px", image)
                    )
                  },
                  align = "center",
                  maxWidth = 64
    ),
    photo = colDef(name = "",
                   cell = function(value) { img_src <- knitr::image_uri(sprintf("players/31012023/%s.png", value))
                   image <- img(src = img_src, style = "height: 57px", alt = value)
                   tagList(
                     div(style = "display: inline-block; width: 57px", image))
                   },
                   maxWidth = 64
    ),
    
    diff = rating_column(
      name = "+ / -",
      cell = function(value) {
        if (value >= 0) paste0("+", value) else value
        scaled <- (value - min(ctd$diff)) / (max(ctd$diff) - min(ctd$diff))
        color <- off_rating_color(scaled)
        value <- format(round(value, 1), nsmall = 1)
        div(class = "spi-rating", style = list(background = color), value)
      }
    ),
    doge = colDef(name = "doge rank",
                  cell = function(value) {
                    img_src <- knitr::image_uri(sprintf("doge/%s.png", value))
                    image <- img(src = img_src, style = "height: 48px;", alt = value)
                    tagList(
                      div(style = "display: inline-block; width: 48px", image)
                    )
                  },
                  align = "center",
                  maxWidth = 64
    )
  ),
  defaultColDef = colDef(headerStyle = list(fontWeight=200), vAlign = "center"),
  defaultSorted = "PlayerRateAfter",
  defaultSortOrder = "desc",
  borderless = TRUE,
  height = 900,
  
  #
  style = list(fontFamily = "Karla, Helvetica Neue"
  ),
  
)


qq2 = all_last_p_rates %>% filter(game_date > "2023-01-20" & competition == "Liga Profesional Argentina" & min > 0) %>% arrange(desc(PlayerRateAfter))  
qq2 = qq2[1:50,c("game_date", "team", "player", "PlayerRateAfter")]
qq2$PlayerRateAfter = round(qq2$PlayerRateAfter, digits = 2 )
write.csv2(qq2, "top50_1702.csv")


event <- page %>%
  read_html() %>%
  html_nodes(".left .bookings img[src*='Y2C.png'], .left .bookings img[src*='RC.png']") 
name = html_nodes(event, xpath = "../../preceding-sibling::td[1]/a") %>% html_text2()
e = event %>% html_attr("src")
min = as.numeric(str_extract(html_nodes(event, xpath = "..") %>% html_text2(), "([0-9])+", group = NULL))
hrm = data.frame(name, e, min)
merged_df <- merge(t_home, hrm, by.x = "name", by.y = "name", all.x = TRUE)
merged_df$min[!is.na(merged_df$min)] <- merged_df$min[!is.na(merged_df$min)]
merged_df$rc <- ifelse(is.na(merged_df$e), NA, 1)
t_home = merged_df[,1:4]



aevent <- page %>%
  read_html() %>%
  html_nodes(".right .bookings img[src*='Y2C.png'], .right .bookings img[src*='RC.png']") 
aname = html_nodes(aevent, xpath = "../../preceding-sibling::td[1]/a") %>% html_text2()
ae = aevent %>% html_attr("src")
amin = as.numeric(str_extract(html_nodes(aevent, xpath = "..") %>% html_text2(), "([0-9])+", group = NULL))
ahrm = data.frame(aname, ae, amin)
ared_player_index  = which(t_away$name == ahrm$aname)
amerged_df <- merge(t_away, ahrm, by.x = "name", by.y = "aname", all.x = TRUE)
amerged_df$min[!is.na(amerged_df$amin)] <- amerged_df$amin[!is.na(amerged_df$amin)]
amerged_df$rc <- ifelse(is.na(amerged_df$ae), NA, 1)
t_away = amerged_df[,1:4]

## find young players

qq = merge(all_last_p_rates, big_pl_data, by.x  = "link", by.y = "shortURL")
qq2 = qq %>% filter(`Fecha de nacimiento` > "2000-03-19")
qq3 = qq2 %>% filter(game_date > "2023-03-01")








############# !!!!!!!!!!!!!!!! ########################
############# Logic regression ########################
############# !!!!!!!!!!!!!!!! ########################

# read calc_data

calc_data2 = readRDS("data/calc_data04112023.RData")
#calc_data = na.omit(calc_data)
calc_data2$result[calc_data2$result == "l"] <- 0
calc_data2$result[calc_data2$result == "d"] <- .5
calc_data2$result[calc_data2$result == "w"] <- 1
calc_data2$result[calc_data2$result == "p1"] <- 1
calc_data2$result[calc_data2$result == "p0"] <- 0
calc_data2$result = as.numeric(calc_data2$result)

mdata = unique(calc_data2[c("game_date", "team", "rival","TeamRateBefore", "RivalRateBefore", "result", "is_home")])
mdata$RatingRatio = mdata$TeamRateBefore / mdata$RivalRateBefore
mdata$RatingDiff = mdata$TeamRateBefore - mdata$RivalRateBefore
#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(mdata), replace=TRUE, prob=c(0.7,0.3))
train  <- mdata[sample, ]
test   <- mdata[!sample, ]

# Summarize the model
model <- glm(result ~ PlayerRateBefore + TeamRateBefore + RivalRateBefore + is_home,family=binomial(link='logit'),data=train)
summary(model)

# Make predictions
probabilities <- model %>% predict(test, type = "response")
predicted.classes <- ifelse(probabilities > 0.66, "1", ifelse(probabilities > 0.33, "0.5", "0"))
test$predicions = predicted.classes
# Model accuracy
mean(predicted.classes == test$result)

plot(probabilities, test$result)




afl <- aflodds[,c(2,3,4,7)]
train <- afl[afl$Week <= 80,]
test <- afl[afl$Week > 80,]
robj <- elo(train)
pvals <- as.numeric(predict(robj, test))
test$pc <- ifelse(pvals > 0.66, "1", ifelse(pvals > 0.33, "0.5", "0"))

test = na.omit(test)


## Mejor rendimiento de los últimos 3 meses
# Filter data by time
cur_calc = calc_data %>% filter(game_date > "2023-09-30")

# Create a data fram of unique players
players_unique = unique(cur_calc[c("link")])
players_unique$dif = ""
players_unique$dif = as.numeric(players_unique$dif)

pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                        total = nrow(players_unique),
                        complete = "=",   # Completion bar character
                        incomplete = "-", # Incomplete bar character
                        current = ">",    # Current bar character
                        clear = FALSE,    # If TRUE, clears the bar when finish
                        width = 100)      # Width of the progress bar

# Going through all of the players to calculate the difference
for (i in 1:nrow(players_unique)){
  pb$tick()
  player_calc = cur_calc %>% filter(link == players_unique[i,1]) %>% arrange(game_date)
  players_unique$dif[i] = tail(player_calc$PlayerRateAfter, n=1) - player_calc$PlayerRateAfter[1]
  
}

# top1
players_unique = players_unique %>% arrange(desc(dif))

pers_perf = cur_calc %>% filter(link == players_unique$link[1])
#pers_perf = pers_perf[-4,]
pers_perf$dif = pers_perf$PlayerRateAfter - pers_perf$PlayerRateBefore
pers_perf = pers_perf[,c("player", "game_date", "team", "rival", "result", "PlayerRateAfter", "dif" )]
cur_calc = cur_calc[!duplicated(cur_calc), ]
write.csv(pers_perf, "data/pers_perf.csv")
