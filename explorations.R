library(rvest)
library(dplyr)
library(ggplot2)
library(ISLR)
library(caret)
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


# team rate test

sum(current_game$PlayerRateBefore * (current_game$min_rc/game_min))/ sum(current_game$min_rc/game_min)
sum(current_game$PlayerRateAfter * (current_game$min/game_min))/ sum(current_game$min/game_min)


po = diff*11
total_min = 11*game_min
min_cost = po/total_min


rcb = 0
rcb_index = which(current_game$rc == "1")  
rcb = sum(abs(diff * (game_min - current_game[rcb_index,]$min)/game_min))
rest_min = game_min * 10
min_bonus = rcb/rest_min
  
current_game = current_game %>% 
  mutate(PlayerRateAfter=NA) %>%
  mutate(PlayerRateAfter=ifelse(rc==1, current_game$PlayerRateBefore + min_cost * current_game$min - rcb,PlayerRateAfter)) %>%
  mutate(PlayerRateAfter=ifelse(rc==0, current_game$PlayerRateBefore + min_cost * current_game$min + min_bonus*current_game$min*2,PlayerRateAfter)) 


## find young players

qq = merge(all_last_p_rates, big_pl_data, by.x  = "link", by.y = "shortURL")
qq2 = qq %>% filter(`Fecha de nacimiento` > "2000-03-19")
qq3 = qq2 %>% filter(game_date > "2023-03-01")



#### Logic regression

# read calc_data

calc_data = readRDS("data/calc_data22032023.RData")
#calc_data = na.omit(calc_data)
calc_data$result[calc_data$result == "l"] <- 0
calc_data$result[calc_data$result == "d"] <- .5
calc_data$result[calc_data$result == "w"] <- 1
calc_data$result = as.numeric(calc_data$result)

calc_data$diff = calc_data$RivalRateBefore - calc_data$TeamRateBefore
ggplot(calc_data, aes(x=diff, y=result)) + 
  geom_jitter(width = 0, height = 0.05) +
  geom_smooth(method="glm",  method.args = list(family="binomial"))  + 
  labs(x = "Team Elo difference", y = "Win probability") +
  facet_wrap(~ team) 


# Team plots
team_data = calc_data[,c("game_date", "team", "TeamRateBefore", "RivalRateBefore", "result","diff")]
team_data = team_data[!duplicated(team_data), ]
team_data = team_data %>% filter(team_data$team == "Godoy Cruz")

y = team_data$result
xDiff = team_data$diff
xTeam = as.factor(team_data$team)
xLink = as.factor(team_data$link)
logR = glm(formula = y ~ xDiff, family = binomial(link = "logit"))
predict(logR, data.frame(xDiff=0), type = "response")
logR_probs = data.frame(probs = predict(logR, type="response"))
head(logR_probs)

qplot(diff, result,  data = team_data, alpha = .02)
## Team plots


ggplot(team_data, aes(x=diff, y=result)) + 
  geom_jitter(width = 0, height = 0.05) +
  geom_smooth(method="glm",  method.args = list(family="binomial"))  + 
  labs(x = "Diferencia de Elo rating respecto al rival", y = "Probabilidad de ganar") +
  facet_wrap(~ team) 

## Personal plots
p_data = calc_data %>% filter(calc_data$player == "A. Soto" & min > 0)
qplot(diff, result,  data = p_data, alpha = .02)

inTrain <- createDataPartition(y=calc_data$result, p=.7, list = FALSE)
trainig <- calc_data[inTrain,]
testing <- calc_data[-inTrain,]

fit.control <- trainControl(method = "repeatedcv", number = 5, repeats = 10)

set.seed(123)  
fit <- train(result ~ diff, data = trainig, method = "glm", 
             family = "binomial", trControl = fit.control)
fit$finalModel
print(fit)

pred = predict(fit, testing)
qplot(result, pred, data = testing)




y = p_data$result


xDiff = p_data$diff
xMin = p_data$min
xTeam = as.factor(p_data$team)
xLink = as.factor(p_data$link)
logPer = glm(formula = y ~ xDiff + xLink, family = binomial(link = "logit"))
summary(logPer)
library(effects)
all.effects <- allEffects(mod = logPer)
summary(all.effects)
plot(all.effects, type = "response", ylim = c(0, 1))

ggplot(p_data, aes(x=diff, y=result)) + 
  geom_jitter(width = 0, height = 0.05) +
  geom_smooth(method="glm",  method.args = list(family="binomial"))  + 
  labs(x = "Age", y = "P(Survival)") +
  facet_wrap(~ team) 


a = p_data$result
b = p_data$diff
logPer = glm(formula = a ~ b, family = "binomial")



realDiff = data.frame(a = 50)
pr = predict(logPer, newdata = data.frame(b = -100))

ggplot(data=p_data, aes(logPer$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")



library(dplyr)

# Создаем новый датафрейм с пустыми строками
out_data_new <- out_data %>%
  mutate(
    logo_home = ifelse(row_number() %% 2 == 0, NA, logo_home),
    elo_home = ifelse(row_number() %% 2 == 0, NA, elo_home),
    dif_home = ifelse(row_number() %% 2 == 0, NA, dif_home),
    team_home = ifelse(row_number() %% 2 == 0, NA, team_home),
    points_home = ifelse(row_number() %% 2 == 0, NA, points_home),
    tire = ifelse(row_number() %% 2 == 0, NA, tire),
    points_away = ifelse(row_number() %% 2 == 0, NA, points_away),
    team_away = ifelse(row_number() %% 2 == 0, NA, team_away),
    elo_away = ifelse(row_number() %% 2 == 0, NA, elo_away),
    dif_away = ifelse(row_number() %% 2 == 0, NA, dif_away),
    logo_away = ifelse(row_number() %% 2 == 0, NA, logo_away)
  )
