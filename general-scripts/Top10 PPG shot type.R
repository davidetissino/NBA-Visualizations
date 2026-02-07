### PROJECT 9: PPG LEADERS AND TYPE OF SHOT TAKEN ####

library(ggthemes)
library(tidyverse)
library(extrafont)
library(janitor)
library(tictoc)
library(ggimage)
library(jsonlite)
library(httr)
library(dplyr)
library(waffle)
library(RColorBrewer)

# custom theme
theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='avenir'), 
      axis.title.x = element_text(face='bold.italic', size=9, margin=margin(t=10)),
      axis.title.y = element_text(face='italic', size=12, margin=margin(r=10), angle = 90),
      axis.text.x = element_text(face='plain'),
      axis.text.y = element_text(face='bold.italic'), 
      panel.background = element_rect('floralwhite'), 
      plot.background = element_rect('floralwhite'),
      plot.title = element_text(face='bold', size=15, hjust = 0.5),
      plot.subtitle=element_text(face='italic', size=10, hjust = 0.5, vjust = -1), 
      panel.grid.major = element_line(color='gray90', linetype = 'dashed'),
      plot.margin = margin(.5, .5, .25, .5, "cm")
    ) 
}

# who knows why but works 
Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 2)

# team colors and infos 
tms <- read_csv('/Users/davidetissino/Desktop/R/data/teams.csv')

# headers to web scrape from NBA website 
headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/shooting/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)


####### BY ZONE FOR FGA ###### 

####### General stats data ==============

# url with general stats 
url1 <- 'https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

res <- GET(url=url1, add_headers(.headers=headers))

json_resp <- fromJSON(content(res,'text'))

general <- data.frame(json_resp$resultSets$rowSet)

colnames(general) <- json_resp[["resultSets"]][["headers"]][[1]]

# removes columns keeping only player and PTS
general <- general[,-c(1,3:30,32:66)]

# convert PPG column to numeric 
general$PTS <- as.numeric(as.character(general$PTS))

# sort in descending order for PTS column 
ppg_leaders <- general[order(general$PTS, decreasing = TRUE), ]

# extracts only Top10 PPG
ppg_leaders <- ppg_leaders[-c(16:493),]

colnames(ppg_leaders)[1] <- 'player'


####### Shooting location data ==========

# site with information on shot type 
url <- 'https://stats.nba.com/stats/leaguedashplayershotlocations?College=&Conference=&Country=&DateFrom=&DateTo=&DistanceRange=By%20Zone&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

# obtain data and convert to df
res <- GET(url=url, add_headers(.headers=headers))

json_resp <- fromJSON(content(res,'text'))

shot_location <- data.frame(json_resp$resultSets$rowSet)

colnames(shot_location) <- json_resp[["resultSets"]][["headers"]][["columnNames"]][[2]]

# remove unnecessary columns (left vs right corner three, backcourt, FG%)
shot_location <- shot_location[,-c(1, 3:6, 9, 12, 15:21, 24:27, 30)]

# remove FGM columns, only interested in FGA
shot_location <- shot_location[,-c(2, 4, 6, 8, 10)]

# manually modify header titles to reflect info on shooting zone 
colnames(shot_location)[2] <- 'Restricted_Area'
colnames(shot_location)[3] <- 'Paint'
colnames(shot_location)[4] <- 'MidRange'
colnames(shot_location)[5] <- 'AboveBreak'
colnames(shot_location)[6] <- 'Corner'

colnames(shot_location)[1] <- 'player'

# keep only values for Top15 PPG 
shot_location <- shot_location %>%
  filter(player %in% ppg_leaders$player)a

# merge two dataframes into one and sort in descending PTS order
final <- merge(shot_location, ppg_leaders, by = 'player') 

final <- final[order(final$PTS, decreasing = T), ]

# convert all columns except player to numeric 
final <- final %>% mutate_at(2:6, as.numeric)

# create additional column with total of FGAs for each player
final <- final %>%
  mutate(totFGA = final$Restricted_Area + final$Paint + final$MidRange + 
           final$Corner + final$AboveBreak)

# pivot to longer format 
final <- final %>%
  pivot_longer(-c(player, totFGA, PTS))

# rename columns 
colnames(final)[4] <- 'zone'
colnames(final)[5] <- 'attempts'

# custom column with player name and tot FGAs in parentheses
final <- final %>%
  mutate(playerTot = paste0(player, ' (', totFGA, ')'))


## plot ## 

final %>%
  ggplot(aes(fill = zone, values = attempts)) + 
  # color, size = line between squares, make_prop makes square of width = height
  geom_waffle(color = 'grey20', size = .2, n_rows = 10, flip = T, 
              make_proportional = T, radius = unit(1.5, 'pt')) + 
  # display facets for each player, in custom order according to PPG
  facet_wrap(~fct_reorder(playerTot, -PTS), nrow = 3, strip.position = 'bottom', labeller = label_wrap_gen(1)) +
  theme_davide() + 
  coord_equal(clip = 'off') +
  theme(
    legend.position = 'top',
    legend.text = element_text(face = 'bold', size = 8.5),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = 'black', size = .3),
    panel.spacing.x = unit(2, 'lines'),
    strip.background = element_rect('floralwhite'),
    strip.text = element_text(face = 'bold', size = 8),
    plot.subtitle = element_text(margin = margin(b=12)), 
    legend.background = element_rect('floralwhite'), 
    legend.title = element_blank(), 
    plot.caption = element_text(hjust = c(1, 0))
  ) + 
  labs(
    title = 'Shooting Habits By Zone For Points Per Game Leaders', 
    subtitle = 'Each square represents 1% of total FGAs (in parentheses) taken by each player in the Top15 for PPG',
    caption = c('@dvdtssn | stats.nba.com', paste0('Updated ', format.Date(Sys.Date(), '%B %d, %Y')))
  ) +
  scale_fill_manual(values = c('#7EC0EE', '#458B74', '#e6f599', '#ff8247', '#d53e4f'),
                    labels = c('Above the Break 3', 'Corner 3', 'Mid Range', 'Paint (non-RA)', 'Restricted Area'))



# ggsave('/Users/davidetissino/Desktop/FGA by zone for Top15 PPG.png', dpi = 'retina')



####### BY ZONE FOR FGM ######

####### General stats data ==============

# url with general stats 
url1 <- 'https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

res <- GET(url=url1, add_headers(.headers=headers))

json_resp <- fromJSON(content(res,'text'))

general <- data.frame(json_resp$resultSets$rowSet)

colnames(general) <- json_resp[["resultSets"]][["headers"]][[1]]

# removes columns keeping only player and PTS
general <- general[,-c(1,3:30,32:66)]

# convert PPG column to numeric 
general$PTS <- as.numeric(as.character(general$PTS))

# sort in descending order for PTS column 
ppg_leaders <- general[order(general$PTS, decreasing = TRUE), ]

# extracts only Top10 PPG
ppg_leaders <- ppg_leaders[-c(16:493),]

colnames(ppg_leaders)[1] <- 'player'


####### Shooting location data ==========

# site with information on shot type 
url <- 'https://stats.nba.com/stats/leaguedashplayershotlocations?College=&Conference=&Country=&DateFrom=&DateTo=&DistanceRange=By%20Zone&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

# obtain data and convert to df
res <- GET(url=url, add_headers(.headers=headers))

json_resp <- fromJSON(content(res,'text'))

shot_location <- data.frame(json_resp$resultSets$rowSet)

colnames(shot_location) <- json_resp[["resultSets"]][["headers"]][["columnNames"]][[2]]

# remove unnecessary columns (left vs right corner three, backcourt, FG%)
shot_location <- shot_location[,-c(1, 3:6, 9, 12, 15:21, 24:27, 30)]

# remove FGM columns, only interested in FGA
shot_location <- shot_location[,-c(3, 5, 7, 9, 11)]

# manually modify header titles to reflect info on shooting zone 
colnames(shot_location)[2] <- 'Restricted_Area'
colnames(shot_location)[3] <- 'Paint'
colnames(shot_location)[4] <- 'MidRange'
colnames(shot_location)[5] <- 'AboveBreak'
colnames(shot_location)[6] <- 'Corner'

colnames(shot_location)[1] <- 'player'

# keep only values for Top10 PPG 
shot_location <- shot_location %>%
  filter(player %in% ppg_leaders$player)

# merge two dataframes into one and sort in descending PTS order
final <- merge(shot_location, ppg_leaders, by = 'player') 

final <- final[order(final$PTS, decreasing = T), ]

# convert all columns except player to numeric 
final <- final %>% mutate_at(2:6, as.numeric)

# create additional column with total of FGAs for each player
final <- final %>%
  mutate(totFGM = final$Restricted_Area + final$Paint + final$MidRange + 
           final$Corner + final$AboveBreak)

# pivot to longer format 
final <- final %>%
  pivot_longer(-c(player, totFGM, PTS))

# rename columns 
colnames(final)[4] <- 'zone'
colnames(final)[5] <- 'attempts'

# custom column with player name and tot FGAs in parentheses
final <- final %>%
  mutate(playerTot = paste0(player, ' (', totFGM, ')'))


## plot ## 

final %>%
  ggplot(aes(fill = zone, values = attempts)) + 
  # color, size = line between squares, make_prop makes square of width = height
  geom_waffle(color = 'grey20', size = .2, n_rows = 10, flip = T, 
              make_proportional = T, radius = unit(1.5, 'pt')) + 
  # display facets for each player, in custom order according to PPG
  facet_wrap(~fct_reorder(playerTot, -PTS), nrow = 3, strip.position = 'bottom', labeller = label_wrap_gen(1)) +
  theme_davide() + 
  coord_equal(clip = 'off') +
  theme(
    legend.position = 'top',
    legend.text = element_text(face = 'bold', size = 8.5),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = 'black', size = .3),
    panel.spacing.x = unit(2, 'lines'),
    strip.background = element_rect('floralwhite'),
    strip.text = element_text(face = 'bold', size = 8),
    plot.subtitle = element_text(margin = margin(b=12)), 
    legend.background = element_rect('floralwhite'), 
    legend.title = element_blank(), 
    plot.caption = element_text(hjust = c(1, 0))
  ) + 
  labs(
    title = 'Scoring Habits By Zone For Points Per Game Leaders', 
    subtitle = 'Each square represents 1% of total FGMs (in parentheses) taken by each player in the Top15 for PPG',
    caption = c('@dvdtssn | stats.nba.com', paste0('Updated ', format.Date(Sys.Date(), '%B %d, %Y')))
  ) +
  scale_fill_manual(values = c('#7EC0EE', '#458B74', '#e6f599', '#ff8247', '#d53e4f'),
                    labels = c('Above the Break 3', 'Corner 3', 'Mid Range', 'Paint (non-RA)', 'Restricted Area'))


# ggsave('/Users/davidetissino/Desktop/FGM by zone for Top15 PPG.png', dpi = 'retina')





