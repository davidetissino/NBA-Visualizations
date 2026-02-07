#### PASSING COMPARISON #####

library(jsonlite)
library(httr)
library(ggthemes)
library(extrafont)
library(tidyverse)
library(readr)
library(ggplot2)
library(ggimage)

# custom theme
theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='avenir'), 
      axis.title.x = element_text(face='italic', size=12, margin=margin(t=10)),
      axis.title.y = element_text(face='italic', size=12, margin=margin(r=10), angle = 90),
      axis.text.x = element_text(face='bold.italic'),
      axis.text.y = element_text(face='bold.italic'), 
      panel.background = element_rect('floralwhite'), 
      plot.background = element_rect('floralwhite'),
      plot.title = element_text(face='bold', size=17, hjust = 0.5),
      plot.subtitle=element_text(face='italic', size=12, hjust = 0.5, vjust = -1), 
      panel.grid.major = element_line(color='gray90', linetype = 'dashed'),
      plot.margin = margin(.5, .5, .25, .5, "cm")
    ) 
}

# csv with colors and logos 
tms <- read_csv('/Users/davidetissino/Desktop/R/data/colors and logos.csv')

# headers
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

url <- 'https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Efficiency&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

res <- GET(url = url, add_headers(.headers = headers))
json_resp <- fromJSON(content(res,'text'))

eff <- data.frame(json_resp$resultSets$rowSet)

colnames(eff) <- json_resp[['resultSets']][['headers']][[1]]

# remove unecessary columns 
eff <- eff[,-c(17:21, 1, 4:8)]

colnames(eff)[1] <- 'slugTeam'
colnames(eff)[2] <- 'team'


# change name to merge with tms
eff['team'][eff['team'] == 'LA Clippers'] <- 'Los Angeles Clippers'

# merge tms and eff
final <- merge(eff, tms, by = 'slugTeam')

final <- final[,-c(2, 11:14)]

# convert to numeric columns 
final <- final %>% mutate_at(2:9, as.numeric)

# multiply times 100 
final$DRIVE_FG_PCT <- final$DRIVE_FG_PCT * 100
final$CATCH_SHOOT_FG_PCT <- final$CATCH_SHOOT_FG_PCT * 100
final$PULL_UP_FG_PCT <- final$PULL_UP_FG_PCT * 100
final$PAINT_TOUCH_FG_PCT <- final$PAINT_TOUCH_FG_PCT * 100

# df with only team and pts scored
points <- final[,c(1,2,4,6,8)]

percent <- final[,c(1,3,5,7,9)]

# convert both to longer format 
points <- points %>%
  pivot_longer(cols = c(2:5), names_to = 'zone', values_to = 'PTS')

percent <- percent %>%
  pivot_longer(cols=c(2:5), names_to = 'zone', values_to = 'eFG')

# merge into one
comp <- cbind(points, percent)

# remove _PTS from zone column, delete zone 2 and slug 2
comp$zone <- gsub('_PTS', '', as.character(comp$zone))

comp <- comp[,-c(4,5)]

comp <- merge(comp, tms, by = 'slugTeam')

comp <- comp[,-c(5:8)]


# plot 

comp %>%
  ggplot(aes(x = eFG, y = PTS)) + 
  geom_image(aes(image = logo)) + 
  facet_wrap(~zone) + 
  theme_davide()


ggsave('banana.png')




