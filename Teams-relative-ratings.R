#### RELATIVE OFFENSIVE AND DEFENSIVE RATINGS CHANGES ####

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

# advanced stats for 2023 - 24 season
season_24 = 'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='

res <- GET(url = season_24, add_headers(.headers = headers))

json_resp <- fromJSON(content(res, 'text'))

df_24 <- data.frame(json_resp$resultSets$rowSet)

colnames(df_24) <- json_resp[['resultSets']][['headers']][[1]]

# advanced stats for 2022 - 23 season 
season_23 = 'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='

res <- GET(url = season_23, add_headers(.headers = headers))

json_resp <- fromJSON(content(res, 'text'))

df_23 <- data.frame(json_resp$resultSets$rowSet)

colnames(df_23) <- json_resp[['resultSets']][['headers']][[1]]

# change column name to merge with tms
colnames(df_23)[2] <- 'team'
colnames(df_24)[2] <- 'team'

# csv with team colors and logos 
tms <- read_csv('/Users/davidetissino/Desktop/R/data/team.csv')

# keep only info on offensive & defensive ratings
df_23 <- df_23[,-c(1,3:8, 10, 12:46)]
df_24 <- df_24[,-c(1,3:8, 10, 12:46)]

# merge all three dfs together 
final <-  df_23 %>% 
  left_join(df_24, by='team') %>% 
  left_join(tms, by='team')

# rename columns 
colnames(final)[2] <- 'OFF_23'
colnames(final)[3] <- 'DEF_23'
colnames(final)[4] <- 'OFF_24'
colnames(final)[5] <- 'DEF_24'

final <- final[,-c(6, 10)]

# plot 











