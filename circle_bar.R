## PROJECT 6: CIRCULAR BARPLOTS ####


library(ggthemes)
library(extrafont)
library(tidyverse)
library(ggplot2)
library(readr)
library(jsonlite)
library(httr)
library(fmsb)
library(dplyr)
library(RColorBrewer)

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

# who knows why but works 
Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 10)

# team colors and infos 
tms <- read_csv('/Users/davidetissino/Desktop/R/data/colors and logos.csv')

# headers Ryan Davis (?)
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

# scrape data
url <- 'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Defense&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='

res <- GET(url=url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res,'text'))
df <- data.frame(json_resp$resultSets$rowSet)
colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]  


### CIRCULAR BARPLOT ####

# df with made up info to create basic
tm <- data.frame(team=rep(c('ATL', 'BOS'), each=6),
                       stat=rep(c('DREB', 'STL', 'BLK', 'TOV','2ND','FB'), times=2),
                       values=c(20, 12, 7, 13, 21, 9, 25, 8, 11, 8, 13, 16))


# circular barplot for only two teams 
ggplot(tm, aes(x=stat, y = values, fill = team)) + 
  geom_bar(stat='identity', position = 'dodge') + 
  coord_polar()


## use df with all teams data 
# !! should remove also dreb to better visualize 
df <- df[,-c(1,3:10,16:30)]

# rename columns
colnames(df)[1] <- 'team'
colnames(df)[4] <- 'TOVs'
colnames(df)[5] <- 'chance2'
colnames(df)[6] <- 'FBs'

# change LA Clippers 
df['team'][df['team'] == 'LA Clippers'] <- 'Los Angeles Clippers'

# convert to df as thinks its matrix 
df <- as.data.frame(df)

# combine with tms 
df <- merge(df, tms, by = 'team')

# remove color columns 
df <- df[,-c(1, 7, 9:11)]

# divide in two dfs for West and East
west <- df[c(7,8,10,11,13:15,18,19,21,24:27,29),]
east <- df[c(1:6,9,12,16:17,20,22,23,28,30),]

# convert to numeric
west$STL <- as.numeric(west$STL)
west$BLK <- as.numeric(west$BLK)
west$TOVs <- as.numeric(west$TOVs)
west$chance2 <- as.numeric(west$chance2)
west$FBs <- as.numeric(west$FBs) 

east$STL <- as.numeric(east$STL)
east$BLK <- as.numeric(east$BLK)
east$TOVs <- as.numeric(east$TOVs)
east$chance2 <- as.numeric(east$chance2)
east$FBs <- as.numeric(east$FBs) 

# modify into longer format from wide format
fwest <- west %>%
  pivot_longer(!slugTeam, names_to = 'stat', values_to = 'value')

feast <- east %>%
  pivot_longer(!slugTeam, names_to = 'stat', values_to = 'value')


# final plot with facet wraps for each team
# western conference
ggplot(fwest, aes(x=stat, y=value, fill=slugTeam)) +
  geom_bar(stat='identity', position='dodge') +
  theme_davide() + 
  theme(legend.position = 'none') + 
  coord_polar() +
  facet_wrap(~slugTeam)

# eastern conference 
feast %>%
  ggplot(aes(x=stat, y=value, fill=slugTeam)) +
  geom_bar(stat='identity', position='dodge') +
  theme_davide() + 
  theme(legend.position = 'none', 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank()) +
  coord_polar() +
  facet_wrap(~slugTeam)





### RADAR CHART ####
library(fmsb)

# need to set max and min values to obtain radar chart 
atl <- data.frame(DREB = c(40, 0, 31.8, 20), 
                  STL = c(40, 0, 8.9, 11), 
                  BLK = c(40, 0, 4.8, 4), 
                  OPP_PTS_OFF_TOV = c(40, 0, 18.6, 14), 
                  OPP_PTS_2ND_CHANCE = c(40, 0, 14.6, 22), 
                  OPP_PTS_FB = c(40, 0, 17.2, 12))

radarchart(atl,
           axistype = 0, 
           pcol = 'chocolate')

