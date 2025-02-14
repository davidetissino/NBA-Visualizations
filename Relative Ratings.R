#### RELATIVE RATINGS #####

library(jsonlite)
library(httr)
library(ggthemes)
library(extrafont)
library(tidyverse)
library(readr)
library(ggplot2)
library(ggimage)
library(ggtext)

# Custom theme 
theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='avenir'), 
      axis.title.x = element_text(face='bold.italic', size=12, margin=margin(t=10)),
      axis.title.y = element_text(face='bold.italic', size=12, margin=margin(r=10), angle = 90),
      axis.text.x = element_text(face='italic'),
      axis.text.y = element_text(face='italic'), 
      panel.background = element_rect('floralwhite'), 
      plot.background = element_rect('floralwhite'),
      plot.title = element_text(face='bold', size=17, hjust = 0),
      plot.subtitle=element_text(face='italic', size=12, hjust = 0, vjust = -1), 
      panel.grid.major = element_line(color='gray90', linetype = 'dashed'),
      plot.margin = margin(.5, .5, .25, .5, "cm")
    ) 
}

# who knows why but works 
Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 10)

# csv with teams info 
tms <- read_csv('/Users/davidetissino/Desktop/R/data/teams.csv')

# headers Ryan Davis
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

# info for 2024
url24 <- 'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='

res24 <- GET(url=url24, add_headers(.headers=headers))

json_resp24 <- fromJSON(content(res24,'text'))

df24 <- data.frame(json_resp24$resultSets$rowSet)

colnames(df24) <- json_resp24[["resultSets"]][["headers"]][[1]]  

# info for 2023
url23 <- 'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='

res23 <- GET(url=url23, add_headers(.headers=headers))

json_resp23 <- fromJSON(content(res23,'text'))

df23 <- data.frame(json_resp23$resultSets$rowSet)

colnames(df23) <- json_resp23[["resultSets"]][["headers"]][[1]]  

# keep only relevant columns 
df23 <- df23[,c(2,9,11)]
df24 <- df24[,c(2,9,11)]

# merge into one 
df <- merge(df23, df24, by = 'TEAM_NAME')

df <- merge(df, tms, by = 'team')

df <- df[,-c(8, 12)]

colnames(df)[1] <- 'team'
colnames(df)[2] <- 'off_last'
colnames(df)[3] <- 'def_last'
colnames(df)[4] <- 'off_curr'
colnames(df)[5] <- 'def_curr'

# convert to numeric 
df <- df %>% mutate_at(2:5, as.numeric)

# create changes in columns 
df$off_change <- df$off_curr - df$off_last
df$def_change <- df$def_curr - df$def_last

### Relative to Last Season ====
# plot 
df %>% 
  ggplot() + 
  #geom_point(aes(x = off_curr, y = def_curr), shape = 21, size = 3) + 
  geom_segment(aes(x = off_last, y = def_last, xend = off_curr, yend = def_curr), 
               arrow = arrow(length = unit(.06, "inch")), size = .8, color = 'gray20') +
  geom_point(aes(x = off_last, y = def_last), shape = 21, size = 3, color = df$primary, fill = df$secondary) +
  facet_wrap(~ slugTeam, strip.position = 'bottom') +
  scale_x_continuous(breaks = c(108, 112, 116, 120, 124, 128), labels = c(108, 112, 116, 120, 124, 128)) + 
  scale_y_continuous(breaks = c(108, 112, 116, 120), labels = c(108, 112, 116, 120)) +
  theme_davide() +
  labs(
    x = 'Offensive Rating', 
    y = 'Defensive Rating', 
    title = 'Changes In Offensive & Defensive Ratings', 
    subtitle = 'Colored dots for 2022-23, arrow end for 2023-24', 
    caption = c('@dvdtssn | stats.nba.com',  paste0('Updated ', format.Date(Sys.Date(), '%B %d, %Y')))
  ) +
  theme_davide() + 
  theme(
    strip.background = element_rect('floralwhite'), 
    plot.subtitle = element_markdown(margin = margin(b=8), hjust = .47),
    plot.title = element_text(margin = margin(b=8), hjust = .47),
    strip.text = element_text(face = 'bold'),
    plot.caption = element_text(hjust = c(1, -.08)), 
    panel.spacing = unit(.5, 'lines'), 
    axis.text.x = element_text(face = 'italic'), 
    axis.text.y = element_text(face = 'italic')
  )


# ggsave('/Users/davidetissino/Desktop/miao.png', dpi = 'retina')



## Relative to League Average ====

# change o/u league averages
df$off_change_last <- df$off_last - off_avg_last
df$def_change_last <- df$def_last - def_avg_last
df$off_change_curr <- df$off_curr - off_avg_curr
df$def_change_curr <- df$def_curr - def_avg_curr


# plot
df %>% 
  ggplot() + 
  #geom_point(aes(x = off_curr, y = def_curr), shape = 21, size = 3) + 
  geom_segment(aes(x = off_change_last, y = def_change_last, xend = off_change_curr, yend = def_change_curr), 
               arrow = arrow(length = unit(.06, "inch")), size = .8, color = 'gray20') +
  geom_point(aes(x = off_change_last, y = def_change_last), shape = 21, size = 3, color = df$primary, fill = df$secondary) +
  facet_wrap(~ slugTeam, strip.position = 'bottom') +
  #scale_x_continuous(breaks = c(108, 112, 116, 120, 124, 128), labels = c(108, 112, 116, 120, 124, 128)) + 
  #scale_y_continuous(breaks = c(108, 112, 116, 120), labels = c(108, 112, 116, 120)) +
  theme_davide() +
  labs(
    x = 'Offensive Rating', 
    y = 'Defensive Rating', 
    title = 'Changes In Offensive & Defensive Ratings', 
    subtitle = 'Relative to League averages | Colored dots for 2022-23, arrow end for 2023-24', 
    caption = c('@dvdtssn | stats.nba.com',  paste0('Updated ', format.Date(Sys.Date(), '%B %d, %Y')))
  ) +
  theme_davide() + 
  theme(
    strip.background = element_rect('floralwhite'), 
    plot.subtitle = element_markdown(margin = margin(b=8), hjust = .47),
    plot.title = element_text(margin = margin(b=8), hjust = .47),
    strip.text = element_text(face = 'bold'),
    plot.caption = element_text(hjust = c(1, -.08)), 
    panel.spacing = unit(.5, 'lines'), 
    axis.text.x = element_text(face = 'italic'), 
    axis.text.y = element_text(face = 'italic')
  ) + 
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed') + 
  geom_hline(yintercept = 0, color = 'black', linetype = 'dashed')

# 
ggsave('/Users/davidetissino/Desktop/mao.png', dpi = 'retina')
