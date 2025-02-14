#### BIGGEST CHANGES IN PPG ####

library(jsonlite)
library(httr)
library(ggthemes)
library(extrafont)
library(tidyverse)
library(readr)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(ggtext)

# custom theme
theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='avenir'), 
      axis.title.x = element_text(face='bold.italic', size=12, margin=margin(t=10)),
      axis.title.y = element_text(face='bold.italic', size=12, margin=margin(r=10), angle = 90),
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

# team colors and infos 
tms <- read_csv('/Users/davidetissino/Desktop/R/data/teams.csv')

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

# increase connection buffer
Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 2)


# data for 24 season ====
url <- 'https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

res <- GET(url = url, add_headers(.headers = headers))

json_resp <- fromJSON(content(res, 'text'))

df24 <- data.frame(json_resp$resultSets$rowSet)

colnames(df24) <- json_resp[['resultSets']][['headers']][[1]]


# data for 23 season ====
url2 <- 'https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2022-23&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

res <- GET(url = url2, add_headers(.headers = headers))

json_resp <- fromJSON(content(res, 'text'))

df23 <- data.frame(json_resp$resultSets$rowSet)

colnames(df23) <- json_resp[['resultSets']][['headers']][[1]]


# keep only relevant column 
df23 <- df23[,-c(1,3,4, 6, 8:13, 15:30, 32:66)]

df24 <- df24[,-c(1,3,4, 6, 8:13, 15:30, 32:66)]

# merge into one 
df <- merge(df23, df24, by = 'PLAYER_NAME')

# rename some columns 
colnames(df)[1] <- 'player'
colnames(df)[2] <- 'slugTeam23'
colnames(df)[3] <- 'GP23'
colnames(df)[4] <- 'EFF23'
colnames(df)[5] <- 'PTS23'
colnames(df)[6] <- 'slugTeam24'
colnames(df)[7] <- 'GP24'
colnames(df)[8] <- 'EFF24'
colnames(df)[9] <- 'PTS24'

# convert to numeric data 
df <- df %>% mutate_at(c(3:5, 7:9), as.numeric)

# in percentage 
df$EFF23 <- df$EFF23 * 100
df$EFF24 <- df$EFF24 * 100

# new column for change in PPG 
df$pts_change <- df$PTS24 - df$PTS23
df$eff_change <- df$EFF24 - df$EFF23

# new column for tot GPs
df$totGP <- df$GP23 + df$GP24


#### Leaders in PPG change (positive) ####

# keep only players having played > 70 games between 23 season and 24 partial season
df <- df %>% filter(totGP > 70 & PTS24 > 10)

# sort in descending order for biggest increases in PPG 
changes <- df[order(df$pts_change, decreasing = TRUE), ]

# keep only top 15 in ppg change
changes <- changes[-c(13:272), ]

changes$player <- as.character(changes$player)

# fixed order in descending PPG variation 
# fixes player according to decreasing pts_change
changes$player <- factor(changes$player, 
                         levels = changes$player[order(changes$pts_change, decreasing = TRUE)])

changes %>%
  ggplot() +
  geom_point(aes(x = EFF23, y = PTS23), color = 'darkred', fill = 'firebrick2', 
             alpha = 0.5, shape = 21, size = 3.5) +
  geom_point(aes(x = EFF24, y = PTS24), color = 'blue', fill = 'dodgerblue2',
             alpha = 0.5, shape = 21, size = 3.5) +
  facet_wrap(~player, nrow = 3, strip.position = 'bottom') +
  geom_segment(aes(x = EFF23, y = PTS23, xend = EFF24, yend = PTS24), 
               arrow = arrow(length = unit(.06, "inch")), size = .8, color = 'gray20') +
  theme_davide() + 
  theme(
    strip.background = element_rect('floralwhite'), 
    plot.subtitle = element_markdown(margin = margin(b=8), hjust = .35),
    plot.title = element_text(margin = margin(b=8)),
    strip.text = element_text(face = 'bold'),
    plot.caption = element_text(hjust = c(1, -.08)), 
    panel.spacing = unit(1, 'lines'), 
    axis.text.x = element_text(face = 'italic'), 
    axis.text.y = element_text(face = 'italic')
    ) + 
  labs(
    title = 'Most Improved Scorers By Increase In PPG', 
    subtitle = "Variation in PPG and FG% from <b style = 'color:#B22222'>2023</b> 
                to <b style = 'color:#1C86EE'>2024</b> season | Ordered by descending PPG change", 
    x = 'Field Goal Percentage', 
    y = 'Points Per Game',
    caption = c('@dvdtssn | stats.nba.com', paste0('Updated ', format.Date(Sys.Date(), '%B %d, %Y')))
    ) + 
  scale_x_continuous(breaks = c(45, 50, 55, 60), labels = c('45%', '50%', '55%', '60%')) + 
  scale_y_continuous(breaks = c(5, 10, 15, 20, 25, 30), labels = c('5', '10', '15', '20', '25', '30'))


# ggsave('/Users/davidetissino/Desktop/Leaders in PPG increase.png', dpi = 'retina')


#### Leaders in PPG change (negative) ####
df <- df %>% filter(totGP > 80)

# sort in descending order for biggest decreases in PPG 
changes <- df[order(df$pts_change, decreasing = F), ]

# keep only bottom 12 in ppg change
changes <- changes[-c(13:272), ]

# fixed order in descending PPG variation 
# fixes player according to decreasing pts_change
changes$player <- factor(changes$player, 
                         levels = changes$player[order(changes$pts_change, decreasing = F)])

changes %>%
  ggplot() +
  geom_point(aes(x = EFF23, y = PTS23), color = 'darkgreen', fill = 'darkolivegreen2', 
             alpha = 0.5, shape = 21, size = 3.5) +
  geom_point(aes(x = EFF24, y = PTS24), color = 'darkorchid4', fill = 'darkorchid',
             alpha = 0.5, shape = 21, size = 3.5) +
  facet_wrap(~player, nrow = 3, strip.position = 'bottom') +
  geom_segment(aes(x = EFF23, y = PTS23, xend = EFF24, yend = PTS24), 
               arrow = arrow(length = unit(.06, "inch")), size = .8, color = 'gray20') +
  theme_davide() + 
  theme(
    strip.background = element_rect('floralwhite'), 
    plot.subtitle = element_markdown(margin = margin(b=8), hjust = .35),
    plot.title = element_text(margin = margin(b=8)),
    strip.text = element_text(face = 'bold'),
    plot.caption = element_text(hjust = c(1, -.08)), 
    panel.spacing = unit(1, 'lines'), 
    axis.text.x = element_text(face = 'italic'), 
    axis.text.y = element_text(face = 'italic')
  ) + 
  labs(
    title = 'Biggest Drops In Points Per Game', 
    subtitle = "Variation in PPG and FG% from <b style = 'color:#6E8B3D'>2023</b> 
                to <b style = 'color:#9932CC'>2024</b> season | Ordered by ascending PPG change", 
    x = 'Field Goal Percentage', 
    y = 'Points Per Game',
    caption = c('@dvdtssn | stats.nba.com', paste0('Updated ', format.Date(Sys.Date(), '%B %d, %Y')))
  ) + 
  scale_x_continuous(breaks = c(30, 40, 50, 60, 70), labels = c('30%', '40%', '50%', '60%', '70%'))

# ggsave('/Users/davidetissino/Desktop/samurai.png', dpi = 'retina')

