#### Points Per Quarter Leaders ####

library(httr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggimage)
library(nbastatR)

# custom theme
theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='avenir'), 
      axis.title.x = element_text(face='bold.italic', size=12, margin=margin(t=10)),
      axis.title.y = element_text(face='bold.italic', size=12, margin=margin(r=10), angle = 90),
      axis.text.x = element_text(face='plain'),
      axis.text.y = element_text(face='plain'), 
      panel.background = element_rect('floralwhite'), 
      plot.background = element_rect('floralwhite'),
      plot.title = element_text(face='bold', size=15, hjust = 0.5),
      plot.subtitle=element_text(face='italic', size=10, hjust = 0.5, vjust = -1), 
      panel.grid.major = element_line(color='gray90', linetype = 'dashed'),
      plot.margin = margin(.5, .5, .25, .5, "cm")
    ) 
}

# increase buffer to scrape 
Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 2)

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

# Process for each quarter ====
url1 <- 'https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=1&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

res1 <- GET(url = url1, add_headers(.headers = headers))

json_resp1 <- fromJSON(content(res1, 'text'))

df1 <- data.frame(json_resp1$resultSets$rowSet)

colnames(df1) <- json_resp1[['resultSets']][['headers']][[1]]

df1 <- df1[,-c(1,3:30,32:66)]



# Function ====
quarter_leaders <- function(n_period) {
  
  url <- paste0('https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=', n_period, '&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=')
  
  res <- GET(url = url, add_headers(.headers = headers))
  
  json_resp <- fromJSON(content(res, 'text'))
  
  df <- data.frame(json_resp$resultSets$rowSet)
  
  colnames(df) <- json_resp[['resultSets']][['headers']][[1]]
  
  df <- df %>%
    mutate(n_quarter = n_period) %>%
    select(PLAYER_NAME, PTS, FG_PCT, n_quarter)
  
  return(df)
  
}


# df for each quarter
df1 <- quarter_leaders(1)
df2 <- quarter_leaders(2)
df4 <- quarter_leaders(4)
df3 <- quarter_leaders(3)

# numeric columns 
df1$PTS <- as.numeric(df1$PTS)
df2$PTS <- as.numeric(df2$PTS)
df3$PTS <- as.numeric(df3$PTS)
df4$PTS <- as.numeric(df4$PTS)

# order in descending Points/quarter
df1 <- df1[order(df1$PTS, decreasing = T), ] %>%
  .[-c(31:446),]
  
df2 <- df2[order(df2$PTS, decreasing = T), ] %>%
  .[-c(31:446),]

df3 <- df3[order(df3$PTS, decreasing = T), ] %>%
  .[-c(31:448),]

df4 <- df4[order(df4$PTS, decreasing = T), ] %>%
  .[-c(31:517),]

# final df 
df <- rbind(df1, df2, df3, df4)

#
df <- df %>% mutate_at(2:4, as.numeric)

df$FG_PCT <- df$FG_PCT * 100

# load csv with players photos 
photos <- read_csv('/Users/davidetissino/Desktop/R/data/Active Index & Photo.csv')

# add column with full name 
photos$PLAYER_NAME <- paste0(photos$PLAYER_FIRST_NAME, ' ', photos$PLAYER_LAST_NAME)

# combine two dfs
final <- merge(df, photos, by = 'PLAYER_NAME') %>%
  .[,-c(5:9)]


final$quarter <- ifelse(final$n_quarter == 1, '1st Quarter', 
                        ifelse(final$n_quarter == 2, '2nd Quarter', 
                               ifelse(final$n_quarter == 3, '3rd Quarter', '4th Quarter')))





# plot 
final %>% 
  ggplot(aes(x = FG_PCT, y = PTS)) + 
  facet_wrap(~quarter, strip.position = 'bottom') + 
  geom_image(aes(image = photo), size = .2) + 
  theme_davide() + 
  theme(
    strip.background = element_rect('floralwhite'), 
    plot.caption = element_text(hjust = 1), 
    plot.subtitle = element_text(margin = margin(b=10)),
    panel.spacing.x = unit(1.5, 'lines')
  ) +
  labs(
    x = 'Field Goal Percentage', 
    y = 'Points Per Quarter', 
    title = 'Quarter Scoring Leaders', 
    subtitle = paste0('Leaders in points scored per quarter | Updated ', format.Date(Sys.Date(), '%B %d, %Y')),
    caption = '@dvdtssn | stats.nba.com'
    ) +
  scale_x_continuous(breaks = c(40, 50, 60, 70), labels = c('40%', '50%', '60%', '70%'))


# 
ggsave('/Users/davidetissino/Desktop/sa4.png', dpi = 'retina')



final %>% 
  ggplot(aes(x = FG_PCT, y = PTS)) + 
  facet_wrap(~quarter, strip.position = 'bottom') + 
  geom_image(aes(image = photo), size = .1, nudge_y = .05) + 
  geom_point(shape = 21, size = 7, color = 'black', fill = 'grey85', alpha = .3) +
  # custom point for Joel Embiid
  geom_point(data = final %>% filter(PLAYER_NAME == 'Joel Embiid' & quarter %in% c('1st Quarter', '3rd Quarter')), shape = 21, 
             size = 9, color = '#ed174c', fill = '#006bb6', alpha = .3) + 
  # custom for Luka
  geom_point(data = final %>% filter(PLAYER_NAME == 'Luka Doncic' & quarter == '2nd Quarter'), shape = 21, 
             fill = '#007dc5', color = '#c4ced3', alpha = .3, size = 9) + 
  # custom Simons 
  geom_point(data = final %>% filter(PLAYER_NAME == 'Anfernee Simons' & quarter == '4th Quarter'), shape = 21, 
             color = '#bac3c9', fill = '#e03a3e', alpha = .5, size = 9) +
  theme_davide() + 
  theme(
    strip.background = element_rect('floralwhite'), 
    plot.caption = element_text(hjust = 1), 
    plot.subtitle = element_text(margin = margin(b=10)), 
    panel.spacing.x = unit(1, 'lines')
  ) +
  labs(
    x = 'Field Goal Percentage', 
    y = 'Points Per Quarter', 
    title = 'Scoring Leaders Per Quarter', 
    subtitle = paste0('Leaders in points scored per quarter | Updated ', format.Date(Sys.Date(), '%B %d, %Y')),
    caption = '@dvdtssn | stats.nba.com'
  ) +
  scale_x_continuous(breaks = c(40, 50, 60, 70), labels = c('40%', '50%', '60%', '70%'))











final %>% 
  ggplot(aes(x = PTS, y = FG_PCT)) + 
  geom_point() + 
  facet_wrap(~ n_quarter)





















