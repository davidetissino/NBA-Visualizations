#### FTA per game ####

library(jsonlite)
library(httr)
library(ggthemes)
library(extrafont)
library(tidyverse)
library(readr)
library(ggplot2)
library(ggrepel)

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

# stats for 2023 - 24 season ====
url <- 'https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

res <- GET(url = url, add_headers(.headers = headers))

json_resp <- fromJSON(content(res, 'text'))

df <- data.frame(json_resp$resultSets$rowSet)

colnames(df) <- json_resp[['resultSets']][['headers']][[1]]

df <- df %>% mutate_at(6:66, as.numeric)


# info on FTs ====
ft <- df[,-c(1,3,4, 6, 8:10, 12:18, 21:66)]

# convert to numeric 
ft <- ft %>% mutate_at(3:6, as.numeric)

ft$FT_PCT <- ft$FT_PCT * 100

# filter for players having played > 20 games, > 30 mins per game
ft <- ft %>% filter(ft$MIN > 30)

ft <- ft %>% filter(ft$GP > 17)

colnames(ft)[1] <- 'player'


# league averages in FT% and FTA 
fta <- mean(df$FTA)

ftp <- mean(df$FT_PCT) * 100

# PPG leaders 
ldrs <- df[,-c(1,3:6, 8:10, 12:30,32:66)] 

ldrs$PTS <- as.numeric(ldrs$PTS) 

# reorder in descending PPG order and filter for GP, MINS 
ppg <- ldrs[order(ldrs$PTS, decreasing = T), ]

ppg <- ppg %>% filter(ppg$MIN > 30)
ppg <- ppg %>% filter(ppg$GP > 17)

# keep only Top10 in PPG
ppg <- ppg[-c(11:507),]

# new column with abbreviated name and surname
ft$abbr <- paste0(substr(ft$player, 1, 1), '. ', word(ft$player, -1))

# manual tweaks to some names 
ft$abbr <- ifelse(ft$player == 'Jabari Smith Jr.', 'Jabari', ft$abbr)
ft$abbr <- ifelse(ft$player == 'Jaren Jackson Jr.', 'JJJ', ft$abbr)
ft$abbr <- ifelse(ft$player == 'Giannis Antetokounmpo', 'Giannis', ft$abbr)
ft$abbr <- ifelse(ft$player == 'Stephen Curry', 'Steph', ft$abbr)
ft$abbr <- ifelse(ft$player == 'Joel Embiid', 'Joel', ft$abbr)
ft$abbr <- ifelse(ft$player == 'Paul George', 'PG13', ft$abbr)

# change team abbr column to merge 
colnames(ft)[2] <- 'slugTeam'

# final df with ft info and team colors
final <- merge(ft, tms, by = 'slugTeam')

# remove some columns 
final <- final[,-c(8,9,12)]

final %>% 
  ggplot(aes(x = FTA, y = FT_PCT)) + 
  geom_point(color = 'chocolate4', fill = 'burlywood3', size = 2.3, alpha = .8, shape = 21) +
  geom_text_repel(segment.size = .15 , data = final %>% filter(player != 'Stephen Curry' & player != 'Joel Embiid' & abbr != 'Giannis' & abbr != 'PG13'), 
                  aes(label = abbr), size = 1.5, position = position_jitter()) + 
  theme_davide() + 
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12), labels = c('0', '2', '4', '6', '8', '10', '12')) + 
  scale_y_continuous(breaks = c(60, 70, 80, 90), labels = c('60%', '70%', '80%', '90%')) + 
  labs(title = 'Charity Stripe Leaders', 
       subtitle = 'Among players having played more than 30 mins per game in more than 20 games', 
       x = 'Free Throws Attempted Per Game', 
       y = 'Free Throw Percentage', 
       caption = c('@dvdtssn | stats.nba.com', paste0('Updated ', format.Date(Sys.Date(), '%B %d, %Y')))) + 
  theme(plot.title = element_text(hjust = 0.475),
        plot.subtitle = element_text(margin = margin(b=15)), 
        plot.caption = element_text(hjust = c(1, 0))) + 
  geom_hline(yintercept = ftp, linetype = 'dashed', alpha = 0.7, color = 'chocolate3') + 
  annotate(geom='text', x=9, y=ftp + .5, label='League Average FT%', size=2.3, fontface='italic', color='chocolate3') +
  # annotations for league leaders 
  geom_point(data = final %>% filter(player == 'Stephen Curry'), 
             size = 4, alpha = 1, shape = 21, color = '#fdb927', fill = '#1D428A') +
  geom_text_repel(data = final %>% filter(player == 'Stephen Curry'), 
                  aes(label = abbr, fontface = 'bold'), nudge_y = .9, nudge_x = .3, segment.color = NA, size = 2.7) +
  geom_point(data = final %>% filter(player == 'Joel Embiid'), 
             size = 4, alpha = 1, shape = 21, color = '#006bb6', fill = '#ed174c')+
  geom_text_repel(data = final %>% filter(player == 'Joel Embiid'), 
                  aes(label = abbr, fontface = 'bold'), nudge_y = -1, nudge_x = -.35, segment.color = NA, size = 2.7) +
  geom_point(data = final %>% filter(player == 'Giannis Antetokounmpo'), 
             size = 4, alpha = 1, shape = 21, color = '#f0ebd2', fill = '#00471b')+
  geom_text_repel(data = final %>% filter(player == 'Giannis Antetokounmpo'), 
                  aes(label = abbr, fontface = 'bold'), nudge_y = 1, nudge_x = -.45, segment.color = NA, size = 2.7) +
  geom_point(data = final %>% filter(player == 'Paul George'), 
             size = 4, alpha = 1, shape = 21, color = '#1D428A', fill = '#C8102E') +
  geom_text_repel(data = final %>% filter(player == 'Paul George'), 
                  aes(label = abbr, fontface = 'bold'), nudge_y = .9, nudge_x = .35, segment.color = NA, size =2.7)


# ggsave('/Users/davidetissino/Desktop/miao1.png', dpi = 'retina')

# ggsave('/Users/davidetissino/Library/Mobile Documents/com~apple~CloudDocs/miao.png', dpi='retina')  


