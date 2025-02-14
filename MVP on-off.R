### PROJECT 7: MVP LEADERS ON/OFF TRACKER #### 

library(ggthemes)
library(extrafont)
library(tidyverse)
library(httr)
library(jsonlite)
library(extrafont)
library(readr)
library(janitor)
library(rvest)
library(ggforce)

# custom theme
theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='avenir'), 
      axis.title.x = element_text(face='italic', size=10, margin=margin(t=10)),
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


# data from basketball reference on players on/off ===
url <- 'https://www.basketball-reference.com/leagues/NBA_2024_play-by-play.html'

bbr <-  read_html(url) %>%
  html_nodes('table') %>%
  .[[1]] %>%
  html_table(fill = T)

bbr <- bbr %>%
  as.data.frame()


# nba data on teams advanced stats === 
url1 <- 'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='

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

res <- GET(url=url1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res,'text'))
adv <- data.frame(json_resp$resultSets$rowSet)
colnames(adv) <- json_resp[["resultSets"]][["headers"]][[1]]  


# load csv with team infos ===
tms <- read_csv('/Users/davidetissino/Desktop/R/data/colors and logos.csv')


### Cleaning====

# remove repeated rows
bbr <- bbr[-c(22, 45, 66, 87, 110, 131, 152, 173, 194, 215, 236, 257, 278, 
              301, 322, 343, 366, 387, 408, 429, 452, 475, 496),]

# set appropriate headers 
bbr <- bbr %>% 
  row_to_names(1, remove_rows_above = F)

# filter and keep only player, team, minutes and on/off data 
bbr <- bbr[,-c(1,3,4,6, 8:12,15:23)]

# convert to numeric
colnames(bbr)[2] <- 'slugTeam'
colnames(bbr)[5] <- 'on_off'

bbr$OnCourt <- as.numeric(as.character(bbr$OnCourt))
bbr$MP <- as.numeric(as.character(bbr$MP))
bbr$on_off <- as.numeric(as.character(bbr$on_off))


# keep only info for top 10 MVP dashboard 
bbr <- bbr %>%
  filter(Player %in% c('Nikola Jokić', 'Joel Embiid', 'Jayson Tatum', 'Luka Dončić', 
                       'Shai Gilgeous-Alexander', 'Anthony Edwards', 'Tyrese Haliburton',
                       'Kevin Durant', 'Giannis Antetokounmpo', 'LeBron James')) 

# keep teams and net rating info
adv <- adv[,-c(1,3:12,14:46)] 

colnames(adv)[1] <- 'team'
colnames(adv)[2] <- 'Net'

adv$Net <- as.numeric(as.character(adv$Net))

# merge with tms 
adv <- merge(adv, tms, by = 'team')

# keep only MVP teams
adv <- adv %>%
  filter(slugTeam %in% c('DEN', 'DAL', 'PHI', 'MIN', 'BOS', 'OKC', 'IND', 'PHX', 'MIL', 'LAL'))

adv <- adv[,-c(3,7)]

# merge adv and bbr into one (change Suns slug)
bbr['slugTeam'][bbr['slugTeam'] == 'PHO'] <- 'PHX'

final <- merge(adv, bbr, by = 'slugTeam')

final <- final[,-c(2,7)]

# reoder to reflect dashboard
final <- final %>%
  arrange(match(Player, c('Nikola Jokić', 'Luka Dončić', 'Joel Embiid', 'Jayson Tatum',  
                          'Shai Gilgeous-Alexander', 'Anthony Edwards', 'Tyrese Haliburton',
                          'Kevin Durant', 'Giannis Antetokounmpo', 'LeBron James')))

## Evaluate NET with player OFF court ====
# create specific OFF the court column ## 
final <- final %>%
  mutate(off = case_when(
    OnCourt >= 0 & on_off >= 0 ~ OnCourt - on_off, 
    OnCourt >= 0 & on_off <0 ~ OnCourt - abs(on_off), 
    OnCourt < 0 & on_off >= 0 ~ OnCourt - on_off, 
    OnCourt < 0 & on_off < 0 ~ OnCourt - abs(on_off)
  ))

# remove unnecessary columns 
final <- final[,-2]

# rename on court net rating column
colnames(final)[5] <- 'on'
final$Player <- as.character(as.factor(final$Player))

# modify names 
final['Player'][final['Player'] == 'Shai Gilgeous-Alexander'] <- 'S. Alexander'
final['Player'][final['Player'] == 'Nikola Jokić'] <- 'N. Jokić'
final['Player'][final['Player'] == 'Luka Dončić'] <- 'L. Dončić'
final['Player'][final['Player'] == 'Joel Embiid'] <- 'J. Embiid'
final['Player'][final['Player'] == 'Jayson Tatum'] <- 'J. Tatum'
final['Player'][final['Player'] == 'Anthony Edwards'] <- 'A. Edwards'
final['Player'][final['Player'] == 'Tyrese Haliburton'] <- 'T. Haliburton'
final['Player'][final['Player'] == 'Kevin Durant'] <- 'K. Durant'
final['Player'][final['Player'] == 'Giannis Antetokounmpo'] <- 'G. Antetokounmpo'
final['Player'][final['Player'] == 'LeBron James'] <- 'L. James'

# order in custom order 
final$Player <- factor(final$Player, levels = rev(unique(final$Player)))

### PLOT ## 

final %>%
  ggplot() + 
  # allows to plot multiple values for same variable & links them with line
  geom_link(aes(x = on, y = Player, xend = off, yend = fct_reorder(Player, off)),  
           size = 1.5, alpha = 0.7, color = 'seashell4') +
  # adds point aesthetic to net rating w/ player OFF
  geom_point(aes(off, y = fct_reorder(Player, on)), color = 'firebrick3', size = 3.5) + 
  # adds point aes w/ player ON
  geom_point(aes(on, y = fct_reorder(Player, off)), color = 'darkgreen', size = 3.5) + 
  theme_davide() +
  labs(
    title = 'Team Net Rating Differential For MVP Favorites',
    subtitle = paste0('Data updated ', format.Date(Sys.Date(), '%B %d, %Y')), 
    caption = '@dvdtssn | basketballreference', 
    x = 'Net rating with player on or off the court'
  ) + 
  scale_x_continuous(breaks = c(-15, -10, -5, 0, 5,10, 15), labels=c('-15','-10', '-5','0','+5','+10', '+15')) + 
  theme( 
    plot.title = element_text(hjust=1.2), 
    plot.subtitle = element_text(hjust=0.25, margin = margin(b=10)),
    axis.title.x = element_text(hjust=0.35, margin = margin(b=5, t = 8)),
    axis.title = element_blank(), 
    axis.title.y = element_blank(),
    ) +
  geom_vline(xintercept = 0, color='burlywood3')

# ggsave('/Users/davidetissino/Desktop/sos.png', dpi='retina', width=6, height=6, units='in')


### LEGEND ##

# create custom df 
df <- data.frame(x = c(.3, .5, .5, .7), y = c(1.2, 1, 1.1, 1.2))

# needed for brace
library(ggbrace)

# create plot for key  
df %>%
  ggplot(aes(x, y)) + 
  # not using theme_davide as did not recognize panel border
  theme_bw()+
  # creates line connecting points
  geom_link(aes(x = 0.3, y = 1.2, xend = 0.7, yend = 1.2), color = "seashell4", size = 2)  +
  labs(title = "Key") +
  coord_cartesian(clip = 'off') +
  # creates a brace (ATTENTION to placement, before points otherwise overlaps)
  stat_brace() +
  # added a series of points, made invisible to better plot
  geom_point(aes(x = .7, y = 1.2), color = "darkgreen",size = 3.5) + 
  geom_point(aes(x = .3, y = 1.2), color = "firebrick3", size = 3.5) + 
  geom_point(aes(x = .5, y = 1.1), color = "floralwhite", size = 3.5) + 
  geom_point(aes(x = 0.5, y = 1.35), color = 'floralwhite') +
  geom_point(aes(x = 0.2, y = 1.12), color = 'floralwhite')+
  geom_point(aes(x=.8, y = 1.12), color = 'floralwhite') +
  theme(legend.position = 'none', 
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        plot.title = element_text(hjust = .5, vjust = -1, face = 'bold', size = 12), 
        panel.background = element_rect('floralwhite'), 
        plot.background = element_rect(fill = 'floralwhite', color = 'black'),
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(), 
        panel.border = element_blank()
        ) + 
  # create annotations for the points and top of bracket
  annotate(geom = 'text', x = 0.29, y = 1.12, label = "Net Rating\nWith Player OFF", size = 1.5, hjust = .5, lineheight = 1) + 
  annotate(geom = 'text', x = .7, y =1.12, label = "Net Rating\nWith Player ON", size = 1.5, hjust = .5, lineheight = 1) +
  annotate(geom = 'text', x = 0.5, y = 1.33, label = 'On/Off Net Rating Differential', size = 2) 


# ggsave('/Users/davidetissino/Desktop/R/graphs/MVP on-off legend.png', dpi='retina', width=1.3, height=1.1, units = 'in')


### LEGEND & GRAPH ### 

# to patch legend on top of graph 
library(magick)

# read legend 
legend <- image_read('/Users/davidetissino/Desktop/R/graphs/MVP on-off legend.png')

# read graph 
plot <- image_read('/Users/davidetissino/Desktop/R/graphs/MVP on-off graph.png')

# overlays the two images specifying position
image_composite(plot, legend, offset = '+450 +500') %>% 
  image_write('/Users/davidetissino/Library/Mobile Documents/com~apple~CloudDocs/R/onoff.png')

image_composite(plot, legend, offset = '+450 +500') %>%
  image_write('/Users/davidetissino/Desktop/R/graphs/MVP on-off.png')






