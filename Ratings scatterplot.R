### PROJECT 2: OFFENSINVE & DEFENSIVE EFFICIENCY ###


## 1) TEAM SLUG AND COLORS ####


library(ggthemes)
library(extrafont)
library(tidyverse)
library(readr)
library(ggplot2)
library(ggimage)
library(jsonlite)
library(httr)
library(prismatic)

# Custom theme 
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
      plot.title = element_text(face='bold', size=17, hjust = 0),
      plot.subtitle=element_text(face='italic', size=12, hjust = 0, vjust = -1), 
      panel.grid.major = element_line(color='gray90', linetype = 'dashed'),
      plot.margin = margin(.5, .5, .25, .5, "cm")
    ) 
}

# who knows why but works 
Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 10)

# package for professional team colors 
library(teamcolors)
team_colors <- teamcolors %>%
  filter(league=='nba')
# only keep color columns
team_colors <- team_colors[,-c(2,6:11)]
# rename team name column
colnames(team_colors)[1] <- 'team'

# package for team info 
library(nbastatR)
library(ggimage)
infos <- nba_teams() %>%
  select(nameTeam, idTeam, slugTeam) %>%
  rename('team' = 'nameTeam')
# obtain only nba teams abbreviations 
infos <- infos[-c(1:50),] 
# arrange by team id --> easier to merge
infos <- infos %>% arrange(idTeam)

# scraped other table of colors to compare 
library(rvest)

url <- 'https://teamcolorcodes.com/nba-team-color-codes/'

table <- read_html(url) %>%
  html_nodes('table') %>%
  `[[`(4) %>%
  html_table

table <- table[-c(5,6)]

# reassign column names 
names(table) <- c('team', 'web.primary', 'web.secondary', 'web.tertiary')

# combine team info & colors into one 
tms <- merge(infos, team_colors, by='team')


# combine tms & table w/ web colors 
tms <- merge(tms, table, by='team')

# remove all word variables from tms web colors 
# use | to remove many words at the same time
tms$web.primary <- gsub(c('Blue |Red |Green |Hawks |Celtics |Black |Hornets |Purple |Bulls |Cavaliers |Wine ||Royal |Midnight |Warriors |Pacers |Lakers |Good |Land |Pelicans |Navy |Knicks |Thunder |Magic |Silver '),
                        '', as.character(tms$web.primary))
tms$web.secondary <- gsub(c('Volt |Green |Celtics |Gold |White |Teal |Black |Cavaliers |Navy |Blue |Sunshine |Yellow |Royal |Golden |Cream |City |Lake |Pelicans |Red |Knicks |Orange |Sunset |Silver |Gray |'),
                          '', as.character(tms$web.secondary))

# remove tertiary colors 
tms <- tms[-c(6,9)]

# rearrange order of columns 
tms <- tms[,c(1,3,2,4:7)]

# modify colors
tms$secondary <- ifelse(tms$slugTeam=='BOS', '#FFFFFF', tms$secondary)
tms$secondary <- ifelse(tms$slugTeam=='BKN', '#FFFFFF', tms$secondary)
tms$primary <- ifelse(tms$slugTeam=='DET', '#C8102E', tms$primary)
tms$secondary <- ifelse(tms$slugTeam=='DET', '#006bb6', tms$secondary)
tms$secondary <- ifelse(tms$slugTeam=='GSW', '#1D428A', tms$secondary)
tms$primary <- ifelse(tms$slugTeam=='LAC', '#C8102E', tms$primary)
tms$secondary <- ifelse(tms$slugTeam=='LAC', '#1D428A', tms$secondary)
tms$primary <- ifelse(tms$slugTeam=='LAL', '#552583', tms$primary)
tms$secondary <- ifelse(tms$slugTeam=='LAL', '#fdb927', tms$secondary)
tms$primary <- ifelse(tms$slugTeam=='MEM', '#00b2a9', tms$primary)
tms$secondary <- ifelse(tms$slugTeam=='MEM', '#e43c40', tms$secondary)
tms$secondary <- ifelse(tms$slugTeam=='MIA', '#FFFFFF', tms$secondary)
tms$secondary <- ifelse(tms$slugTeam=='NOP', '#C8102E', tms$secondary)
tms$secondary <- ifelse(tms$slugTeam=='OKC', '#EF3B24', tms$secondary)
tms$primary <- ifelse(tms$slugTeam=='PHI', '#ed174c', tms$primary)
tms$secondary <- ifelse(tms$slugTeam=='PHI', '#006bb6', tms$secondary)
tms$primary <- ifelse(tms$slugTeam=='SAC', '#5A2D81', tms$primary)
tms$secondary <- ifelse(tms$slugTeam=='SAC', '#FFFFFF', tms$secondary)
tms$primary <- ifelse(tms$slugTeam=='OKC', '#ef3b24', tms$primary)
tms$secondary <- ifelse(tms$slugTeam=='OKC', '#007ac1', tms$secondary)
tms$secondary <- ifelse(tms$secondary=='WAS', '#FFFFFF', tms$secondary)
tms$secondary <- ifelse(tms$secondary=='TOR', '#FFFFFF', tms$secondary)

# delete web columns as already modified colors 
tms <- tms[,-c(6,7)]

# arrange in idTeam ascending order to better merge with logos info 
tms <- tms %>% arrange(idTeam)

# packages for web scraping 
library(jsonlite)
library(httr)

url <- 'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='
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

res <- GET(url=url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res,'text'))
df <- data.frame(json_resp$resultSets$rowSet)
colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]  

# keep only columns of name, offrtg, dfrtg, netrtg
df <- df[-c(1, 3:8,10,12, 14:46)]

# rename columns

colnames(df)[1] <- 'team'
colnames(df)[2] <- 'Offensive'
colnames(df)[3] <- 'Defensive'
colnames(df)[4] <- 'Net'

# change team name so that we can merge dataframes
df['team'][df['team'] == 'LA Clippers'] <- 'Los Angeles Clippers'

# merge tms & df into one
df <- merge(df, tms, by='team')

# convert to numeric
df$Offensive <- as.numeric(df$Offensive)
df$Defensive <- as.numeric(df$Defensive)
df$Net <- as.numeric(df$Net)

# mean values to plot 
mean_offense <- mean(df$Offensive)
mean_defense <- mean(df$Defensive)


######### Ratings w/ slug ==================

df %>%
  ggplot(aes(Offensive, Defensive)) +
  geom_point(size=8, alpha=0.7, color=df$primary) + 
  labs(x = 'Offensive rating', 
       y = 'Defensive rating', 
       title = 'Offensive and defensive rating team comparison',
       subtitle = 'Data expressed in points scored and allowed per 100 possessions | Updated November 14, 2023',
       caption = '@dvdtssn | Source: stats.nba.com') +
  theme_davide() +
  theme(axis.title.x = element_text(face='bold.italic'),
        axis.title.y = element_text(face='bold.italic'),
        legend.position = 'none', 
        panel.border=element_rect(size= 20), 
        plot.subtitle=element_text(margin=margin(b=10))) +
  # lines corresponding to league averages 
  geom_hline(yintercept = mean_defense, linetype='dashed', alpha=0.7, color='chocolate') + 
  geom_vline(xintercept = mean_offense, linetype='dashed', alpha=0.7, color='chocolate') +
  # labels corrisponding to team abbreviation
  geom_text(aes(label=slugTeam), color=df$secondary, size=2.5, fontface='bold') +
  annotate('text', x=120, y=112.5, label='League average defensive rating', size=3, fontface= 'italic', color='chocolate') +
  annotate('text', x=112.1, y=103.7, label='League average offensive rating', size=3, fontface= 'italic', color='chocolate', angle=90)

# export graph with high quality (retina)
# ggsave('Team ratings.png', path = '/Users/davidetissino/Desktop/R/graphs', dpi='retina', width=2600, height=2600, units='px')



#### --------------------------------space--------------------------------------####




## 2) TEAM LOGOS ####

# custom theme 

library(ggthemes)
library(extrafont)
library(tidyverse)

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
      plot.title = element_text(face='bold', size=17, hjust = 0),
      plot.subtitle=element_text(face='italic', size=12, hjust = 0, vjust = -1), 
      panel.grid.major = element_line(color='gray90', linetype = 'dashed'),
      plot.margin = margin(.5, .5, .25, .5, "cm")
    ) 
}

# who knows why but works 
Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 10)

# load team info file with colors and logos 
infos <- read_csv('/Users/davidetissino/Desktop/R/data/colors and logos.csv')

# scrape info for teams 
# packages for web scraping 
library(jsonlite)
library(httr)

url <- 'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='
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

res <- GET(url=url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res,'text'))
df <- data.frame(json_resp$resultSets$rowSet)
colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]  

# keep only columns of name, offrtg, dfrtg, netrtg
df <- df[-c(1, 3:8,10,12, 14:46)]

# rename columns

colnames(df)[1] <- 'team'
colnames(df)[2] <- 'Offensive'
colnames(df)[3] <- 'Defensive'
colnames(df)[4] <- 'Net'

# change team name so that we can merge dataframes
df['team'][df['team'] == 'LA Clippers'] <- 'Los Angeles Clippers'

# merge df with infos df 
final <- merge(df, infos, by='team')

# remove colors and slug columns as not necessary 
final <- final[,-c(5:8)]

# convert to numeric values
final$Offensive <- as.numeric(final$Offensive)
final$Defensive <- as.numeric(final$Defensive)
final$Net <- as.numeric(final$Net)

# mean values to plot 
mean_off <- mean(final$Offensive)
mean_dif <- mean(final$Defensive)


# create plot 
library(ggimage) # to plot images
final %>%
  ggplot(aes(Offensive, Defensive)) + 
  geom_image(aes(image=logo)) +
  labs(x = 'Offensive rating', 
       y = 'Defensive rating', 
       title = 'Offensive and defensive rating team comparison',
       subtitle = 'Data expressed in points scored and allowed per 100 possessions | Updated November 14, 2023',
       caption = '@dvdtssn | Source: stats.nba.com') +
  theme_davide() +
  theme(axis.title.x = element_text(face='bold.italic'),
        axis.title.y = element_text(face='bold.italic'),
        legend.position = 'none', 
        panel.border=element_rect(size= 20), 
        # to add space between subtitle and graph
        plot.subtitle = element_text(margin = margin(b=10))) +
  # lines corresponding to league averages 
  geom_hline(yintercept = mean_dif, linetype='dashed', alpha=0.7, color='chocolate') + 
  geom_vline(xintercept = mean_off, linetype='dashed', alpha=0.7, color='chocolate') +
  # labels corrisponding to team abbreviation
  annotate('text', x=120, y=112.5, label='League average defensive rating', size=3, fontface= 'italic', color='chocolate') +
  annotate('text', x=112.1, y=104, label='League average offensive rating', size=3, fontface= 'italic', color='chocolate', angle=90)

  
# save as
# ggsave('/Users/davidetissino/Desktop/R/graphs/Ratings with logos.png', dpi='retina', width=2600, height=2600, units='px')
  
# save in icloud
# ggsave('/Users/davidetissino/Library/Mobile Documents/com~apple~CloudDocs/samurai.png', dpi='retina', height=2600, width=2600, units='px')  
  

  
## 3) NET RATINGS GRAPH ####

# Custom theme 

library(ggthemes)
library(extrafont)
library(tidyverse)
library(readr)
library(ggplot2)
library(ggimage)
library(jsonlite)
library(httr)
library(prismatic)


theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='avenir'), 
      axis.title.x = element_text(face='bold.italic', size=11, margin=margin(t=10)),
      axis.title.y = element_text(face='italic', size=12, margin=margin(r=10), angle = 90),
      axis.text.x = element_text(face='bold.italic'),
      axis.text.y = element_text(face='bold.italic'), 
      panel.background = element_rect('floralwhite'), 
      plot.background = element_rect('floralwhite'),
      plot.title = element_text(face='bold', size=17, hjust = 0.5),
      plot.subtitle=element_text(face='italic', size=11, hjust = 0.5, vjust = -1), 
      panel.grid.major = element_line(color='gray90', linetype = 'dashed'),
      plot.margin = margin(.5, .5, .25, .5, "cm")
    ) 
}

# load logos and colors csv
infos <- read_csv('/Users/davidetissino/Desktop/R/data/colors and logos.csv')

# scrape ratings data
url <- 'https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision='

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

res <- GET(url=url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res,'text'))
df <- data.frame(json_resp$resultSets$rowSet)
colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]] 

# keep only selected columns 
df <- df[,-c(3:12,14:46)]

# assign new name to columns 
colnames(df)[1] <- 'idTeam'
colnames(df)[2] <- 'team'


# change LA Clippers to properly merge 
df['team'][df['team']=='LA Clippers'] <- 'Los Angeles Clippers'

# create only one dataframe with colors, logos and ratings
complete <- merge(df, infos, by='team') %>%
  .[,-c(2,4)] 

# rename
colnames(complete)[2] <- 'Net'

# convert net to numeric
complete$Net <- as.numeric(complete$Net)


complete$primary <- as.character(complete$primary)
complete$secondary <- as.character(complete$secondary)

# mean net rating to plot in the graph
mean_net <- mean(complete$Net)

# needed to be able to display logos in ascending order
complete$slugTeam <- factor(complete$slugTeam, levels = rev(unique(complete$slugTeam)))



## Graph with logos on columns ====

# Need to modify some colors for better readability: BOS, PAC, MEM, MIA, MIN, PHI, KINGS, SPU/TOR

complete$secondary <- ifelse(complete$slugTeam == 'BOS', '#008348', complete$secondary)
complete$secondary <- ifelse(complete$slugTeam == 'IND', '#ffc633', complete$secondary)
complete$secondary <- ifelse(complete$slugTeam == 'MEM', '#00b2a9', complete$secondary)
complete$secondary <- ifelse(complete$slugTeam == 'MIA', '#98002e', complete$secondary)
complete$secondary <- ifelse(complete$slugTeam == 'MIN', '#005083', complete$secondary)
complete$secondary <- ifelse(complete$slugTeam == 'PHI', '#006bb6', complete$secondary)
complete$secondary <- ifelse(complete$slugTeam == 'SAC', '#5A2D81', complete$secondary)
complete$secondary <- ifelse(complete$slugTeam == 'SAS', '#bac3c9', complete$secondary)
complete$secondary <- ifelse(complete$slugTeam == 'CLE', '#860038', complete$secondary)
complete$secondary <- ifelse(complete$slugTeam == 'HOU', '#c4ced3', complete$secondary)
complete$secondary <- ifelse(complete$slugTeam == 'LAL', '#552583', complete$secondary)
complete$secondary <- ifelse(complete$slugTeam == 'POR', '#e03a3e', complete$secondary)
complete$secondary <- ifelse(complete$slugTeam == 'WAS', '#002b5c', complete$secondary)
complete$secondary <- ifelse(complete$slugTeam == 'ATL', '#e13a3e', complete$secondary)
complete$secondary <- ifelse(complete$slugTeam == 'LAC', '#c8102e', complete$secondary)

# NOTE: order is important --> add first geom_col() and then geom_image() 
# if inverted order, logos are 'behind' the columns!!

complete %>%
  ggplot(aes(Net, slugTeam)) +
  # make colors a bit darker
  geom_col(aes(fill = secondary, color = after_scale(clr_darken(fill, 0.4))), 
           alpha=0.9, 
           width = 0.75) +
  # add logos on columns
  geom_image(aes(image=logo), size=0.05) +
  theme_davide() + 
  # line corresponding to 0
  geom_vline(xintercept = 0, color='burlywood3') +
  scale_color_identity(aesthetics =  c("fill", 'color')) + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        plot.title = element_text(size=19, hjust = 0.5),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(margin = margin(b=15), hjust = 0.5),
        axis.text.x = element_text(face='bold.italic')
  )  + 
  labs(y='', 
       x = 'Net rating per 100 possessions', 
       caption = '@dvdtssn | stats.nba.com', 
       title = 'Teams net rating', 
       # %B for full month (%b abbreviated)
       subtitle = paste0('Data updated ', format.Date(Sys.Date(), '%B %d, %Y'))) + 
  scale_x_continuous(breaks = c(-15, -10, -5, 0, 5, 10, 15), labels=c('-15','-10', '-5','0','+5','+10', '+15'))

# ggsave('/Users/davidetissino/Desktop/R/graphs/Net ratings.png', dpi='retina', width=2100, height = 2100, units='px')

# ggsave('/Users/davidetissino/Library/Mobile Documents/com~apple~CloudDocs/R/Net ratings 12.12.png', dpi='retina', height=2600, width=2600, units='px')  



# Graph with logos on axis ====

complete %>%
  ggplot(aes(Net, slugTeam)) + 
  # add logos aligned on y axis
  geom_image(x=-13, aes(image=logo), size=0.04) +
  # expand graph limits to do so
  expand_limits(x=-13) + 
  theme_davide() + 
  # line corresponding to 0
  geom_vline(xintercept = 0, color='chocolate') +
  geom_col(aes(fill=secondary, color=after_scale(clr_darken(fill, 0.4)))) + 
  scale_color_identity(aesthetics =  c("fill", 'color')) + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(margin = margin(b=13)),
        axis.text.x = element_text(face='bold.italic'), 
  ) + 
  labs(y='', 
       x = 'Net rating per 100 possessions', 
       caption = '@dvdtssn | stats.nba.com', 
       title = 'Teams net rating', 
       # %B for full month (%b abbreviated)
       subtitle = paste0('Data updated ', format.Date(Sys.Date(), '%B %d, %Y')))


# ggsave('/Users/davidetissino/Desktop/cobra.png', dpi='retina', width=2100, height = 2100, units='px')

# ggsave('/Users/davidetissino/Library/Mobile Documents/com~apple~CloudDocs/cobra.png', dpi='retina', height=2600, width=2600, units='px')  





