## PROJECT: NBA leading ppg and corresponding efficiency ## 

library(tidyverse)
library(httr)
library(jsonlite)
library(ggplot2)
library(extrafont) # call font() to see all available fonts
library(ggthemes) # for custom themes 
library(ggrepel) # to customize text labels 


# get data for ppg and player name
url <- 'https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

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

# assign appropriate name to columns
colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]  

# remove unnecessary columns 
df <- df[-c(1,3:10,12:30,32:66)]

# rename columns
colnames(df)[1] <- 'Player'
colnames(df)[3] <- 'PPG'
colnames(df)[2] <- 'Mins'

df$Mins <- as.numeric(df$Mins)


# get data for player name and eFG
url1 <- 'https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

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

res1 <- GET(url=url1, add_headers(.headers = headers))
json_resp1 <- fromJSON(content(res1, 'text'))
df1 <- data.frame(json_resp1$resultSets$rowSet)

#assign appropriate names to columns 
colnames(df1) <- json_resp1[['resultSets']][['headers']][[1]]

# keep name and efg% columns 
df1 <- df1[-c(1,3:28,30:78)]

# rename columns 
colnames(df1)[1] <- 'Player'
colnames(df1)[2] <- 'eFG'

# convert eFG to numeric and express eFG in %
df1$eFG <- as.numeric(df1$eFG)
df1$eFG <- df1$eFG*100
df$PPG <- as.numeric(df$PPG)

# merge two dataframes according to relevant info (player name)
final <- merge(df, df1, by='Player')

# column with abbreviated names
final$abbr_name <- paste0(substr(final$Player, 1, 1), '. ', word(final$Player, -1))

# abbreviate specific values 
final$abbr_name <- ifelse(final$Player == "Shai Gilgeous-Alexander", "Shai", final$abbr_name)
final$abbr_name <- ifelse(final$Player=='Giannis Antetokounmpo', 'Giannis', final$abbr_name)
final$abbr_name <- ifelse(final$Player=='Jaren Jackson Jr.', 'JJJ', final$abbr_name)

# save as csv
write_csv(final, '/Users/davidetissino/Desktop/R/data/ppg&eff.csv')

# filter according to parameters
final <- filter(final, final$Mins>25)
final <- filter(final, final$PPG>22)

# define league average eFG to plot in graph later
league_mean_eFG <- mean(df1$eFG)

# graph creation 
final %>%
  # select x and y to be represented 
  ggplot(aes(PPG, eFG)) + 
  # type of graph (point) + corresponding point color 
  geom_point(color='firebrick4', size=4, alpha=0.45) +
  # text repel to better visualize text related to dots 
  geom_text_repel(aes(label=abbr_name), size=3.5, position = position_jitter()) +
  # labels to axis + title and subtitle 
  labs(x= 'Points Per Game', 
       y= 'Effective Field Goal Percentage', 
       title = 'Points per game and corresponding effective FG percentage',
       subtitle = 'Parameters used: more than 25mins/game, more than 22ppg | Updated 11.9.2023', 
       caption = '@dvdtssn | stats.nba.com') +
  theme(
    # text font
    text = element_text(family='avenir'), 
    # axis appearance + label distance from tickers
    axis.title.x = element_text(face='bold.italic', size=13, margin=margin(t=10)),
    axis.title.y = element_text(face='bold.italic', size=13, margin=margin(r=10)),
    # tickers 
    axis.text.x = element_text(face='bold'),
    axis.text.y = element_text(face='bold'), 
    # only panel color 
    panel.background = element_rect('floralwhite'), 
    # only "margins" color 
    plot.background = element_rect('floralwhite'),
    # title 
    plot.title = element_text(face='bold', size=15),
    # subtitle
    plot.subtitle=element_text(face='italic', size=11), 
    # interior lines 
    panel.grid.major = element_line(color='gray90', linetype = 'dashed'),
    # set plot margins --> ordered values Top, Right, Bottom, Left (trouble)
    plot.margin = margin(.5, .5, .25, .5, "cm"),
  ) +
  # modify x tickers displayed  
  scale_x_continuous(breaks=c(22, 23.5, 25, 26.5, 28, 29.5, 31)) +  
  # change text displayed in y tickers 
  scale_y_continuous(breaks=c(50, 55, 60, 65), labels = c('50%', '55%', '60%', '65%')) +
  # add line corresponding to league eFG average
  geom_hline(yintercept = league_mean_eFG, linetype='dashed', alpha=0.7, color='chocolate3') +
  # add label (to average eFG line) 
  annotate(geom='text', x=31, y=50.2, label='League Average eFG', size=3, fontface='italic', color='chocolate3')

# export graph with high quality (retina)
ggsave('PPG & eFG.png', path = '/Users/davidetissino/Desktop/R/graphs', dpi='retina',
        width=2600, height=2600, units='px')




