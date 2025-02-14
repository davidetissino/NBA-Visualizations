### HIGHEST SCORING GAMES ####

library(httr)
library(jsonlite)
library(gt)
library(tidyverse)
library(fontawesome)

# increase buffer to scrape 
Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 2)

# load all RS gamelogs
rs_logs <- read_csv('/Users/davidetissino/Desktop/R/data/All Gamelogs.csv')

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

# scrape playoffs gamelogs 
gamelogs <- function(season) {
  
  url <- paste0('https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&ISTRound=&LeagueID=00&PlayerOrTeam=P&Season=',
                season, '&SeasonType=Playoffs&Sorter=DATE')
  
  res <- GET(url = url, add_headers(.headers = headers))
  resp <- fromJSON(content(res, 'text'))
  
  logs <- data.frame(resp$resultSets$rowSet)
  colnames(logs) <- resp[['resultSets']][['headers']][[1]]
  
  return(logs)
  
}

# vector of seasons
seasons <- c(1946:2022)

# scrape ALL playoffs gamelogs 1946 - 2022
po_logs <- map_df(seasons, gamelogs)

# scrape boxscores for this season 
url <- 'https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&ISTRound=&LeagueID=00&PlayerOrTeam=P&Season=2023-24&SeasonType=Regular%20Season&Sorter=DATE'

res <- GET(url = url, add_headers(.headers = headers))
resp <- fromJSON(content(res, 'text'))

logs_24 <- data.frame(resp$resultSets$rowSet)
colnames(logs_24) <- resp[['resultSets']][['headers']][[1]]

# merge rs_logs with logs_24 
logs_rs <- rbind(rs_logs, logs_24)

# merge logs_rs and po_logs
df <- rbind(logs_rs, po_logs)

df <- df %>% mutate_at(11:32, as.numeric)

df <- df[order(df$PTS, decreasing = T), ]

df <- df[,c(3, 8, 11, 13:14, 18, 29)]

df <- df[-c(11:nrow(df)), ]

df$FG_PCT <- df$FG_PCT * 100

df <- df[,-3]

df$GAME_DATE <- format(df$GAME_DATE, format = "%B, %Y")



# table
df %>% 
  gt() %>% 
  cols_label(
    PLAYER_NAME = 'PLAYER', 
    GAME_DATE = 'DATE',
    FGA = 'FGA', 
    FG_PCT = 'FG%',
    FTM = 'FTM', 
    # md necessary to add icon of arrown 
    PTS = md(paste(fontawesome::fa('arrow-down', fill = 'black'), 'PTS'))
  ) %>%
  tab_header(
    title = md('**Most Points in a Game**'), 
    subtitle = 'In NBA history, including regular season and playoffs'
  ) %>%
  tab_options(
    table.background.color = "white",
    column_labels.font.size = 11,
    table.font.size = 14,
    heading.title.font.size  = 25,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 11,
    table.border.top.color = "transparent",
    data_row.padding = px(6), 
    footnotes.font.size = 10,
    source_notes.font.size = 9,
    footnotes.padding = px(4), 
    table.border.bottom.color = 'grey40',
    heading.border.bottom.color = 'grey40',
    column_labels.border.bottom.color = 'grey40'
    ) %>%
  tab_footnote(footnote = '@dvdtssn | Source: stats.nba.com') %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = vars(c(PLAYER_NAME, PTS))
    )) %>% 
  tab_style(
    style = cell_text(color = 'grey30', size = px(9)), 
    locations = cells_body(columns = vars(GAME_DATE))
  ) %>% 
  tab_style(
    style = cell_text(size = px(11)),
    locations = cells_body(columns = vars(c(FGA, FG_PCT, FTM)))
  ) %>%
  data_color(
    rows = PLAYER_NAME == 'Luka Doncic',
    color = 'antiquewhite'
  ) %>%
  tab_style(
    style = cell_text(color = 'firebrick', size = px(15)), 
    locations = cells_body(columns = vars(PTS))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = 'chocolate'),
    locations = cells_column_labels()) %>% 
  tab_style(
    style = cell_text(color = "darkblue", weight = "bold"),
    locations = cells_body(
      columns = vars(FG_PCT),
      rows = FG_PCT >= 75
    )) %>%
  tab_style(
    style = cell_text(size = px(13)), 
    locations = cells_body(columns = vars(PLAYER_NAME))
  ) 




