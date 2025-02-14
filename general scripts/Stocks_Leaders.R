library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)
library(ggthemes)
library(ggimage)
library(gtUtils)
library(extrafont)
library(ggchicklet)
library(gt)
library(gtUtils)



# CONTENT ####
## Scatter for stocks leaders 2024-25
## Table for all time stocks leaders


# Load custom theme
theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='Geist Mono'), 
      axis.title.x = element_text(color = 'black', margin = margin(t = 10, b = 8), family = 'K2D', face = 'bold', size = 15), 
      axis.title.y = element_text(color = 'black', margin = margin(r = 25, l = 8), family = 'K2D', face = 'bold', size = 15, angle = 90), 
      axis.text = element_text(color = 'grey30'),
      axis.text.x = element_text(face='bold', size = 13),
      axis.text.y = element_text(face='bold', size = 13), 
      panel.background = element_rect('grey98'), 
      plot.background = element_rect('grey98'),
      plot.title = element_text(margin = margin(b=15, t = 10), face='bold', size=25, hjust = 0.5, family = 'Proxima Nova'),
      plot.subtitle=element_text(size=12, hjust = 0.5, margin = margin(b = 10), family = 'Proxima Nova'), 
      panel.grid.major = element_line(color='gray80', linetype = 'dashed'),
      plot.margin = margin(0, 10, 15, 10),
      plot.caption = element_text(size = 6.5, hjust = -0.4, vjust = -18)
    ) 
}



# headers for scraping
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


# Active Players Index ####
active <- read.csv('/Users/davidetissino/Desktop/R/data/Active_1.csv') %>% 
  select(
    Player, headshot
  )

# All Players Index ####
all <- read.csv('/Users/davidetissino/Desktop/R/data/Players Index.csv') %>% 
  mutate(
    Player = paste(all$PLAYER_FIRST_NAME, all$PLAYER_LAST_NAME)
  ) #%>% 
  .[, 27]
  
  
# Teams Index ####
tms <- read.csv('/Users/davidetissino/Desktop/R/data/teams.csv')
  


# -------------------------------------------- ###


# CURRENT RS ####
url <- 'https://stats.nba.com/stats/leagueLeaders?LeagueID=00&PerMode=PerGame&Scope=S&Season=2024-25&SeasonType=Regular%20Season&StatCategory=PTS'

res <- GET(url = url, add_headers(.headers = headers))

resp <- fromJSON(content(res, 'text'))

per_game_stats <- data.frame(resp$resultSet$rowSet)

colnames(per_game_stats) <- resp[['resultSet']][['headers']]


# filter for specific columns
per_game_stats <- per_game_stats %>% 
  select(
    PLAYER, TEAM, GP, MIN, STL, BLK
  ) %>% 
  mutate_at(
    c(3:6), 
    as.numeric
  ) %>% 
  # custom STOCKS column 
  mutate(
    STKS = STL + BLK
  ) %>% 
  arrange(
    desc(
      STKS
    )
  ) %>%
  .[, c(1, 2, 3, 4, 7, 5, 6)]
  

colnames(per_game_stats)[1] <- 'Player'


# merge into one DF
df_TY <- merge(per_game_stats, active, by = 'Player') 



## Plot ====
df_TY %>% 
  filter(
    MIN >= 15
  ) %>% 
  ggplot(
    aes(STL, BLK)
  ) +
  geom_image(
    aes(image = headshot),
    size = 0.1
  ) +
  #geom_point() + 
  labs(
    x = 'Steals per Game', 
    y = 'Blocks per Game', 
    title = '',
    subtitle = 'Leaders in Steals and Blocks, 2024-25 Regular Season | Min. 15 Minutes Per Game', 
    caption = 'Source: NBA | Chart: Davide Tissino'
  ) + 
  theme_davide() +
  theme(
    plot.caption = element_text(size = 8, hjust = -0.1, vjust = 0), 
    plot.subtitle = element_text(hjust = 0.4)
  )



ggsave("/Users/davidetissino/Desktop/Stocks_TY.png", w = 9, h = 7, dpi = 'retina')




# -------------------------------------------- ###




# ALL RS ####
PG_stats_RS <- function(season) {
  
  url <- paste0(
    'https://stats.nba.com/stats/leagueLeaders?LeagueID=00&PerMode=PerGame&Scope=S&Season=', 
    season, 
    '&SeasonType=Regular%20Season&StatCategory=PTS'
    
  )
  
  res <- GET(url = url, add_headers(.headers = headers))
  resp <- fromJSON(content(res, 'text'))
  
  pg_stats <- data.frame(resp$resultSet$rowSet)
  colnames(pg_stats) <- resp[['resultSet']][['headers']]
  
  pg_stats$RS <- season
  
  pg_stats <- pg_stats %>% 
    select(
      PLAYER_ID, PLAYER, TEAM, GP, MIN, STL, BLK, RS
    ) %>% 
    mutate_at(
      c(4:7), 
      as.numeric
    ) %>% 
    # custom STOCKS column 
    mutate(
      STKS = STL + BLK
    ) %>% 
    arrange(
      desc(
        STKS
      )
    )
  
  return(pg_stats)
  
}



# vector of seasons 
seasons <- paste0(
  1951:2024, '-', substr(1952:2025, 3, 4)
)



# create final DF
df <- map_df(seasons, PG_stats_RS) %>% 
  arrange(
    desc(
      STKS
    )
  )%>% 
  .[c(1:10), ] %>% 
  .[, c(1, 2, 8, 3:5, 9, 6, 7)]


# create custom player photo column
df$headshot <- paste0(
  'https://cdn.nba.com/headshots/nba/latest/260x190/', 
  df$PLAYER_ID, 
  '.png'
)


# change column name to merge
colnames(df)[4] <- 'slugTeam'


# manually change slug 
df$slugTeam[df$slugTeam == 'UTH'] <- 'UTA'
df$slugTeam[df$slugTeam == 'SAN'] <- 'SAS'


# merge with teams info 
fin <- merge(df, tms, by = 'slugTeam') %>% 
  .[, c(10, 3, 4, 15, 5:9)]



# Table ====
fin %>%
  arrange(desc(STKS)) %>%
  gt()  %>%
  cols_label(headshot = "",
             PLAYER = "",
             logo = "Team",
             RS = "",
             STKS = 'STOCKS'
             
             ) %>%
  tab_header(
    title = md("**All-Time Stocks Leaders**"),
    subtitle = 'Top-10 Players in NBA History for Stocks (Steals + Blocks) per Game'
  )  %>%
  text_transform(
    locations = cells_body(c(headshot)),
    fn = function(x) {
      web_image(url = x,
                height = px(41))
    }
  ) %>%
  text_transform(
    locations = cells_body(c(logo)),
    fn = function(x) {
      web_image(url = x,
                height = px(32))
    }
  ) %>% 
  cols_merge(
    columns = c(PLAYER, RS)
  ) %>%
  text_transform(
    locations = cells_body(
      columns = c(PLAYER)
    ),
    fn = function(x){
      name <- word(x, 1, 2)
      team <- word(x, -1)
      glue::glue(
        "<div><span style='font-weight:bold;font-variant:small-
caps;font-size:18px'>{name}</div>
           <div style='line-height:17px'><span style ='font-
weight:bold;color:grey;font-size:11px'>{team}</div>"
      ) }
  ) %>%
  data_color(
    columns = c(STKS),
    colors = scales::col_numeric(
      palette = c("#A1D99B", "#00441B"),  # Dark green to light green
      domain = c(min(fin$STKS), max(fin$STKS)),  # Use actual range of values
      na.color = "black"
    )
  ) %>%
  cols_align(
    align = "center",
    columns = c(logo, STKS, STL, BLK, MIN)
  ) %>%
  # cols_align(
  #   align = "left",
  #   columns = c(ATT_rnk, FGpct_rnk, MPG_rnk)
  # ) %>% 
  cols_width(
    STKS ~ px(90), 
    c(GP, MIN) ~ px(50), 
    c(STL, BLK) ~ px(70)
    ) %>%
  tab_options(
    column_labels.font.size = 14,
    table.font.size = 15,
    heading.title.font.size  = 30,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 15,
    table.font.names = "Proxima Nova",
    table.font.color = 'black',
    table.border.top.color = "transparent",
    data_row.padding = px(10),
    footnotes.font.size = 8,
    source_notes.font.size = 15,
    footnotes.padding = px(1),
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold", size = 35),
    locations = cells_body(columns = c(STKS))
  ) %>% 
  # tab_style(
  #   style = cell_text(size = px(10.5)),
  #   locations = cells_body(columns = c(ATT_rnk, FGpct_rnk))
  # ) %>%
  tab_source_note(source_note = md(
    "Source: NBA | Table: Davide Tissino"
  )) %>% 
  opt_footnote_marks(marks = "letters") |>
  tab_style(
    style = list(
      cell_fill(color = "gray80"),
      cell_text('black')
    ),
    locations = cells_body(columns = STKS)
  ) %>% 
  gt_theme_tier('light')



  









