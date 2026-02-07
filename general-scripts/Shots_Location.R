library(ggthemes)
library(extrafont)
library(tidyverse)
library(nbastatR)
library(jsonlite)
library(httr)
library(prismatic)
library(ggimage)
library(tictoc)
library(sportyR)

## Shots Location 
## Plots shots made and missed from both away & home team given game ID
## plots on full basketball court


# buffer size
Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 10)


## CUSTOM THEME ####
theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='avenir'), 
      panel.background = element_rect('floralwhite'), 
      plot.background = element_rect('floralwhite'),
      plot.title = element_text(size=22, hjust=.5, vjust = -3, face = 'bold'),
      plot.subtitle = element_text(size=12, vjust=-8.5, face = 'italic'), 
      plot.margin = margin(0, 0, 0, 0, "cm"), 
      panel.grid.major = element_line(color='floralwhite')
    ) 
}

# team colors and infos 
tms <- read_csv('/Users/davidetissino/Desktop/R/data/teams.csv')

# team names 
names <- unique(tms$team)


# GAME ID #### 
game_id <- '0042300226'


#### Scrape PLAYOFF Logs ====
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

url <- 'https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&ISTRound=&LeagueID=00&PlayerOrTeam=P&Season=2023-24&SeasonType=Playoffs&Sorter=DATE'

res <- GET(url = url, add_headers(.headers = headers))

json_resp <- fromJSON(content(res, "text"))

po_logs <- data.frame(json_resp[["resultSets"]][["rowSet"]][[1]])

colnames(po_logs) <- json_resp[["resultSets"]][["headers"]][[1]]

# filter for specific GAME ID
po_logs <- po_logs %>% 
  filter(GAME_ID == game_id)

# mutate some columns to numeric
po_logs <- po_logs %>%
  mutate_at(
    c(11:30),
    as.numeric
  )





#### Scrape SHOTS Data ====
tic()
po_shots <- teams_shots(season_types = 'Playoffs', seasons = 2024, teams = names)
toc()

# add two 0s before game id 
po_shots$idGame <- paste0('00', po_shots$idGame)


# SINGLE GAME SPECIFIC ####
# get data only for that game 
df <- po_shots %>% 
  filter(idGame == game_id)

# resize to fit in court 
df$locationX <- df$locationX / 10
df$locationY <- df$locationY / 10 + 5.25


# merge with teams color df 
shots <- merge(df, tms, by = 'idTeam')




## Location Values ----
### coordinates scraped are given for single half court
### need to tweak location values to plot HOME on the right & AWAY on the left 
# Home : locY = - locationX , locX = 47 - locationY 
# Away : locY = locationX , locX = locationY - 47
shots$locY <- ifelse(
  shots$slugTeam == shots$slugTeamHome,
  -1 * shots$locationX,
  shots$locationX
)

shots$locX <- ifelse(
  shots$slugTeam == shots$slugTeamHome,
  47 - shots$locationY,
  shots$locationY - 47
)


## VARIOUS INFOS ####

### Home & Away Slugs ====
home_slug <- unique(shots$slugTeam[shots$slugTeam == shots$slugTeamHome])
away_slug <- unique(shots$slugTeam[shots$slugTeam == shots$slugTeamAway])

home_team <- unique(shots$nameTeam[shots$slugTeam == shots$slugTeamHome])
away_team <- unique(shots$nameTeam[shots$slugTeam == shots$slugTeamAway])

### Points ==== 
away_points <- sum(po_logs$PTS[po_logs$TEAM_NAME == away_team])
home_points <- sum(po_logs$PTS[po_logs$TEAM_NAME == home_team])

### Colors ====
home_primary <- unique(shots$primary[shots$slugTeam == shots$slugTeamHome])
home_secondary <- unique(shots$secondary[shots$slugTeam == shots$slugTeamHome])

away_primary <- unique(shots$primary[shots$slugTeam == shots$slugTeamAway])
away_secondary <- unique(shots$secondary[shots$slugTeam == shots$slugTeamAway])

### Home Logo ==== 
home_logo <- unique(shots$logo[shots$slugTeam == shots$slugTeamHome])

### Long Shots ====
## max distance shot for home & away team
# max_home_dist <- max(shots$distanceShot[shots$slugTeam == shots$slugTeamHome])
# max_home_dist_label <- paste(max_home_dist, 'ft')
# max_home_dist_X <- shots$locX[shots$distanceShot == max_home_dist][[1]]
# max_home_dist_Y <- shots$locY[shots$distanceShot == max_home_dist][[1]]

max_away_dist <- max(shots$distanceShot[shots$slugTeam == shots$slugTeamAway & shots$isShotMade == TRUE])
max_away_dist_label <- paste(max_away_dist, 'ft')
max_away_dist_X <- shots$locX[shots$distanceShot == max_away_dist][[1]]
max_away_dist_Y <- shots$locY[shots$distanceShot == max_away_dist][[1]]


### Shooting Stats ====
#### Home ----
home_FGM <- sum(po_logs$FGM[po_logs$TEAM_ABBREVIATION == home_slug])
home_FGA <- sum(po_logs$FGA[po_logs$TEAM_ABBREVIATION == home_slug])
home_FGp <- round(home_FGM / home_FGA * 100, digits = 1)

home_P3M <- sum(po_logs$FG3M[po_logs$TEAM_ABBREVIATION == home_slug])
home_P3A <- sum(po_logs$FG3A[po_logs$TEAM_ABBREVIATION == home_slug])
home_P3p <- round(home_P3M / home_P3A * 100, digits = 1)

home_FTM <- sum(po_logs$FTM[po_logs$TEAM_ABBREVIATION == home_slug])
home_FTA <- sum(po_logs$FTA[po_logs$TEAM_ABBREVIATION == home_slug])
home_FTp <- round(home_FTM / home_FTA * 100, digits = 1)

#### Away ----
away_FGM <- sum(po_logs$FGM[po_logs$TEAM_ABBREVIATION == away_slug])
away_FGA <- sum(po_logs$FGA[po_logs$TEAM_ABBREVIATION == away_slug])
away_FGp <- round(away_FGM / away_FGA * 100, digits = 1)

away_P3M <- sum(po_logs$FG3M[po_logs$TEAM_ABBREVIATION == away_slug])
away_P3A <- sum(po_logs$FG3A[po_logs$TEAM_ABBREVIATION == away_slug])
away_P3p <- round(away_P3M / away_P3A * 100, digits = 1)

away_FTM <- sum(po_logs$FTM[po_logs$TEAM_ABBREVIATION == away_slug])
away_FTA <- sum(po_logs$FTA[po_logs$TEAM_ABBREVIATION == away_slug])
away_FTp <- round(away_FTM / away_FTA * 100, digits = 1)







# Game Date ----
game_date <- unique(po_logs$GAME_DATE) %>% 
  as.Date() %>% 
  format(., "%B %d, %Y")


'#8B7765'

## PLOT ####
geom_basketball(
  'nba', 
  color_updates = list(
    plot_background = "white",
    defensive_half_court = "#CDB38B",
    offensive_half_court = "#CDB38B",
    court_apron = "#CDB38B",
    center_circle_outline = "#595959",
    center_circle_fill = "#CDB38B",
    division_line = "#595959",
    endline = "#595959",
    sideline = "#595959",
    two_point_range = "#CDB38B",
    three_point_line = "#595959",
    painted_area = "#CDB38B",
    lane_boundary = "#595959",
    free_throw_circle_outline = "#595959",
    free_throw_circle_fill = "#CDB38B",
    free_throw_circle_dash = "#595959",
    lane_space_mark = "#595959",
    inbounding_line = "#595959",
    substitution_line = "#595959",
    baseline_lower_defensive_box = "#595959",
    lane_lower_defensive_box = "#595959",
    team_bench_line = "#595959",
    restricted_arc = "#595959",
    backboard = "#595959",
    basket_ring = "#f55b33",
    net = "#ffffff"
    )
  ) + 
  # home shots missed
  geom_point(
    data = shots %>% 
      filter(
        slugTeam == slugTeamHome,
        isShotMade == F
      ), 
    aes(
      x = locX, 
      y = locY
    ), 
    size = 1.8,
    shape = 21, 
    stroke = 1.8,
    color = home_primary, 
    fill = 'white'
  ) +
  # home shots made 
  geom_point(
    data = shots %>% 
      filter(
        slugTeam == slugTeamHome,
        isShotMade == T
      ), 
    aes(
      x = locX, 
      y = locY
    ), 
    size = 3.5,
    shape = 21, 
    stroke = 1.2,
    color = 'white', 
    fill = home_primary
  ) + 
  
  # away shots missed
  geom_point(
    data = shots %>% 
      filter(
        slugTeam == slugTeamAway,
        isShotMade == F
      ), 
    aes(
      x = locX, 
      y = locY
    ), 
    size = 1.8,
    shape = 21, 
    stroke = 1.8,
    color = away_primary, 
    fill = 'white'
  ) +
  # away shots made 
  geom_point(
    data = shots %>% 
      filter(
        slugTeam == slugTeamAway,
        isShotMade == T
      ), 
    aes(
      x = locX, 
      y = locY
    ), 
    size = 3.5,
    shape = 21, 
    stroke = 1.2,
    color = 'white', 
    fill = away_primary
  ) + 
  geom_image(
    x = 0, 
    y = 0,
    aes(
      image = home_logo
    ), 
    size = 0.15
  ) + 
  #annotation on max home distance shot
  # geom_label(
  #   size = 3,
  #   fontface = 'bold',
  #   aes(
  #     x = max_home_dist_X - 2.8,
  #     y = max_home_dist_Y + 1.5,
  #     label = max_home_dist_label
  #   )
  # ) +
  geom_label(
    size = 3,
    fontface = 'bold',
    aes(
      x = max_away_dist_X + 2.5,
      y = max_away_dist_Y - 1.5,
      label = max_away_dist_label
    )
  ) +
  geom_label(
    size = 4,
    fontface = 'bold',
    aes(
      x = c(-24, 24),
      y = 27.5,
      label = c(away_slug, home_slug)
    ), 
    color = 'black',
    fill = 'white'
  ) + 
  # shot made 
  geom_point(
    aes(
      x = -6, 
      y = -27.5
    ), 
    shape = 21,
    size = 3.5, 
    stroke = 1.2,
    color = 'white',
    fill = home_primary
  ) + 
  geom_label(
    size = 3,
    fontface = 'bold',
    aes(
      x = -2.5,
      y = -27.5,
      label = 'Made' 
    )
  ) + 
  # shot missed 
  geom_point(
    aes(
      x = 2, 
      y = -27.5
    ), 
    size = 2.5,
    shape = 21, 
    stroke = 1.8,
    color = home_primary, 
    fill = 'white'
  ) +
  geom_label(
    size = 3,
    fontface = 'bold',
    aes(
      x = 6,
      y = -27.5,
      label = 'Missed' 
    )
  ) +
  theme(
    text = element_text(family='PT Mono', color = 'black'), 
    
    plot.caption = element_text(color = 'gray40', size = 10, hjust = c(0.05,0.95)),
    
    plot.subtitle=element_text(size=13, hjust = 0.5, margin = margin(t = 5, b = 10)), 
    
    ##### Plot TITLE ====
    plot.title = element_text(face='bold', size=18, hjust = 0.5)
  ) + 
  labs(
    title = paste(
      away_team, '-', away_points, '@', home_points, '-', home_team
    ),
    
    ##### SERIES INFO ----  
    subtitle = paste0(game_date, ' - ', 'Game 6: DAL wins series 4 - 2'), 
    
    caption = c(
      
      ### Shooting Splits ====
      paste0(
      
      ###### Home ----
      home_slug, ': FG ', home_FGM, '/', home_FGA, ' (', home_FGp, '%), ', 
      '3P ', home_P3M, '/', home_P3A, ' (', home_P3p, '%), ', 
      'FT ', home_FTM, '/', home_FTA, ' (', home_FTp, '%) \n', 
      
      ###### Away ----
      away_slug, ': FG ', away_FGM, '/', away_FGA, ' (', away_FGp, '%), ', 
      '3P ', away_P3M, '/', away_P3A, ' (', away_P3p, '%), ', 
      'FT ', away_FTM, '/', away_FTA, ' (', away_FTp, '%)'
        
      ), 
      '@dvdtssn | stats.nba.com')
  )






ggsave(paste0('/Users/davidetissino/Desktop/shots_', away_slug, '@', home_slug, '_', game_id, '.png'), dpi = 'retina', height = 7, width = 9)






