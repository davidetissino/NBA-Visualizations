library(janitor)
library(hablar)
library(jsonlite)
library(httr)
library(ggtext)
library(ggplot2)
library(ggprism)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(nbastatR)
library(ggpubr)


# csv with team names, slugs, colors 
tms <- read_csv('/Users/davidetissino/Desktop/R/data/teams.csv')


# increase buffer size for scraping
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)


yesterday <- Sys.Date() - 1


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


po_logs <- po_logs %>%
  filter(GAME_DATE == yesterday)


game_ids <- unique(po_logs$GAME_ID)



for (game_id in game_ids) {
  
  #### DESCRIPTION 
  ## Code to plot two graphs: 
  # 1) Players' playing stints over game, with point differential in each stint 
  # 2) Game scoring margin, as away team points - home team points 
  # Set only GAME ID, rest automatically done when run 
  # May need to modify graph 2 margins to fit graph 1
  
  
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
  
  ## SCRAPE GAME INFO ####
  url <- paste0('https://stats.nba.com/stats/gamerotation?GameID=', game_id)
  
  res <- GET(url = url, add_headers(.headers = headers))
  
  json_resp <- fromJSON(content(res, "text"))
  
  # two tabs, home & away
  home <- data.frame(json_resp[["resultSets"]][["rowSet"]][[2]])
  away <- data.frame(json_resp[["resultSets"]][["rowSet"]][[1]])
  
  colnames(home) <- json_resp[["resultSets"]][["headers"]][[1]]
  colnames(away) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  home <- home %>%
    clean_names() %>%
    retype()
  
  away <- away %>%
    clean_names() %>%
    retype()
  
  # one player column 
  home$player <- paste0(home$player_first, ' ', home$player_last)
  away$player <- paste0(away$player_first, ' ', away$player_last)
  
  # add location column 
  home$location <- 'Home'
  away$location <- 'Away'
  
  # remove & clean some columns
  home <- home[, c(14, 13, 8:12, 1:7)]
  away <- away[, c(14, 13, 8:12, 1:7)]
  
  ### merge into one #
  df <- rbind(home, away)
  
  
  ##### Time Formatting ====
  # time in strange format, function to convert to correct format
  convert_to_time_string <- function(seconds) {
    time_strings <- sprintf("%02d:%02d", as.integer(seconds) %/% 60, as.integer(seconds) %% 60)
    return(time_strings)
  }
  
  # Correct format to in & out time home & away
  df$in_time_real <- lapply(df$in_time_real / 10, convert_to_time_string)
  df$out_time_real <- lapply(df$out_time_real / 10, convert_to_time_string)
  
  # epoch to convert to UNIX time
  date <- as.Date("2000-01-01")
  
  # Convert "mm:ss" to Unix time (seconds since Unix Epoch)
  df$in_unix_time <- as.integer(as.POSIXct(paste(date, df$in_time_real), format = "%Y-%m-%d %M:%S"))
  df$out_unix_time <- as.integer(as.POSIXct(paste(date, df$out_time_real), format = "%Y-%m-%d %M:%S"))
  
  
  # remove some columns
  df <- df[, -c(7, 9:14)]
  
  
  ##### Playoff Logs Scrape ====
  url <- 'https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&ISTRound=&LeagueID=00&PlayerOrTeam=P&Season=2023-24&SeasonType=Playoffs&Sorter=DATE'
  
  res <- GET(url = url, add_headers(.headers = headers))
  
  json_resp <- fromJSON(content(res, "text"))
  
  po_logs <- data.frame(json_resp[["resultSets"]][["rowSet"]][[1]])
  
  colnames(po_logs) <- json_resp[["resultSets"]][["headers"]][[1]]
  
  # filter for specific GAME ID
  po_logs <- po_logs %>% 
    filter(GAME_ID == game_id)
  
  # change column name to merge 
  colnames(po_logs)[3] <- 'player'
  
  # numeric PTS column for after
  po_logs$PTS <- as.numeric(po_logs$PTS)
  
  
  
  # MERGE into one df
  final <- merge(df, po_logs, by = 'player')
  
  # remove unnecessary columns
  final <- final[, -c(10:12, 14,15, 17, 20:36, 39, 40)]
  
  # rearrange columns order
  final <- final[, c(7, 11, 10, 2, 12, 1, 13:15, 3, 4, 6, 8, 9, 5)]
  
  final$MIN <- as.numeric(final$MIN)
  final$PTS <- as.numeric(final$PTS)
  final$PLUS_MINUS <- as.numeric(final$PLUS_MINUS)
  
  final <- final %>% 
    arrange(location, desc(-MIN)) %>%
    arrange(factor(location, levels = c("Home", "Away")))
  
  
  # abbreviate the name to initial . 
  final$short_player <- gsub("(\\w)\\w* ", "\\1. ", final$player)
  
  
  
  #### Players NAMES -----
  final$short_player[final$player == 'Karl-Anthony Towns'] <- 'K.A. Towns'
  final$short_player[final$player == 'Michael Porter Jr.'] <- 'M. Porter Jr.'
  final$short_player[final$player == 'Wendell Moore Jr.'] <- 'W. Moore Jr.'
  # final$short_player[final$player == ''] <- ''
  
  
  
  # add a + sign if plus/minus > 0
  final <- final %>%
    mutate(
      PLUS_MINUS = ifelse(PLUS_MINUS >= 0, paste("+ ", PLUS_MINUS, sep = ""), paste('- ', abs(PLUS_MINUS), sep = '')))
  
  
  # merge shortened name with clean plus/minus
  final$axis <- paste0(final$short_player, ' (', final$MIN , ' min, ' , final$PLUS_MINUS, ')')
  
  
  # Add a unique identifier for each player
  players <- unique(final$player)
  player_ids <- seq_along(players)
  player_mapping <- data.frame(player = players, player_id = player_ids)
  
  
  df_end <- merge(final, player_mapping, by = "player")
  
  
  # Define the coordinates of rectangles
  df_end$ymin <- df_end$player_id - 0.35
  df_end$ymax <- df_end$player_id + 0.35
  
  
  
  
  ## Teams Colors ====
  playing_teams <- data.frame(
    slugTeam = unique(df_end$TEAM_ABBREVIATION),
    loc = unique(df_end$location)
  )
  
  teams <- merge(playing_teams, tms, by = 'slugTeam')
  
  
  # Game Date
  game_date <- unique(df_end$GAME_DATE) %>% 
    as.Date() %>% 
    format(., "%B %d, %Y")
  
  
  ## Away & Home Team Name 
  away_team <- teams$team[teams$loc == 'Away']
  home_team <- teams$team[teams$loc == 'Home']
  
  
  ## Away & Home slugs
  away_slug <- teams$slugTeam[teams$loc == 'Away']
  home_slug <- teams$slugTeam[teams$loc == 'Home']
  
  ## Away & Home Team Points
  away_points <- sum(po_logs$PTS[po_logs$TEAM_NAME == away_team])
  home_points <- sum(po_logs$PTS[po_logs$TEAM_NAME == home_team])
  
  
  
  
  # ROTATION PLOT ####
  
  
  rotation <- df_end %>%
    ggplot(aes(ymin = ymin, ymax = ymax, xmin = in_unix_time, xmax = out_unix_time, fill = pt_diff)) +
    geom_rect(color = 'black') +
    geom_label(
      size = 5,
      color = 'black', 
      fill = 'white',
      fontface = 'bold',
      aes(
        y = (ymin + ymax) / 2, 
        x = (out_unix_time + in_unix_time) / 2,
        label = pt_diff)
    ) +
    scale_x_continuous(breaks = c(946681200, 946681920, 946682640, 946683360, 946684080),
                       labels = c("", "", "", "", ""), 
                       guide = 'prism_minor', 
                       limits = c(946681200, 946684080),
                       minor_breaks = seq(946681200, 946684080, 60)
    ) +
    scale_y_continuous(breaks = df_end$player_id, labels = df_end$axis) +
    scale_fill_gradient2(low = 'firebrick3', mid = 'floralwhite', high = 'forestgreen') +
    facet_grid(
      TEAM_ABBREVIATION ~ ., 
      scales = "free_y", 
      space = "free_y"
    ) +
    labs(
      x = "", 
      y = "", 
      
      title = paste(
        away_team, '-', away_points, '@', home_points, '-', home_team
      ),
      
      subtitle = paste0(game_date)
    ) + 
    theme_minimal() +
    coord_cartesian(xlim = c(946681328, 946683952)) +
    theme(
      text = element_text(family='avenir', color = 'black'), 
      axis.title.x = element_text(face='bold', size=23, margin=margin(t=7)),
      axis.text.y = element_text(margin = margin(r = 5, unit = "pt"), hjust = 0.95, color = 'black', size = 15), 
      axis.text.x = element_blank(),
      axis.ticks.x = element_line(color = 'black'),
      
      plot.background = element_rect(color = 'white'),
      plot.margin = margin(.5, .5, 0, .5, "cm"), 
      plot.caption = element_text(color = 'gray40', size = 10),
      plot.title = element_text(face='bold', size=30, hjust = 0.1, margin = margin(t = 10)),
      plot.subtitle=element_text(size=17, hjust = 0.4, margin = margin(t = 10, b = 15)), 
      
      panel.grid = element_line(color = "#afa9a9"),
      panel.grid.major.y = element_blank(), 
      panel.grid.minor.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect('white'), 
      panel.border = element_rect(colour = 'black', fill = NA, size = 1), 
      
      panel.spacing = unit(1, 'lines'),
      
      legend.position = "right",  # Position the legend at the top of the plot
      legend.title = element_text(size = 15, face = 'bold', angle = -90),  # Set the legend title
      legend.box = 'vertical',
      legend.background = element_rect(fill = 'floralwhite', color = 'black'),
      legend.margin = margin(15,5,15,5),
      
      strip.text = element_text(face = 'bold', size = 15), 
      strip.background = element_rect(color = 'black', fill = 'floralwhite', size = 1.3)
      
    ) +
    guides(
      fill = guide_colorbar(
        barwidth = 1.5, 
        barheight= 15, 
        title = "Point differential in stint", ## LEGEND TITLE
        title.position = "right", 
        title.hjust = 0.5,
        label.theme = element_text(size = 14), 
        hjust = 0.3
      )
    )
  
  rotation
  
  
  
  # ggsave(paste0('/Users/davidetissino/Desktop/rotation', game_id, '.png'), dpi = 'retina', height = 6, width = 10)
  
  
  
  
  
  
  #### PBP DATA - MARGIN DIFF####
  
  library(nbastatR)
  
  pbp <- play_by_play(game_ids = game_id)
  
  pbp <- pbp[, - c(1, 4:12, 14:16)]
  
  # new column with point differential (margin)
  pbp$margin <- pbp$scoreAway - pbp$scoreHome
  
  
  game_start <- pbp[-c(2:nrow(pbp)), ]
  
  
  # remove all columns containing NAs
  pbp <- pbp %>%
    drop_na()
  
  
  pbp <- rbind(game_start, pbp)
  
  pbp$margin[pbp$minuteGame == 0.0000000] <- 0
  
  
  
  # create column with seconds
  pbp$seconds <- pbp$minuteGame * 60
  
  
  
  ##### Time Formatting 2 ====
  convert_seconds_to_time_string <- function(seconds) {
    
    # Extract minutes and seconds
    minutes <- floor(seconds / 60)
    seconds <- seconds %% 60
    
    # Format the time string
    time_string <- sprintf("%02d:%02d", as.integer(minutes), as.integer(seconds))
    
    return(time_string)
  }
  
  # MM:SS time format to new column
  pbp$MIN <- sapply(pbp$seconds, convert_seconds_to_time_string)
  
  # unix time format from epoch date
  pbp$unix_time <- as.integer(as.POSIXct(paste(date, pbp$MIN), format = "%Y-%m-%d %M:%S"))
  
  
  ##### Evaluations to Plot ====
  # variable for margin = 0
  t <- 0  
  
  # take only relevant columns
  fin <- pbp %>% 
    select(unix_time, margin) 
  
  # identify points completely > or < 0 
  fin <- fin %>% 
    arrange(unix_time) %>%
    mutate(
      above_t = margin >= t
    ) %>% 
    mutate(
      changed = is.na(lag(above_t)) | lag(above_t) != above_t
    ) %>% 
    mutate(
      section_id = cumsum(changed)
    ) %>% 
    select(- above_t, - changed)
  
  # calculate the x-coordinate of the intersection point with 0 
  # (the y-coordinate would be t), & add this to the data frame
  fin <- rbind(
    fin, 
    fin %>% 
      group_by(section_id) %>% 
      filter(unix_time %in% c(min(unix_time), max(unix_time))) %>% 
      ungroup() %>% 
      mutate(
        mid_unix = ifelse(
          section_id == 1 | section_id == lag(section_id), 
          NA, 
          unix_time - (unix_time - lag(unix_time)) / 
            (margin - lag(margin)) * (margin - t))) %>% 
      select(
        mid_unix, margin, section_id
      ) %>% 
      rename(
        unix_time = mid_unix
      ) %>%
      mutate(margin = t) %>% 
      na.omit())
  
  
  end_margin <- max(fin$margin)
  
  
  
  # GAME MARGIN PLOT ####
  margin <- fin %>%
    ggplot(aes(x = unix_time, y = margin)) +
    # home & away slugs in differential 
    geom_text(
      x = 946681270, 
      y = -end_margin + 3, 
      aes(
        label = home_slug, 
        size = 8, 
        fontface = 'bold'
      )
    ) + 
    geom_text(
      x = 946681270, 
      y = end_margin - 3, 
      aes(
        label = away_slug, 
        size = 8, 
        fontface = 'bold'
      )
    ) + 
    # geom_image(x = 946681300, y = -18, aes(image = teams$logo[teams$loc == 'Home']), size = 0.25) +
    # geom_image(x = 946681300, y = 18, aes(image = teams$logo[teams$loc == 'Away']), size = 0.25) +
    # ribbons to fill areas between 0 and margin
    geom_ribbon(
      data = . %>% filter(margin >= 0),
      aes(ymin = 0, ymax = margin),
      fill = teams$secondary[teams$loc == 'Away'],
      color = teams$primary[teams$loc == 'Away'], 
      size = 1.5
    ) +
    geom_ribbon(
      data = . %>% filter(margin <= 0),
      aes(ymin = margin, ymax = 0),
      fill = teams$secondary[teams$loc == 'Home'], 
      color = teams$primary[teams$loc == 'Home'], 
      size = 1.5
    ) +
    #horizontal line on 0
    geom_hline(
      yintercept = 0,
      color = 'grey50',
      size = 1.5
    ) +
    geom_vline(
      xintercept = c(946681920, 946682640, 946683360), 
      color = 'grey80',
      size = .5
    ) +
    scale_y_continuous(
      breaks = c(-15,0, 15),
      labels = c("-15","0","+15"), 
    ) +
    scale_x_continuous(
      breaks = c(946681200, 946681920, 946682640, 946683360, 946684080),
      labels = c("0", "12", "24", "36", "48"),
      guide = 'prism_minor',
      limits = c(946681200, 946684080),
      minor_breaks = seq(946681200, 946684080, 60)
    ) +
    coord_cartesian(
      xlim = c(946681328, 946683952),
      ylim = c(-end_margin, end_margin)
    ) +
    theme(
      panel.border = element_rect(colour = 'black', fill = NA, size = 1), 
      panel.background = element_rect('white')
    ) + 
    labs(
      x = 'game minute',
      y = paste0('point differential \n', '(', away_slug, ' - ', home_slug, ')'), 
      title = '', 
      caption = '@dvdtssn | stats.nba.com', 
    ) + 
    theme(
      text = element_text(family='avenir', color = 'black'), 
      axis.title.x = element_text(face='bold', size=15, margin=margin(t=10)),
      axis.title.y = element_text(margin = margin(r = 5), angle = 360, vjust = 0.5, size = 18, face = 'bold'),
      axis.text.y = element_text(margin = margin(r = 5, unit = "pt"), face = 'bold', hjust = 0.95, color = 'black', size = 13), 
      axis.text.x = element_text(face='bold', color = 'black', size = 15),
      axis.ticks.x = element_line(color = 'black'),
      
      plot.background = element_rect('white'),
      plot.margin = margin(0, 130, 30, 104, "pt"), 
      plot.caption = element_text(color = 'black', size = 15),
      plot.title = element_text(face='bold', size=17, hjust = 0.5),
      plot.subtitle=element_text(size=13, hjust = 0.5), 
      
      panel.grid = element_line(color = "#afa9a9"),
      panel.grid.major.y = element_blank(), 
      panel.grid.minor.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect('white'), 
      panel.border = element_rect(colour = 'black', fill = NA, size = 1), 
      
      legend.position = 'none'
    )
  
margin  
  
  
  # ggsave(paste0('/Users/davidetissino/Desktop/margin', game_id, '.png'), dpi = 'retina', height = 6, width = 10)
  
  
  
  ## COMBINED PLOT ####
  
  ggarrange(
    rotation, 
    margin, 
    ncol = 1, 
    nrow = 2,
    align = 'hv', 
    heights = c(3, 1)
  )
  
  
ggsave(paste0('/Users/davidetissino/Desktop/s', away_slug, '@', home_slug, game_id, '.png'), dpi = 'retina', height = 11, width = 17)
  

