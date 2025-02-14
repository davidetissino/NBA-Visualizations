library(tidyverse)
library(rvest)
library(paletteer)
library(extrafont)
library(httr)
library(jsonlite)
library(purrr)
library(ggchicklet)
library(gt)
library(gtUtils)


# DESCRIPTION ####


# Code to scrape all RS data from 1996-97 
# Viz of players who made the most threes thru first 4 games of RS

# Chicklet Plot 



### LOAD CSV 
## all RS player box scores NBA History 
rs_logs <- read.csv('/Users/davidetissino/Desktop/R/data/RS logs.csv')

## Teams index 
tms <- read.csv('/Users/davidetissino/Desktop/R/data/teams.csv')

## Players index
players <- read.csv('/Users/davidetissino/Desktop/R/data/Players Index.csv')
colnames(players)[1] <- 'idPlayer'

players$headshot <- paste0('https://cdn.nba.com/headshots/nba/latest/260x190/', players$idPlayer, '.png')




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


# scrape RS box scores so far 
url <- 'https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&ISTRound=&LeagueID=00&PlayerOrTeam=P&Season=2024-25&SeasonType=Regular%20Season&Sorter=DATE'

res <- GET(url = url, add_headers(.headers = headers))

resp <- fromJSON(content(res, 'text'))

df <- data.frame(resp$resultSets$rowSet)

colnames(df) <- resp[['resultSets']][['headers']][[1]]



df$GAME_ID <- as.numeric(df$GAME_ID)
df$FG3M <- as.numeric(df$FG3M)



# summary df (same as scraping tot boxscore)
summ <- df %>%
  group_by(SEASON_ID, PLAYER_ID, PLAYER_NAME, TEAM_ABBREVIATION) %>%
  summarise(
    GP = n_distinct(GAME_ID),  # Count unique games for each player
    TP = sum(FG3M, na.rm = TRUE)  # Sum of threes made, ignoring NA values
  ) %>%
  ungroup()  


# function to scrape all others
get_boxscores <- function(season) {
  
  url <- paste0('https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&ISTRound=&LeagueID=00&PlayerOrTeam=P&Season=', 
                season, 
                '&SeasonType=Regular%20Season&Sorter=DATE')
  
  res <- GET(url = url, add_headers(.headers = headers))
  resp <- fromJSON(content(res, 'text'))
  
  logs <- data.frame(resp$resultSets$rowSet)
  
  colnames(logs) <- resp[['resultSets']][['headers']][[1]]
  
  return(logs)
  
}



# to scrape use season = RS - 1 (eg for 2024-25 use 2024)
# creates vector of seasons since 1996 
seasons <- paste0(1996:2024)

# map all single dfs into one 
df <- map_df(seasons, get_boxscores)

# as numeric column 
df$FG3M <- as.numeric(df$FG3M)
df$FG3A <- as.numeric(df$FG3A)
df$PTS <- as.numeric(df$PTS)
df$MIN <- as.numeric(df$MIN)
df$FG3_PCT <- as.numeric(df$FG3_PCT)


## ALL SEASONS since '96 ----
# Filter each season's data to only the first four games for each player, then calculate total threes
top_threes <- df %>%
  group_by(SEASON_ID, PLAYER_NAME) %>%
  arrange(GAME_DATE) %>%
  slice_head(n = 4) %>%  # Keep only the first four games for each player in each season
  summarise(
    idPlayer = first(PLAYER_ID),
    slugTeam = first(TEAM_ABBREVIATION),
    Total_Threes = sum(FG3M, na.rm = TRUE),  # Sum up the three-pointers
    ATT_pg = round(mean(FG3A), 1),
    FG_pg = round(mean(FG3_PCT), 3) * 100,
    PPG = round(mean(PTS), 1),
    MIN = round(mean(MIN), 1)
  ) %>%
  ungroup() %>%
  arrange(desc(Total_Threes))  # Sort by total threes to get the ranking



# merge with teams info
merged_tms <- merge(only_20_threes, tms, by = 'slugTeam')

merged_tms$idPlayer <- as.numeric(merged_tms$idPlayer)


# merge with player index
df <- merge(merged_tms, players, by = 'idPlayer') %>% 
  mutate(
    season_start = as.integer(substr(SEASON_ID, 2, 5)),  # Extract the starting year
    season_formatted = paste0(season_start, "-", sprintf("%02d", (season_start + 1) %% 100))
  ) %>% 
  select(-SEASON_ID, -season_start) %>%   
  arrange(desc(Total_Threes)) %>% 
  .[, c(39, 3, 40, 10, 13, 4:8)]



# Calculate rankings for multiple statistics
ranked_df <- df %>%
  mutate(
    Threes_rnk = dense_rank(desc(Total_Threes)),
    MPG_rnk = dense_rank(desc(MIN)),
    ATT_rnk = dense_rank(desc(ATT_pg)),
    PPG_rnk = dense_rank(desc(PPG)), 
    FGpct_rnk = dense_rank(desc(FG_pg))
  ) %>% 
  .[, c(1:5, 6, 7, 13, 8, 15, 9, 14, 10, 12)]








  




ranked_df %>%
  arrange(desc(Total_Threes)) %>%
  gt()  %>%
  cols_hide(
    c(team, PPG, PPG_rnk, MIN, MPG_rnk)
  ) %>% 
  cols_label(headshot = "",
             team = '',
             PLAYER_NAME = "",
             season_formatted = "",
             logo = "Team",
             Total_Threes = 'Threes',
             ATT_pg = "3PA/G",
             ATT_rnk = "",
             FG_pg = "3P%",
             FGpct_rnk = "",
             PPG = "PPG", 
             PPG_rnk = '', 
             MIN = 'MIN', 
             MPG_rnk = '') %>%
  tab_header(
    title = md("**Hot Start from Three**"),
    subtitle = 'Only players in NBA History with 20+ threes through the first four games of the Regular Season'
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
    columns = c(PLAYER_NAME, season_formatted)
  ) %>%
  text_transform(
    locations = cells_body(
      columns = c(PLAYER_NAME)
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
    columns = c(Total_Threes),
    colors = scales::col_numeric(
      palette = c("#A1D99B", "#00441B"),  # Dark green to light green
      domain = c(min(ranked_df$Total_Threes), max(ranked_df$Total_Threes)),  # Use actual range of values
      na.color = "black"
    )
  ) %>%
  cols_align(
    align = "center",
    columns = c(logo, Total_Threes, ATT_pg, FG_pg, MIN)
  ) %>%
  cols_align(
    align = "left",
    columns = c(ATT_rnk, FGpct_rnk, MPG_rnk)
  ) %>% 
  cols_width(Total_Threes ~ px(90)) %>%
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
    source_notes.font.size = 9,
    footnotes.padding = px(1),
    
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold", size = 35),
    locations = cells_body(columns = c(Total_Threes))
  ) %>% 
  tab_style(
    style = cell_text(size = px(10.5)),
    locations = cells_body(columns = c(ATT_rnk, FGpct_rnk))
  ) %>% 
  tab_source_note(source_note = md(
    "Source: NBA | Table: Davide Tissino"
  )) %>% 
  opt_footnote_marks(marks = "letters") |>
  tab_style(
    style = list(
      cell_fill(color = "gray80"),
      cell_text('black')
    ),
    locations = cells_body(columns = Total_Threes)
    ) %>%
  gt_color_pills(ATT_rnk, fill_type = 'rank', rank_order = 'desc', digits = 0, pill_height = 15) %>% 
  gt_color_pills(FGpct_rnk, fill_type = 'rank', rank_order = 'desc', digits = 0, pill_height = 15) %>% 
  gt_theme_tier('light') 










# gt savant ----
gt_theme_savant <- function(gt_object, ...) {
  
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
              class(gt_object))
  
  table_id <- subset(gt_object[['_options']], parameter == 'table_id')$value[[1]]
  
  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }
  
  gt_object %>%
    # cell body
    gt::tab_style(locations = gt::cells_body(),
                  style = gt::cell_text(font = gt::google_font('Roboto Condensed'), size = px(14))) %>%
    # col. headers
    gt::tab_style(locations = gt::cells_column_labels(),
                  style = gt::cell_text(weight = 'bold', font = gt::google_font('Roboto Condensed'), size = px(14))) %>%
    # group rows
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(
          font = gt::google_font("Roboto Condensed"),
          weight = 650,
          size = px(14),
          color = "#FFFDF5"
        ),
        gt::cell_fill(
          color = "#000000"
        )
      )
    ) %>%
    # footnote
    gt::tab_style(locations = gt::cells_footnotes(),
                  style = gt::cell_text(font = gt::google_font('Roboto Condensed'), size = px(12))) %>%
    # title
    gt::tab_style(locations = gt::cells_title('title'),
                  style = gt::cell_text(weight = 'bold', font = gt::google_font('Roboto Condensed'), size = px(18))) %>%
    # subtitle
    gt::tab_style(locations = gt::cells_title('subtitle'),
                  style = gt::cell_text(font = gt::google_font('Roboto Condensed'), size = px(14))) %>%
    # caption
    gt::tab_style(locations = gt::cells_source_notes(),
                  style = gt::cell_text(font = gt::google_font('Roboto Condensed'), size = px(12))) %>%
    # spanner
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        font = gt::google_font("Roboto Condensed"),
        weight = 650,
        size = px(8)
      )
    ) %>%
    gt::tab_options(
      data_row.padding = 1,
      table_body.hlines.color = "transparent",
      column_labels.border.top.color = 'black',
      column_labels.border.top.width = px(1),
      column_labels.border.bottom.style = 'none',
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "black",
      row_group.border.bottom.style = 'solid',
      row_group.padding = px(1.5),
      heading.align = 'center',
      heading.border.bottom.style = "none",
      table_body.border.top.style = "none",
      table_body.border.bottom.color = "white",
      table.border.bottom.style = 'none',
      table.border.top.style = 'none',
      source_notes.border.lr.style = "none",
      ...
    ) %>%
    gt::opt_row_striping() %>%
    gt::opt_css(c(paste0("#", table_id, " tbody tr:last-child {border-bottom: 2px solid #ffffff00;}"),
                  paste0("#", table_id, " .gt_col_heading {padding-bottom: 2px; padding-top: 2px;}"),
                  paste0("#", table_id, " .gt_subtitle {padding-top:0px !important; padding-bottom: 4px !important;}"),
                  paste0("#", table_id, " .gt_heading {padding-bottom: 0px; padding-top: 6px;}"),
                  paste0("#", table_id, " .gt_column_spanner {font-size: 12px; font-weight: bold; text-decoration: underline;}")))
  
}





# gt athletic ----
gt_theme_athletic <- function(gt_object, ...) {
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
              class(gt_object))
  
  table_id <- subset(gt_object[["_options"]], parameter == "table_id")$value[[1]]
  
  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }
  
  table <- gt_object %>%
    gt::opt_table_font(
      font = list(
        gt::google_font("Spline Sans Mono"),
        gt::default_fonts()
      ),
      weight = 500
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(
        columns = gt::everything()
      ),
      style = gt::cell_text(
        font = gt::google_font("Work Sans"),
        weight = 650,
        size = px(12),
        transform = "uppercase"
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(
        font = gt::google_font("Work Sans"),
        weight = 650,
        size = px(22)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(
        font = gt::google_font("Work Sans"),
        weight = 500,
        size = px(14)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(
          weight = 650,
          size = px(12),
          color = "white"
        ),
        gt::cell_fill(
          color = "black"
        )
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "left", weight = px(0.5), color = "black"),
      locations = gt::cells_body(
        columns = c(-names(gt_object[["_data"]])[1])
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "top", color = "black", weight = px(1.5), style = "dotted"),
      locations = gt::cells_body(
        rows = gt::everything()
      )
    ) %>%
    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::tab_options(
      table.font.size = 12,
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      table_body.border.top.style = "none",
      heading.border.bottom.style = "none",
      heading.align = "left",
      heading.title.font.size = px(26),
      source_notes.border.lr.style = "none",
      source_notes.font.size = 10,
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "black",
      row_group.border.bottom.style = "solid",
      row_group.padding = px(1.5),
      ...
    ) %>%
    gt::opt_css(c(
      paste0(
        "#",
        table_id,
        " tbody tr:last-child {border-bottom: 2px solid #ffffff00;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_subtitle {padding-top:0px !important; padding-bottom: 4px !important;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_sourcenote {border-bottom-color: #FFFDF5 !important;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_heading {padding-bottom: 0px; padding-top: 6px;}"
      )
    ))
  
  return(table)
}






# gt utils ----
gt_theme_gtutils <- function(gt_object, ...) {
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
              class(gt_object))
  
  table_id <- subset(gt_object[["_options"]], parameter == "table_id")$value[[1]]
  
  if (is.na(table_id)) {
    table_id <- gt::random_id()
    opt_position <- which("table_id" %in% gt_object[["_options"]][["parameter"]])[[1]]
    gt_object[["_options"]][["value"]][[opt_position]] <- table_id
  }
  
  data <- gt_object[["_data"]]
  
  table <- gt_object %>%
    gt::opt_table_font(
      font = list(
        gt::google_font("Almarai"),
        gt::default_fonts()
      ),
      weight = 500
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("title"),
      style = gt::cell_text(
        font = gt::google_font("Signika Negative"),
        weight = 650
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_title("subtitle"),
      style = gt::cell_text(
        font = gt::google_font("Signika Negative"),
        weight = 500
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_labels(
        columns = gt::everything()
      ),
      style = gt::cell_text(
        font = gt::google_font("Signika Negative"),
        weight = 650,
        size = px(14)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_column_spanners(),
      style = gt::cell_text(
        font = gt::google_font("Signika Negative"),
        weight = 650,
        size = px(13)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(
          font = gt::google_font("Signika Negative"),
          weight = 650,
          size = px(14),
          color = "#FFFDF5"
        ),
        gt::cell_fill(
          color = "#8A817C"
        )
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(
        font = gt::google_font("Almarai"),
        size = px(12)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_footnotes(),
      style = gt::cell_text(
        font = gt::google_font("Almarai"),
        size = px(12)
      )
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(rows = 1:(nrow(data) - 1)),
      style = gt::cell_borders(sides = "bottom", color = "#8A817C")
    ) %>%
    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::tab_options(
      data_row.padding = 1,
      table_body.hlines.color = "transparent",
      # column_labels.border.top.style = 'solid',
      # column_labels.border.top.color = '#ffffff',
      # column_labels.border.top.width = px(0.5),
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "black",
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "black",
      row_group.border.bottom.style = "solid",
      row_group.padding = px(1.5),
      heading.align = "left",
      heading.border.bottom.style = "none",
      table_body.border.top.style = "none",
      table.border.bottom.style = "none",
      table.border.top.style = "none",
      source_notes.border.lr.style = "none",
      table.background.color = "#FFFDF5",
      table.border.top.color = "#FFFDF5",
      table.border.right.color = "#FFFDF5",
      table.border.bottom.color = "#FFFDF5",
      table.border.left.color = "#FFFDF5",
      ...
    ) %>%
    gt::opt_css(c(
      paste0(
        "#",
        table_id,
        " tbody tr:last-child {border-bottom: 2px solid #FFFDF5;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_col_heading {padding-bottom: 2px; padding-top: 2px;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_subtitle {padding-top:0px !important; padding-bottom: 4px !important;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_sourcenote {border-bottom-color: #FFFDF5 !important;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_heading {padding-bottom: 0px; padding-top: 6px;}"
      ),
      paste0(
        "#",
        table_id,
        " .gt_column_spanner {padding-bottom: 2px;}"
      )
    ))
  
  return(table)
  
}