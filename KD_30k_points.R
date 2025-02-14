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
library(ggtext)
library(geomtextpath)
library(stringi)


## Create various graphs for career points overview of specific player
## Graph 1: YoY PTS scored progression, divided in 2pt, 3pt and ft


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


# Players index ====
# make player name plain text, no accents 
players <- read_csv('/Users/davidetissino/Desktop/R/data/Active Index.csv')

players$Player <- stri_trans_general(players$Player, 'Latin-ASCII')


# Teams index ====
tms <- read_csv('/Users/davidetissino/Desktop/R/data/teams.csv')




##### ============================================================================= ###
#GRAPH 1 ####


# Setup: set player name 
input_player <- ''



# derive player index data
player_index <- players %>% 
  filter(
    Player == input_player
  )



## Scrape Career Stats ====
url <- 'https://stats.nba.com/stats/playercareerstats?LeagueID=00&PerMode=Totals&PlayerID=202691'

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))


#### Regular Season Year by Year ----
RS_yoy <- data.frame(json_resp$resultSets$rowSet[1])
colnames(RS_yoy) <- json_resp[["resultSets"]][["headers"]][[1]]  




# filter for KD specific infos 
kd_profile <- players %>% 
  filter(
    Player ==
      'Kevin Durant'
  )


kd_teams <- tms %>% 
  filter(
    slugTeam %in% c('PHX', 'OKC', 'GSW', 'BKN')
  )







###### RS YoY ====

# Create df with PTS scored overview (2pt, 3pt, ft)

RS_yearly <- RS_yoy %>% 
  mutate_at(
    c(10, 13), 
    as.numeric
  ) %>% 
  mutate(
    FG2M = FGM - FG3M
  ) %>%
  select(
    SEASON_ID, 
    FG2M, 
    FTM, 
    FG3M,
    PTS) %>%
  mutate(
    FG2PTs = as.numeric(FG2M * 2),
    FG3PTs = FG3M * 3,
    FTPTs = as.numeric(FTM)
  ) %>%
  select(
    SEASON_ID, 
    FG2PTs, 
    FG3PTs, 
    FTPTs, 
    PTS) %>% 
  pivot_longer(
    c("FG2PTs","FTPTs","FG3PTs"), 
    names_to="shot_type", 
    values_to="shot_points") %>%
  mutate(
    shot_type = factor(shot_type, levels=c("FG2PTs","FTPTs","FG3PTs")), 
    shot_lab = case_when(
      shot_type == "FG2PTs" ~ "Points From 2 PT Field Goal",
      shot_type == "FG3PTs" ~ "Points From 3 PT Field Goal",
      shot_type == "FTPTs" ~  "Points From Free Throw")
  )



# Plot all PTS YoY with three lines (2pt, 3pt, ft)
ggplot(data = RS_yearly, aes(x=SEASON_ID, y=shot_points, group=shot_type))+
  geom_segment(
    aes(
      x=SEASON_ID, 
      xend=SEASON_ID,
      y=0, 
      yend=max(shot_points)+10, 
      color='black'
    ), 
    linewidth=.7, 
    linetype="dotted", 
    alpha=.3)+
  geom_textbox(
    aes(
      y=max(shot_points)+150,
      label = 'SAMOS',
      color='black'), 
    box.color=NA, 
    fill=NA, 
    hjust=.5, 
    halign=.5, 
    lineheight=.8, 
    alpha=.2) +
  geom_textbox(
    data = RS_yearly %>% 
      filter(shot_points == max(shot_points)), 
    aes(y=max(shot_points)+150,
        label = 'SUS',
        color='black'), 
    box.color=NA, 
    fill=NA, 
    hjust=.5, 
    halign=.5, 
    lineheight=.8)+
  geom_area(
    aes(fill=shot_type),
    linewidth=1.5,
    alpha=.2, 
    position="identity") +
  scale_fill_manual(values=c("#e63946","#ee9b00","#00afb9","#f35345" ))+
  theme_minimal()+
  scale_color_identity()+
  geom_textline(aes(label=shot_lab, color=shot_type), family="Open Sans", fontface=2, linewidth=1.5, hjust=.2,text_smoothing = 50, size=5)+
  scale_color_manual(values=c("#e63946","#ee9b00","#00afb9", "#f35345"))+
  theme(legend.position="none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(color="#A2A2A2", face="bold"))+
  coord_cartesian(clip="off")



