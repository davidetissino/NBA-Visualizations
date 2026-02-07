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
library(scales)
library(ggimage)


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
      text = element_text(family='Pt Mono'), 
      axis.title.x = element_text(color = 'black', margin = margin(t = 30, b = 8), family = 'K2D', face = 'bold', size = 19), 
      axis.title.y = element_text(color = 'black', margin = margin(r = 25, l = 8), family = 'K2D', face = 'bold', size = 19, angle = 90), 
      axis.text = element_text(color = 'grey30'),
      axis.text.x = element_text(face='bold', size = 13),
      axis.text.y = element_text(face='bold', size = 13), 
      panel.background = element_rect('grey98'), 
      plot.background = element_rect('grey98'),
      plot.title = element_text(margin = margin(b=15, t = 10), face='bold', size=30, hjust = 0, family = 'Proxima Nova'),
      plot.subtitle=element_text(size=15, hjust = 0, margin = margin(b = 10), family = 'Proxima Nova'), 
      panel.grid.major = element_line(color='gray80', linetype = 'dashed'),
      plot.margin = unit(c(0.5, 1, 0, 0.2), "inches"), 
    ) 
}


# Players and Teams ====
### Teams ----
tms <- read.csv('/Users/davidetissino/Desktop/R/data/teams.csv')

### Players ----
players <- read_csv('/Users/davidetissino/Desktop/R/data/Active Index.csv')

# convert to plain text to filter with player name
players$Player <- stri_trans_general(players$Player, 'Latin-ASCII')





##### ============================================================================= ###


### SET PLAYER NAME ####
input_player <- 'Kevin Durant'


##### ============================================================================= ###
#GRAPH 1 ####


# derive player index data
player_index <- players %>% 
  filter(
    Player == input_player
  )


# derive player ID 
player_ID <- players$idPlayer[players$Player == input_player]




## Scrape Career Stats ====

url <- paste0(
  'https://stats.nba.com/stats/playercareerstats?LeagueID=00&PerMode=Totals&PlayerID=', 
  player_ID
)

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))


#### Regular Season Year by Year ----
RS_yoy <- data.frame(json_resp$resultSets$rowSet[1])
colnames(RS_yoy) <- json_resp[["resultSets"]][["headers"]][[1]]  


# convert columns to numeric 
RS_yoy <- RS_yoy %>% 
  mutate_at(
    c(1, 4, 6:27), 
    as.numeric
  )




# modify for TOT rows (player traded)
RS_clean <- RS_yoy %>% 
  mutate(
    TEAM_ID = case_when(
      TEAM_ABBREVIATION == 'TOT' ~ lag(TEAM_ID), 
      TRUE ~ TEAM_ID
    ), 
    TEAM_ABBREVIATION = case_when(
      TEAM_ABBREVIATION == 'TOT' ~ lag(TEAM_ABBREVIATION), 
      TRUE ~ TEAM_ABBREVIATION
    )
  )


## TO MODIFY HERE 
RS_clean <- RS_clean[-c(15, 16), ]

RS_clean$TEAM_ABBREVIATION[RS_clean$TEAM_ABBREVIATION == 'SEA'] <- 'OKC'



# Get unique teams' slugs
team_slugs <- unique(RS_clean$TEAM_ABBREVIATION)


# filter tms index accordingly 
teams_infos <- tms %>% 
  filter(
    slugTeam %in% team_slugs
  ) %>% 
  mutate(
    logo = paste0(
      "https://raw.githubusercontent.com/Henryjean/data/refs/heads/main/square_nba_logos/", 
      slugTeam, 
      ".svg"
    ))


# rename to merge
colnames(teams_infos)[3] <- 'TEAM_ABBREVIATION'







###### RS YoY ====

# Create df with PTS scored overview (2pt, 3pt, ft)

RS_yearly <- RS_clean %>% 
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
    PTS, 
    TEAM_ABBREVIATION) %>%
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
    PTS, 
    TEAM_ABBREVIATION) %>% 
  pivot_longer(
    c("FG2PTs","FTPTs","FG3PTs"), 
    names_to="shot_type", 
    values_to="shot_points") %>%
  mutate(
    shot_type = factor(shot_type, levels=c("FG2PTs","FTPTs","FG3PTs")), 
    shot_lab = case_when(
      shot_type == "FG2PTs" ~ "Points From 2 PT Field Goal",
      shot_type == "FG3PTs" ~ "Points From 3 PT Field Goal",
      shot_type == "FTPTs" ~  "Points From Free Throws")
  )



# merge 
shot_types <- merge(RS_yearly, teams_infos, by = 'TEAM_ABBREVIATION')


shot_types$season_pts <- paste0(
  shot_types$SEASON_ID, "\n", shot_types$PTS, 'PTS'
)







# Plot all PTS YoY with three lines (2pt, 3pt, ft)
ggplot(data = shot_types, aes(x=SEASON_ID, y=shot_points, group=shot_type))+
  geom_textbox(
    aes(
      y=max(shot_points)+150,
      label= paste0(
        "<span style='font-family:\"Playfair Display\";font-size:15px'><b>", 
        TEAM_ABBREVIATION,
        "</b></span><br><span style='font-name:\"Open Sans\"; font-size:10px'><b>",
        SEASON_ID,
        "<br>",
        scales::comma(PTS),
        " PTS</b></span>"
        ), 
      color='red'
      ), 
    box.color=NA, 
    fill=NA, 
    hjust=.5, 
    halign=.5, 
    lineheight=.8, 
    alpha=.2)+
  geom_textbox(
    aes(
      y= max(shot_points) + 160,
      label = season_pts),
    color='black',
    box.color=NA,
    fill=NA,
    hjust=.5,
    halign=.5,
    lineheight=.8,
    alpha=.3) +
  geom_area(
    aes(fill=shot_type),
    linewidth=1.5,
    alpha=.2, 
    position="identity") +
  # geom_rect(
  #   ymin=1500,
  #   ymax=1600, 
  #   xmin = c(0:52) - .5, 
  #   xmax = c(0:52) + .5, 
  #   colour="white", 
  #   size=0.5, 
  #   alpha=0.2
  # ) +
  # geom_image(
  #   aes(
  #     image = logo
  #   ),
  #   y = 1550,
  #   size = 0.08
  # ) +
  scale_fill_manual(values=c("#e63946","#ee9b00","#00afb9","#f35345" ))+
  theme_minimal()+
  scale_color_identity()+
  geom_textline(
    data = RS_yearly %>% 
      filter(
        shot_type == 'FG2PTs'
      ),
    aes(
      label=shot_lab 
      ), 
    color = '#e63946', 
    family="Open Sans", fontface=2, linewidth=1.5, hjust=.1, vjust = -1, text_smoothing = 45, size=5
    )+
  geom_textline(
    data = RS_yearly %>% 
      filter(
        shot_type == 'FG3PTs'
      ),
    color = '#00afb9',
    aes(
      label=shot_lab, 
      ), 
    family="Open Sans", fontface=2, linewidth=1.5, hjust=.67, vjust =-1.5, text_smoothing = 50, size=5
  ) + 
  geom_textline(
    data = RS_yearly %>% 
      filter(
        shot_type == 'FTPTs'
      ),
    aes(
      label=shot_lab 
    ), 
    color = '#ee9b00', 
    family="Open Sans", fontface=2, linewidth=1.5, hjust=.185, vjust = -1, text_smoothing = 50, size=5
  )+
  theme(legend.position="none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(color="#A2A2A2", face="bold"), 
        plot.background = element_rect(color = 'floralwhite')
        )+
  scale_y_continuous(
    limits = c(0, 1500)
  ) +
  coord_cartesian(clip="off") + 
  labs(
    x = 'Season', 
    y = 'Points Scored'
  ) + 
  theme_davide()




ggsave('/Users/davidetissino/Desktop/KD_30k.png', dpi = 'retina', height = 9, width = 14)






ggplot(data = shot_types, aes(x=SEASON_ID, y=shot_points, group=shot_type))+
  geom_textbox(
    aes(
      y= 1530,
      label= paste0(
        "<b>",
        SEASON_ID,
        "<br>",
        scales::comma(PTS),
        " PTS</b></span>"
      ), 
      color='grey30'
    ), 
    box.color=NA, 
    fill=NA, 
    hjust=.5, 
    halign=.5, 
    lineheight=1
    )+
  geom_area(
    aes(fill=shot_type),
    linewidth=1.5,
    alpha=.2, 
    position="identity") +
geom_image(
  aes(
    image = logo
  ),
  y = 1600,
  size = 0.08
) +
scale_fill_manual(values=c("#e63946","#ee9b00","#00afb9","#f35345" ))+
  theme_minimal()+
  scale_color_identity()+
  geom_textline(
    data = RS_yearly %>% 
      filter(
        shot_type == 'FG2PTs'
      ),
    aes(
      label=shot_lab 
    ), 
    color = '#e63946', 
    family="Open Sans", fontface=2, linewidth=1.5, hjust=.1, vjust = -1, text_smoothing = 45, size=5
  )+
  geom_textline(
    data = RS_yearly %>% 
      filter(
        shot_type == 'FG3PTs'
      ),
    color = '#00afb9',
    aes(
      label=shot_lab, 
    ), 
    family="Open Sans", fontface=2, linewidth=1.5, hjust=.67, vjust =-1.5, text_smoothing = 50, size=5
  ) + 
  geom_textline(
    data = RS_yearly %>% 
      filter(
        shot_type == 'FTPTs'
      ),
    aes(
      label=shot_lab 
    ), 
    color = '#ee9b00', 
    family="Open Sans", fontface=2, linewidth=1.5, hjust=.185, vjust = -1, text_smoothing = 50, size=5
  )+
  theme_davide() +
  theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(color="#A2A2A2", face="bold"), 
        axis.text.x = element_blank(),
        plot.background = element_rect(color = 'floralwhite'),
        legend.position =  'none'
        )+
  scale_y_continuous(
    limits = c(0, 1550)
  ) +
  coord_cartesian(clip="off") + 
  labs(
    x = 'Season', 
    y = 'Points Scored'
  ) 







