## PROJECT 5: SHOT LOCATION WITH NBASTATR ####

# Load packages 
library(tidyverse)
library(metR)
library(nbastatR)
library(extrafont)
library(teamcolors)
library(cowplot)
library(ggthemes)
library(readr)
library(tictoc)
library(purrr)

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
      plot.title = element_text(face='bold', size=17, hjust = 0.5),
      plot.subtitle=element_text(face='italic', size=12, hjust = 0.5, vjust = -1), 
      panel.grid.major = element_line(color='gray90', linetype = 'dashed'),
      plot.margin = margin(.5, .5, .25, .5, "cm")
    ) 
}

# who knows why but works 
Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 10)

# team colors and infos 
tms <- read_csv('/Users/davidetissino/Desktop/R/data/teams.csv')

# Load NBA court dimensions from Github
devtools::source_url("https://github.com/Henryjean/NBA-Court/blob/main/CourtDimensions.R?raw=TRUE")

#ONLY RUN ONCE
names <- unique(tms$team)

# function to get shot data for the season
get_shots <- function(team) {
  
  df <- teams_shots(teams = team, season_types = "Regular Season", seasons = 2024)
  
  return(df)
  
}

# create one df with all shot data for the season
# tic()
# shots <- map_df(names, get_shots)
# toc()

# save as csv
# write.csv(shots, '/Users/davidetissino/Desktop/R/data/24 season shooting.csv', row.names = FALSE)

# saved as csv --> now import it
shooting <- read.csv('/Users/davidetissino/Desktop/R/data/24 season shooting.csv')

# column nameTeam to team to meet tms 
colnames(shooting)[7] <- 'team'

# modify LA Clippers to Los Angeles Clippers (doesnt merge otherwise)
shooting['team'][shooting['team'] == 'LA Clippers'] <- 'Los Angeles Clippers'

# change some colors for better readability 
tms$primary <- ifelse(tms$slugTeam == 'CHA', '#008ca8', tms$primary)
tms$primary <- ifelse(tms$slugTeam == 'DEN', '#fdb927', tms$primary)
tms$primary <- ifelse(tms$slugTeam == 'GSW', '#1D428A', tms$primary)
tms$primary <- ifelse(tms$slugTeam == 'PHI', '#006bb6', tms$primary)
tms$primary <- ifelse(tms$slugTeam == 'SAS', '#696969', tms$primary)
tms$primary <- ifelse(tms$slugTeam == 'UTA', '#f9a01b', tms$primary)
tms$primary <- ifelse(tms$slugTeam == 'ORL', '#c4ced3', tms$primary)

# Merge shot info with tms
final <- left_join(shooting, tms)

# convert and clean shot data to fit court dimensions 
final <- final %>% mutate(locationX = as.numeric(as.character(locationX)) / 10,
                    locationY = as.numeric(as.character(locationY)) / 10 + hoop_center_y)

# Horizontally flip the data
final$locationX <- final$locationX * -1 

# remove shots from distance > 35 ft 
final <- final %>% filter(distanceShot <= 35)


# graph
final %>%
  ggplot(aes(locationX, locationY)) + 
  geom_point(size=0.1, alpha=0.4, color = final$primary) + 
  # unknown 
  geom_path(data = court_points,
            aes(x = x, y = y, group = desc, linetype=dash),
            color = "gray10", size = .25) +
  # unknown
  coord_fixed(clip='off') +
  scale_y_continuous(limits = c(0, 45)) +
  scale_x_continuous(limits = c(-27.5, 27.5)) +
  theme_davide() +
  theme(
    legend.position = 'none', 
    panel.grid.major = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    strip.background = element_rect('floralwhite'),
    plot.subtitle = element_text(margin = margin(b=13)),
    strip.text = element_text(face = 'bold')
  ) + 
  facet_wrap(~ slugTeam, strip.position = 'bottom') +
  labs(
    title = 'Teams shooting habits',
    subtitle = paste0('Data updated ', format.Date(Sys.Date(), '%B %d, %Y')),
    caption = '@dvdtssn | stats.nba.com', 
  )

# save
# ggsave('/Users/davidetissino/Desktop/R/graphs/Teams shooting habits.png', dpi='retina', width=2100, height=2100, units='px')

# ggsave('/Users/davidetissino/Library/Mobile Documents/com~apple~CloudDocs/Teams shooting.png', dpi='retina', height=2600, width=2600, units='px')  


