### PROJECT 8: SCORING TREND OVER 48 MINUTES FOR PTS LEADERS ### 

library(ggthemes)
library(tidyverse)
library(extrafont)
library(janitor)
library(nbastatR)
library(ggfx)
library(ggridges)
library(lubridate)
library(tictoc)
library(ggimage)
library(ggpubr)

# custom theme
theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='avenir'), 
      axis.title.x = element_text(face='bold.italic', size=9, margin=margin(t=10)),
      axis.title.y = element_text(face='italic', size=12, margin=margin(r=10), angle = 90),
      axis.text.x = element_text(face='plain'),
      axis.text.y = element_text(face='bold.italic'), 
      panel.background = element_rect('floralwhite'), 
      plot.background = element_rect('floralwhite'),
      plot.title = element_text(face='bold', size=15, hjust = 0.5),
      plot.subtitle=element_text(face='italic', size=10, hjust = 0.5, vjust = -1), 
      panel.grid.major = element_line(color='gray90', linetype = 'dashed'),
      plot.margin = margin(.5, .5, .25, .5, "cm")
    ) 
}

# who knows why but works 
Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 2)

# team colors and infos 
tms <- read_csv('/Users/davidetissino/Desktop/R/data/colors and logos.csv')

names <- unique(tms$team)

# define vector of teams we are considering (teams of points leaders)
tms_ldrs <- c('Phoenix Suns', 'Philadelphia 76ers', 'Denver Nuggets', 'Boston Celtics', 'Golden State Warriors', 
           'Dallas Mavericks', 'Oklahoma City Thunder', 'Sacramento Kings', 'Milwaukee Bucks', 'Cleveland Cavaliers')


# create df with info for desired teams & convert to df
tic()
df <- teams_shots(seasons = 2024, teams = tms_ldrs)
toc()

df <- as.data.frame(df)

# vector of pts leaders to extract data for them only
ldrs <- c('Joel Embiid', 'Luka Doncic', 'Kevin Durant', 'Shai Gilgeous-Alexander', "De'Aaron Fox", 'Stephen Curry', 
          'Donovan Mitchell', 'Jayson Tatum', 'Giannis Antetokounmpo', 'Nikola Jokic')

# filter for pts leaders only 
df <- df %>%
  filter(namePlayer %in% ldrs)

# keep only data regarding player, slug, and time of the shot (quarter, minutes & seconds)
df <- df[,c(6:8,16,17,24)]

# create new columns with min/secs/tot elapsed since begnning of game 
df <- df %>%
  mutate(
    min_elaps = case_when(
      numberPeriod == 1 ~ 12 - minutesRemaining, 
      numberPeriod == 2 ~ 24 - minutesRemaining, 
      numberPeriod == 3 ~ 36 - minutesRemaining, 
      numberPeriod == 4 ~ 48 - minutesRemaining, 
      numberPeriod == 5 ~ 53 - minutesRemaining, 
      numberPeriod == 6 ~ 58 - minutesRemaining),
    sec_elaps = 60 - secondsRemaining, 
    time_elap = paste(min_elaps, sec_elaps, sep = '.'))

# convert column to numeric 
df$time_elap <- as.numeric(as.character(df$time_elap))

# rename team column to merge with tms
colnames(df)[2] <- 'team'

# merge df with tms info and colors
df <- merge(df, tms, by = 'team')
df <- df[,-c(10,13,14)]

# get specific players heashot 
photos <- nba_players() %>%
  filter(namePlayer %in% ldrs) %>%
  select(namePlayer, urlPlayerPhoto)

colnames(photos)[1] <- 'player'
colnames(df)[2] <- 'player'

# combine two dfs to have also player photo 
df <- merge(df, photos, by = 'player')

colnames(df)[12] <- 'photo'

# custom order to reflect PPG leaders
df <- df %>%
  arrange(match(player, c('Joel Embiid','Kevin Durant',  'Luka Doncic', "De'Aaron Fox", 'Shai Gilgeous-Alexander', 'Giannis Antetokounmpo', 
                          'Stephen Curry', 'Nikola Jokic', 'Jayson Tatum', 'Donovan Mitchell')))

# manually tweak some names
df['player'][df['player'] == 'Joel Embiid'] <- 'J. Embiid'
df['player'][df['player'] == 'Kevin Durant'] <- 'K. Durant'
df['player'][df['player'] == 'Luka Doncic'] <- 'L. Doncic'
df['player'][df['player'] == "De'Aaron Fox"] <- 'D. Fox'
df['player'][df['player'] == 'Shai Gilgeous-Alexander'] <- 'Shai'
df['player'][df['player'] == 'Giannis Antetokounmpo'] <- 'Giannis'
df['player'][df['player'] == 'Stephen Curry'] <- 'S. Curry'
df['player'][df['player'] == 'Nikola Jokic'] <- 'N. Jokic'
df['player'][df['player'] == 'Donovan Mitchell'] <- 'D. Mitchell'
df['player'][df['player'] == 'Jayson Tatum'] <- 'J. Tatum'

# fixed order 
df$player <- factor(df$player, levels = rev(unique(df$player)))

# plot
tic()
df %>%
  ggplot(aes(x = time_elap, y = player)) + 
  # nudge_y > 0 moves image upward, nudge_x < 0 moves image leftward
  geom_image(x = -3, aes(image = photo), size = 0.09, nudge_y = 0.33, nudge_x = - 0.25) +
  # expand limits of the graph to properly plug image
  expand_limits(x=-7) +
  # density curve 
  geom_density_ridges(alpha = 0.7, scale = 0.75, bandwidth = .7, aes(fill = slugTeam)) +
  # manually fill colors otherwise order not correct
  scale_fill_manual(values = c('BOS' = '#008348', 'CLE' = '#860038', 'DEN' = '#fdb927', 'GSW'= '#1d428a', 
                               'MIL' = '#00471b', 'OKC' = '#ef3b24', 'SAC' = '#5a2d81',
                               'DAL' = '#c4ced3', 'PHX' = '#e56020', 'PHI' = '#006bb6')) +
  # custom theme
  theme_davide() +
  # modify tickers of x axis 
  scale_x_continuous(limits = c(-2, 55), 
                     breaks = c(0, 12, 24, 36, 48, 53), 
                     labels = c('0', '12', '24', '36', '48', '2OT')) +
  theme(plot.title = element_text(hjust = 0.35), 
        plot.subtitle = element_text(hjust = .4, margin = margin(b=15), face = 'italic'), 
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = 'bold.italic'),
        panel.grid.major.y = element_blank(),
        legend.position = 'none'
        ) + 
  labs(title = 'Distribution Of Shooting Attempts During A Game',
       subtitle = paste0('Among Top10 in PPG | Updated ', format.Date(Sys.Date(), '%B %d, %Y')),
       x = 'Game minute',
       caption = c('@dvdtssn | stats.nba.com')) +
  # tweak y tickers 
  rotate_y_text(angle = 0, vjust = -.2) # angle = 0 keeps horiz, vjust > 0 moves down
toc()  



# save
tic()
ggsave('/Users/davidetissino/Desktop/Shooting trend top10 PPG.png', dpi = 'retina')
toc()





