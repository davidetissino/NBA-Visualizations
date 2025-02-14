### Steph Curry Scorigami #####

library(rvest)
library(tidyverse)
library(ggthemes)

# custom theme
theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='avenir'), 
      axis.title.x = element_text(face='bold.italic', size=12, margin=margin(t=10)),
      axis.title.y = element_text(face='bold.italic', size=12, margin=margin(r=10), angle = 90),
      axis.text.x = element_text(face='plain'),
      axis.text.y = element_text(face='plain'), 
      panel.background = element_rect('floralwhite'), 
      plot.background = element_rect('floralwhite'),
      plot.title = element_text(face='bold', size=15, hjust = 0.5),
      plot.subtitle=element_text(face='italic', size=10, hjust = 0.5, vjust = -1), 
      panel.grid.major = element_line(color='gray90', linetype = 'dashed'),
      plot.margin = margin(.5, .5, .25, .5, "cm")
    ) 
}

# Regular Season logs only 
logs <- function(year) {
  
  url <- paste0('https://www.basketball-reference.com/players/c/curryst01/gamelog/', year)
  
  log <- read_html(url) %>% html_nodes('table') %>% .[[8]] %>% html_table(fill = T)
  
  return(log) %>% as.data.frame()
  
}

# vector of seasons played
seasons <- c(2010:2024)

# final df with ALL gamelogs 
rs <- map_df(seasons, logs) 


# Playoff Logs  
url <- 'https://www.basketball-reference.com/players/c/curryst01/gamelog-playoffs/'

po <- read_html(url) %>%
  html_nodes('table') %>%
  .[[8]] %>%
  html_table(fill = T)


## PTS and 3PM ====
# keep only info on PTS and 3PM 
rs <- rs[,-c(1:13, 15:27, 29, 30)]

po <- po[,-c(1:14, 16:28, 30:32)]

# combine regular season and playoffs 
df <- rbind(po, rs)

# convert to numeric and delete NA 
colnames(df)[1] <- 'threes'
df$threes <- as.numeric(df$threes)
df$PTS <- as.numeric(df$PTS)

df <- df %>% drop_na()

# create ranges 
df$n_pts <- ifelse(df$PTS %in% 0:9, '0 to 9', 
                     ifelse(df$PTS %in% 10:19, '10 to 19', 
                            ifelse(df$PTS %in% 20:29, '20 to 29', 
                                   ifelse(df$PTS %in% 30:39, '30 to 39', 
                                          ifelse(df$PTS %in% 40:49, '40 to 49', 
                                                 ifelse(df$PTS >= 50, '50+', ''))))))


df$n_three <- ifelse(df$threes == 0, '0', 
                     ifelse(df$threes %in% 1:3, '1 to 3', 
                            ifelse(df$threes %in% 4:6, '4 to 6', 
                                   ifelse(df$threes %in% 7:9, '7 to 9', 
                                          ifelse(df$threes %in% 10:12, '10 to 12', 
                                                 ifelse(df$threes >= 13, '13', ''))))))


comb <- count(df, n_three, n_pts)

comb <- complete(comb, n_pts, n_three = c('0', '1 to 3', '4 to 6', '7 to 9', '10 to 12', '13'), fill = list(n = 0))


# plot 
comb %>%
  ggplot(aes(x = n_pts, y = n_three)) + 
  # creates tile style, add color for border 
  geom_tile(aes(fill = n), color = 'floralwhite', alpha = 0.7, lwd = 2.5, linetype = 1) +
  # makes tile a square
  coord_fixed() +
  # adds corresponding n on each tile 
  geom_text(aes(label = n), fontface = 'bold', color = 'black', size = 4) + 
  theme_davide() + 
  scale_fill_gradient(low = 'grey88', high = 'firebrick2', space = 'Lab', na.value = NA)+
  # reorders x axis 
  scale_x_discrete(limits = c('0 to 9', '10 to 19', '20 to 29', '30 to 39', '40 to 49', '50+')) + 
  scale_y_discrete(limits = c('0', '1 to 3', '4 to 6', '7 to 9', '10 to 12', '13')) +
  labs(
    x = 'POINTS', 
    y = 'THREES', 
    title = "Stephen Curry", 
    subtitle = 'No. of games with different combinations of 3PM and PTS (RS + Playoffs)', 
    caption = c('@dvdtssn | basketballreference', paste0('Updated ', format.Date(Sys.Date(), '%B %d, %Y')))
  ) + 
  theme(
    panel.grid.major = element_blank(),
    plot.subtitle = element_text(margin = margin(b=10), hjust = 0.3), 
    plot.caption = element_text(hjust = c(1, 0)), 
    legend.position = 'none', 
    panel.grid = element_blank(), 
    axis.line = element_line(size = .4, linetype = 1, color = 'black')
  )


# ggsave('/Users/davidetissino/Desktop/STEPH.2.png', dpi = 'retina')




## 3PM and FG% ====
# keeps 3pm and fg%
r <- rs[,-c(1:12, 15:30)]

colnames(r)[1] <- 'FG_perc'
colnames(r)[2] <- 'threes'

r$FG_perc <- as.numeric(r$FG_perc)
r$threes <- as.numeric(r$threes)

r <- r %>% drop_na()

r$FG_perc <- r$FG_perc * 100

# add new columns with info on ranges based on 3PM & FG% values 
r$n_three <- ifelse(r$threes == 0, '0', 
                   ifelse(r$threes %in% 1:3, '1 to 3', 
                          ifelse(r$threes %in% 4:6, '4 to 6', 
                                 ifelse(r$threes %in% 7:9, '7 to 9', 
                                        ifelse(r$threes %in% 10:12, '10 to 12', 
                                               ifelse(r$threes >= 13, '13', ''))))))

# ifelse as above not recognizing decimals, different method but correct result 
r <- r %>%
  mutate('n_fg' = 
           ifelse(between(FG_perc, 0, 19.9), '0 to 19', 
                  ifelse(between(FG_perc, 20.0, 39.9), '20 to 39', 
                         ifelse(between(FG_perc, 40.0, 59.9), '40 to 59', 
                                ifelse(between(FG_perc, 60.0, 79.9), '60 to 79',
                                       ifelse(between(FG_perc, 80.0, 100.0), '80+', ''))))))


# count the combinations  
raw <- count(r, n_fg, n_three)

# fill missing combinations with n = 0
raw <- complete(raw, n_fg, n_three = c('0', '1 to 3', '4 to 6', '7 to 9', '10 to 12', '13'), fill = list(n = 0))


# plot
raw %>%
  ggplot(aes(x = n_fg, y = n_three)) + 
  # creates tile style, add color for border 
  geom_tile(aes(fill = n), color = 'floralwhite', alpha = 0.7, lwd = 2.5, linetype = 1) +
  # makes tile a square
  coord_fixed() +
  # adds corresponding n on each tile 
  geom_text(aes(label = n), fontface = 'bold', color = 'black', size = 4) + 
  theme_davide() + 
  scale_fill_gradient(low = 'grey92', high = 'dodgerblue4', space = 'Lab', na.value = NA)+
  # reorders x axis 
  scale_x_discrete(limits = c('0 to 19', '20 to 39', '40 to 59', '60 to 79', '80+')) + 
  scale_y_discrete(limits = c('0', '1 to 3', '4 to 6', '7 to 9', '10 to 12', '13')) +
  labs(
    x = 'Field Goal Percentage', 
    y = 'Threes', 
    title = "Steph GOAT", 
    subtitle = 'No. of games with different combinations of 3PM and FG% (RS + Playoffs)', 
    caption = c('@dvdtssn | stats.nba.com', paste0('Updated ', format.Date(Sys.Date(), '%B %d, %Y')))
  ) + 
  theme(
    panel.grid.major = element_blank(),
    plot.subtitle = element_text(margin = margin(b=10), hjust = 0.3), 
    plot.caption = element_text(hjust = c(1, 0)), 
    legend.position = 'none', 
    panel.grid = element_blank(), 
    axis.line = element_line(size = .4, linetype = 1, color = 'black')
  )


# ggsave('/Users/davidetissino/Desktop/STEPH.1.png', dpi = 'retina')
