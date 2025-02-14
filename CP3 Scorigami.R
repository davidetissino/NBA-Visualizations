### Chris Paul Assists to TOVs for career ###

library(rvest)
library(tidyverse)
library(ggplot2)
library(stats)
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


# Function 
logs <- function(year) {
  
  url <- paste0('https://www.basketball-reference.com/players/p/paulch01/gamelog/', year)
  
  log <- read_html(url) %>% html_nodes('table') %>% .[[8]] %>% html_table(fill = T)
  
  return(log) %>% as.data.frame()
  
}

# vector of seasons played
seasons <- c(2006:2024)

# final df with ALL gamelogs 
df <- map_df(seasons, logs) 

# keep only AST & TOVs column
df <- df[,-c(1:22,24, 25,27:30)]

# convert to numeric 
df$AST <- as.numeric(df$AST)
df$TOV <- as.numeric(df$TOV)

# remove NAs 
df <- df %>% drop_na()

# add new columns with info on ranges based on AST & TOV values 
df$n_ast <- ifelse(df$AST == 0, '0', 
                        ifelse(df$AST %in% 1:4, '1 to 4', 
                               ifelse(df$AST %in% 5:9, '5 to 9', 
                                      ifelse(df$AST %in% 10:14, '10 to 14', 
                                             ifelse(df$AST %in% 15:19, '15 to 19', 
                                                    ifelse(df$AST >= 20, '20+', ''))))))


df$n_tov <- ifelse(df$TOV == 0, '0', 
                        ifelse(df$TOV %in% 1:2, '1 to 2', 
                               ifelse(df$TOV %in% 3:4, '3 to 4', 
                                      ifelse(df$TOV %in% 5:6, '5 to 6', 
                                             ifelse(df$TOV %in% 6:7, '6 to 7', 
                                                    ifelse(df$TOV >= 8, '8+', ''))))))


# count the combinations  
raw <- count(df, n_ast, n_tov)

# fill missing combinations with n = 0
raw <- complete(raw, n_tov, n_ast = c('0', '1 to 4', '5 to 9', '10 to 14', '15 to 19', '20+'), fill = list(n = 0))


# plot 
raw %>%
  ggplot(aes(x = n_ast, y = n_tov)) + 
  # creates tile style, add color for border 
  geom_tile(aes(fill = n), color = 'floralwhite', alpha = 0.7, lwd = 2.5, linetype = 1) +
  # makes tile a square
  coord_fixed() +
  # adds corresponding n on each tile 
  geom_text(aes(label = n), fontface = 'bold', color = 'black', size = 4) + 
  theme_davide() + 
  scale_fill_gradient2(low = 'grey85', mid = 'coral', high = 'firebrick', midpoint = 140)+
  # reorders x axis 
  scale_x_discrete(limits = c('0', '1 to 4', '5 to 9', '10 to 14', '15 to 19', '20+')) + 
  labs(
    x = 'ASSISTS', 
    y = 'TURNOVERS', 
    title = "Chris Paul's Efficiency As A Passer", 
    subtitle = 'No. of games with different combinations of assists and turnovers (RS only)', 
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

# ggsave('/Users/davidetissino/Desktop/CP3.9.png', dpi = 'retina')






