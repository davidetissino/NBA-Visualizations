### JOKIC SCORIGAMI ####

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
  
  url <- paste0('https://www.basketball-reference.com/players/j/jokicni01/gamelog/', year)
  
  log <- read_html(url) %>% html_nodes('table') %>% .[[8]] %>% html_table(fill = T)
  
  return(log) %>% as.data.frame()
  
}


seasons <- c(2016:2024)

df <- map_df(seasons, logs)

df <- df[,c(22, 23)]

df$TRB <- as.numeric(df$TRB)
df$AST <- as.numeric(df$AST)

df <- df %>% drop_na() 


# playoff logs 
url <- 'https://www.basketball-reference.com/players/j/jokicni01/gamelog-playoffs/'

po <- read_html(url) %>%
  html_nodes('table') %>%
  .[[8]] %>% 
  html_table(fill = T)

po <- po[,c(23, 24)]

po$TRB <- as.numeric(po$TRB)
po$AST <- as.numeric(po$AST)

po <- po %>% drop_na()

# merge
df <- rbind(df, po)

df$n_ast <- ifelse(df$AST == 0, '0', 
                   ifelse(df$AST %in% 1:4, '1 to 4', 
                          ifelse(df$AST %in% 5:9, '5 to 9', 
                                 ifelse(df$AST %in% 10:14, '10 to 14', 
                                        ifelse(df$AST >= 15, '15+', '')))))


df$n_reb <- ifelse(df$TRB == 0, '0', 
                   ifelse(df$TRB %in% 1:4, '1 to 4', 
                          ifelse(df$TRB %in% 5:9, '5 to 9', 
                                 ifelse(df$TRB %in% 10:14, '10 to 14', 
                                        ifelse(df$TRB %in% 15:19, '15 to 19', 
                                               ifelse(df$TRB >= 20, '20+', ''))))))



raw <- count(df, n_reb, n_ast)

raw <- complete(raw, n_reb, n_ast = c('0', '1 to 4', '5 to 9', '10 to 14', '15+'), fill = list(n = 0))

raw %>%
  ggplot(aes(x = n_reb, y = n_ast)) + 
  # creates tile style, add color for border 
  geom_tile(aes(fill = n), color = 'floralwhite', alpha = 0.7, lwd = 2.5, linetype = 1) +
  # makes tile a square
  coord_fixed() +
  # adds corresponding n on each tile 
  geom_text(aes(label = n), fontface = 'bold', color = 'black', size = 5) + 
  theme_davide() + 
  scale_fill_gradient2(low = 'grey85', mid = 'coral', high = 'firebrick', midpoint = 75)+
  # reorders x axis 
  scale_x_discrete(limits = c('0', '1 to 4', '5 to 9', '10 to 14', '15 to 19', '20+')) + 
  scale_y_discrete(limits = c('0', '1 to 4', '5 to 9', '10 to 14', '15+')) + 
  labs(
    x = 'REBOUNDS', 
    y = 'ASSISTS', 
    title = "Nikola Jokic: Triple Double Player", 
    subtitle = 'No. of games with different combinations of rebounds and assists', 
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

# ggsave('/Users/davidetissino/Desktop/Joker.png', dpi = 'retina')




