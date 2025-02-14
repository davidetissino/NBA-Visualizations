#### JK COMPARISON STARTER VS BENCH ####

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
  
  url <- paste0('https://www.basketball-reference.com/players/k/kuminjo01/gamelog/', year)
  
  log <- read_html(url) %>% html_nodes('table') %>% .[[8]] %>% html_table(fill = T)
  
  return(log) %>% as.data.frame()
  
}

seasons <- c(2022:2024)

rs <- map_df(seasons, logs)

url <- 'https://www.basketball-reference.com/players/k/kuminjo01/gamelog-playoffs/'

po <- read_html(url) %>%
  html_nodes('table') %>% 
  .[[8]] %>% 
  html_table(fill = T)


po <- po[,-c(1:9, 32)]
rs <- rs[,-c(1:8)]

df <- rbind(rs, po) %>% 
  filter(GS == 0 | GS == 1)

df$MP <- gsub(':', '.', df$MP)

df <- df %>% mutate_at(1:22, as.numeric)

# convert all remaining NAs to 0
df[is.na(df)] <- 0





# df for starter and bench games
start <- df %>% filter(GS ==1)
colnames(start)[22] <- 'pm'

bench <- df %>% filter(GS == 0)
colnames(bench)[22] <- 'pm'

# convert to numeric minutes played 
start$MP <- gsub(':', '.', start$MP)
start$MP <- as.numeric(as.character(start$MP))

bench$MP <- gsub(':', '.', bench$MP)
bench$MP <- as.numeric(as.character(bench$MP))

# convert all to numeric
start <- start %>% mutate_at(1:22, as.numeric)
bench <- bench %>% mutate_at(1:22, as.numeric)

# convert all remaining NAs to 0
start[is.na(start)] <- 0
bench[is.na(bench)] <- 0

tot_start <- colSums(start[,1:22])
tot_start <- as.data.frame(t(tot_start))

tot_bench <- colSums(bench[,1:22])
tot_bench <- as.data.frame(t(tot_bench))

# manually reevaluate some columns % 
tot_start$`FG%` <- tot_start$FG / tot_start$FGA
tot_start$`3P%` <- tot_start$'3P' / tot_start$'3PA'
tot_start$`FT%` <- tot_start$FT / tot_start$FTA

tot_bench$`FG%` <- tot_bench$FG / tot_bench$FGA
tot_bench$`3P%` <- tot_bench$'3P' / tot_bench$'3PA'
tot_bench$`FT%` <- tot_bench$FT / tot_bench$FTA
  



tot <- rbind(tot_start, tot_bench)


tot %>% 
  gt() %>% 
  tab_spanner(
    label = 'Starter', 
    columns = c(MP, '3P%', TRB)
  )

tot %>% 
  gt()




airquality_m <-
  airquality %>%
  mutate(Year = 1973L) %>%
  slice(1:10)

# Create a display table using the `airquality`
# dataset; arrange columns into groups
gt_tbl <-
  gt(data = airquality_m) %>%
  tab_header(
    title = "New York Air Quality Measurements",
    subtitle = "Daily measurements in New York City (May 1-10, 1973)"
  ) %>%
  tab_spanner(
    label = "Time",
    columns = vars(Year, Month, Day)
  ) %>%
  tab_spanner(
    label = "Measurement",
    columns = vars(Ozone, Solar.R, Wind, Temp)
  ) %>% 
  gtsave('/Users/davidetissino/Desktop/tab.png')

