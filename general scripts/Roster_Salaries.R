library(tidyverse)
library(rvest)
library(paletteer)
library(extrafont)
library(ggthemes)
library(stringr)



# CONTENT ####
## Overview of teams' financials


# Load custom theme
theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='Geist Mono'), 
      axis.title.x = element_text(color = 'black', margin = margin(t = 30, b = 8), family = 'K2D', face = 'bold', size = 19), 
      axis.title.y = element_text(color = 'black', margin = margin(r = 25, l = 8), family = 'K2D', face = 'bold', size = 19, angle = 90), 
      axis.text = element_text(color = 'grey30'),
      axis.text.x = element_text(face='bold', size = 13),
      axis.text.y = element_text(face='bold', size = 13), 
      panel.background = element_rect('grey98'), 
      plot.background = element_rect('grey98'),
      plot.title = element_text(margin = margin(b=15, t = 10), face='bold', size=30, hjust = 0.5, family = 'Proxima Nova'),
      plot.subtitle=element_text(size=15, hjust = 0, margin = margin(b = 10), family = 'Proxima Nova'), 
      panel.grid.major = element_line(color='gray80', linetype = 'dashed'),
      plot.margin = margin(0, 10, 5, 10),
      plot.caption = element_text(size = 6.5, hjust = -0.4, vjust = -18)
    ) 
}


## Players index ----
players <- read.csv('/Users/davidetissino/Desktop/R/data/Active Index.csv')



## Teams index ----
tms <- read.csv('/Users/davidetissino/Desktop/R/data/teams.csv')

# vector of lowercase, dashed names for scraping later
team_names <- gsub(" ", "-",tolower(tms$team))  

team_names[10] <- 'la-clippers'




## Salaries Data ==== 
### Spotrac ----
salaries_data <- function(team) {
  
  # set URL
  url <- paste0('https://www.spotrac.com/nba/', team, '/overview/_/year/2024')
  
  # read in URL
  team_salary <- read_html(url)%>%
    html_nodes('table') %>%
    .[[1]]%>%
    html_table(fill = TRUE) 
  
  # remove columns 
  team_salary <- team_salary[, -c(1, 8:12)]
  
  # rename some columns 
  colnames(team_salary)[1] <- 'Player'
  colnames(team_salary)[5] <- 'Salary'
  colnames(team_salary)[6] <- 'Cap_pct'
  
  # proper player column 
  team_salary$Player <- gsub("^.*\n\\s*", "", team_salary$Player)
  
  # filter TW out 
  team_salary <- team_salary %>% 
    filter(
      Type != 'TW'
    )
  
  # numeric salary column
  team_salary <- team_salary %>%
    mutate(Salary = as.numeric(gsub("[\\$,]", "", Salary))) 
  
  # decimal cap percentage column 
  team_salary$Cap_pct <- as.numeric(gsub("%", "", team_salary$Cap_pct)) / 100
  
  # add team column to ID
  team_salary$team <- team
  
  # proper rendering 
  team_salary$team <- gsub('-', ' ', team_salary$team)
  
  team_salary$team <- tools::toTitleCase(team_salary$team)
  
  return(team_salary)
  
}


# map df with all teams

#data <- map_df(team_names, salaries_data)
#write.csv(data, '/Users/davidetissino/Desktop/R/data/salaries_ty.csv', row.names = F)

salaries <- read.csv('/Users/davidetissino/Desktop/R/data/salaries_ty.csv')


# filter Exhibit-10 contracts
salaries <- salaries %>% 
  filter(
    Type != 'E10'
  ) %>%
  .[, c(7, 1:3, 5, 6, 4)]


# some players have mixed pos
salaries$Player[salaries$Pos == 'G']
salaries$Player[salaries$Pos == 'F']

# manually adjust some positions
salaries$Pos[salaries$Player == 'Craig Porter Jr.'] <- 'PG'
salaries$Pos[salaries$Player == 'Haywood Highsmith'] <- 'PF'
salaries$Pos[salaries$Player == 'Ricky Council IV'] <- 'SG'
salaries$Pos[salaries$Player == 'Alex Reese'] <- 'PF'









# count each position 
fin <- salaries %>%
  select(-c(2, 4:7)) %>%
  group_by(team, Pos) %>% 
  summarise(
    count = n()
  )


# if in ecf, eastern conf, otherwise be western conf
fin <- fin %>%
  mutate(conf = case_when(
    team %in% c("Atlanta Hawks",
                "Boston Celtics",
                "Brooklyn Nets",
                "Charlotte Hornets",
                "Chicago Bulls",
                "Cleveland Cavaliers",
                "Detroit Pistons",
                "Indiana Pacers",
                "Miami Heat",
                "Milwaukee Bucks",
                "New York Knicks",
                "Orlando Magic",
                "Philadelphia Sixers",
                "Toronto Raptors",
                "Washington Wizards") ~ "Eastern Conf.",
    TRUE ~ "Western Conf."
  ))




# adjust factors
df$Pos <- as.factor(df$Pos)
df$Pos <- factor(df$Pos, levels = c('PG', 'SG', 'SF', 'PF', 'C'))


# column for total players drafted (will use to sort chart)
df <- df %>%
  group_by(team) %>%
  mutate(pg_count = count[Pos == "PG"])



# Plot

df %>%
  ggplot(aes(count, fct_reorder(team, pg_count),
             fill = fct_relevel(Pos, 'C', 'PF', 'SF', 'SG', 'PG'))) +
  geom_col(alpha = 1) +
  geom_vline(xintercept = seq(1, 15, 1), size = .5, color = 'grey98') +
  facet_wrap(~fct_rev(conf),
             scales = 'free_y') +
  scale_x_continuous(limits = c(0, 15)) +
  scale_fill_manual(values = c('#FAB255FF','#DD5129FF',  '#43B284FF', '#0F7BA2FF', 'grey20' ),
                    breaks = c('PG', 'SG', 'SF', 'PF', 'C'),
                    labels = c('PG', 'SG', 'SF', 'PF', 'C')) +
  guides(fill=guide_legend(
    keywidth= .7,
    keyheight= .18,
    default.unit="inch",
    title.position = 'top',
    label.position = 'bottom',
    title.vjust = .5,
    label.vjust = .2,
    nrow = 1)
  ) +
  theme_davide() +
  theme(text = element_text(family = 'Geist Mono'),
        plot.title = element_text(face = 'bold', family = 'K2D', size = 22, hjust = 0.5, margin = margin(b=15, t = 5)),
        plot.subtitle = element_text(family = 'K2D', size = 11, hjust = 0.5, margin = margin(b = 20)),
        plot.title.position = "plot",
        legend.position = 'top',
        aspect.ratio = 2/1,
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 7.5, vjust = 12),
        legend.title = element_text(size = 7.5, hjust = .5, vjust = -2),
        legend.background = element_rect(fill = 'grey98'),
        plot.margin = margin(0, 10, 5, 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(0, -5, 0, 0), size = 8.5, face = 'bold', hjust = 1),
        panel.grid.major = element_blank(), 
        plot.caption = element_text(size = 6.5, hjust = -0.4, vjust = -18), 
        strip.background = element_rect(fill = 'grey98'), 
        legend.justification = -.1
  ) +
  labs(
    title = "NBA Positions Distribution",
    subtitle = "Distribution of listed positions for rostered players on each NBA team",
    fill = "Five Positions",
    caption = '* Does not include Two-Way contracts \n 
Source: Spotrac | Chart: Davide Tissino')


ggsave("/Users/davidetissino/Desktop/Roster_Positions.png", w = 7, h = 7, dpi = 'retina')











salaries <- salaries %>%
  mutate(conf = case_when(
    team %in% c("Atlanta Hawks",
                "Boston Celtics",
                "Brooklyn Nets",
                "Charlotte Hornets",
                "Chicago Bulls",
                "Cleveland Cavaliers",
                "Detroit Pistons",
                "Indiana Pacers",
                "Miami Heat",
                "Milwaukee Bucks",
                "New York Knicks",
                "Orlando Magic",
                "Philadelphia Sixers",
                "Toronto Raptors",
                "Washington Wizards") ~ "Eastern Conf.",
    TRUE ~ "Western Conf."
  ))





ggplot(salaries, aes(x = fct_reorder(team, Cap_pct, .fun = sum), y = Cap_pct, fill = fct_rev(Player))) +
  geom_col() +
  facet_wrap(~conf, scales = 'free_y') +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  #scale_fill_manual(values = c('#FAB255FF','#DD5129FF',  '#43B284FF', '#0F7BA2FF', 'grey20', '#E56E1B')) +
  guides(fill = guide_legend(
    title.position = 'top', 
    label.position = 'bottom',
    nrow = 2
  )) +
  labs(
    title = "NBA Team Salary Cap Distribution",
    subtitle = "Salary cap percentage by player for each NBA team",
    x = NULL,
    y = "Team Salary Cap (%)",
    fill = "Players"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = 'Geist Mono'),
    plot.title = element_text(face = 'bold', family = 'K2D', size = 22, hjust = 0.5, margin = margin(b = 15, t = 5)),
    plot.subtitle = element_text(family = 'K2D', size = 11, hjust = 0.5, margin = margin(b = 20)),
    legend.position = 'top',
    aspect.ratio = 2/1,
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(size = 7.5, vjust = 12),
    legend.title = element_text(size = 7.5, hjust = .5, vjust = -2),
    plot.margin = margin(0, 10, 5, 10),
    strip.background = element_rect(fill = 'grey98')
  )





library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

# Assuming `salaries` is already defined in your environment

# Set salary cap
salary_cap <- 140588000

# Calculate total salary for each team and assign conference
samos <- salaries %>%
  group_by(team) %>%
  mutate(
    total_salary = sum(Salary),
    conf = case_when(
      team %in% c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", "Chicago Bulls", 
                  "Cleveland Cavaliers", "Detroit Pistons", "Indiana Pacers", "Miami Heat", "Milwaukee Bucks", 
                  "New York Knicks", "Orlando Magic", "Philadelphia Sixers", "Toronto Raptors", "Washington Wizards") ~ "Eastern Conf.",
      TRUE ~ "Western Conf."
    )
  ) %>%
  ungroup()

# Plot
ggplot(samos, aes(x = total_salary, y = fct_reorder(team, total_salary), fill = Pos)) +
  geom_col(width = 0.7, position = "stack") +
  geom_vline(xintercept = salary_cap, linetype = "dashed", color = "red", size = 0.8) +
  facet_wrap(~ conf, scales = 'free_y') +
  scale_x_continuous(labels = dollar_format(), limits = c(0, max(samos$total_salary) * 1.1)) +
  scale_fill_manual(values = c('PG' = '#FAB255FF', 'SG' = '#DD5129FF', 'SF' = '#43B284FF', 'PF' = '#0F7BA2FF', 'C' = 'grey20')) +
  labs(
    title = "NBA Team Salary Distribution by Position",
    subtitle = "Total team salary with player positions indicated, and salary cap marked",
    x = "Total Team Salary ($)",
    y = NULL,
    fill = "Positions"
  ) +
  theme_davide() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position = "none",  # Remove player legend
    strip.text = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 9)
  )










