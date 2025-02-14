library(tidyverse)
library(rvest)
library(paletteer)
library(extrafont)
library(cowplot)
library(ggthemes)
library(janitor)


# CONTENT: 
## Team roster composition (players acquired thru trade, FA, draft, ...)



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


# BY ACQUISITION #####

# url to scrape
url <- "https://basketball.realgm.com/nba/transactions/composition"


# convert html table to data frame
df <- url %>%
  read_html() %>%
  html_elements('table') %>%
  html_table() %>%
  .[9] %>%          # initiallly 13, changes sometimes
  as.data.frame()


# select relevant columns and adjust values
df <- df %>%
  select(-Total.Roster, -Acquiredvia.Expansion, -Acquiredvia.Draft.Rights.Trade, -Selectedvia.Draft) %>%
  filter(Team != "League Totals") %>%
  pivot_longer(-Team) %>%
  mutate(name = case_when(
    name == "Draft...Draft.RightsTrade.Combined" ~ "Draft",
    name == "Acquiredvia.Trade" ~ "Trade",
    name == "Acquiredvia.Waiver.Claim" ~ "Waivers",
    name == "Acquiredvia.Free.Agency" ~ "Free Agency",
    TRUE ~ ""
  ))


# if in ecf, eastern conf, otherwise be western conf
df <- df %>%
  mutate(conf = case_when(
    Team %in% c("Atlanta Hawks",
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
df$name <- as.factor(df$name)
df$name <- factor(df$name, levels = c('Draft', 'Free Agency', 'Trade', 'Waivers'))


# column for total players drafted (will use to sort chart)
df <- df %>%
  group_by(Team) %>%
  mutate(draft_val = value[name == "Draft"])



# Plot

df %>%
  ggplot(aes(value, fct_reorder(Team, draft_val),
             fill = fct_relevel(name, 'Waivers', 'Trade', 'Free Agency', 'Draft'))) +
  geom_col(alpha = 1) +
  geom_vline(xintercept = seq(1, 15, 1), size = .5, color = 'floralwhite') +
  facet_wrap(~fct_rev(conf),
             scales = 'free_y') +
  scale_x_continuous(limits = c(0, 15)) +
  scale_fill_manual(values = c('#FAB255FF','#DD5129FF',  '#43B284FF', '#0F7BA2FF' ),
                    breaks = c('Draft', 'Free Agency', 'Trade', 'Waivers'),
                    labels = c('Draft', 'Free Agency', 'Trade', 'Waivers')) +
  guides(fill=guide_legend(
    keywidth= .75,
    keyheight= .2,
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
        legend.justification = 0.1
  ) +
  labs(
    title = "NBA Rosters Composition",
    subtitle = "Acquisition methods for rostered players on each NBA team",
    fill = "Acquired Via",
    caption = 'Source: RealGM | Chart: Davide Tissino')





ggsave("/Users/davidetissino/Desktop/Roster_Composition.png", w = 7, h = 7, dpi = 'retina')





# BY POSTION ####

### NBA Data - 3 POS====
players <- read.csv('/Users/davidetissino/Desktop/R/data/Active Index.csv')

# remove secondary position 
players$POSITION <- sub("-.*", "", players$POSITION)


# count each position 
df1 <- players %>%
  select(c(4, 11)) %>%
  group_by(slugTeam, POSITION) %>% 
  summarise(
    count = n()
  )


# proper teams info to merge
tms <- tms %>%
  select(-idTeam, -primary, - secondary, -logo) 

# merge
def <- merge(df1, tms, by = 'slugTeam')


# if in ecf, eastern conf, otherwise be western conf
def <- def %>%
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
def$POSITION <- as.factor(def$POSITION)
def$POSITION <- factor(def$POSITION, levels = c('G', 'F', 'C'))


# column for total players drafted (will use to sort chart)
def <- def %>%
  group_by(team) %>%
  mutate(guard_count = count[POSITION == "G"])


'#DD5129FF'
'#0F7BA2FF'
'#FAB255FF'
'#43B284FF'
'#8785B2FF'
'#3A488AFF'
'#45837FFF'



# Plot

def %>%
  ggplot(aes(count, fct_reorder(team, guard_count),
             fill = fct_relevel(POSITION, 'C', 'F', 'G'))) +
  geom_col(alpha = 1) +
  geom_vline(xintercept = seq(1, 18, 1), size = .5, color = 'grey98') +
  facet_wrap(~fct_rev(conf),
             scales = 'free_y') +
  scale_x_continuous(limits = c(0, 20)) +
  scale_fill_manual(values = c('#FAB255FF',  '#45837FFF', '#DD5129FF' ),
                    breaks = c('G','F', 'C'),
                    labels = c('G', 'F', 'C')) +
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
        aspect.ratio = 2.3/1,
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(size = 7.5, vjust = 12),
        legend.title = element_text(size = 7.5, hjust = .5, vjust = -2),
        legend.background = element_rect(fill = 'grey98'),
        plot.margin = margin(0, 10, 5, 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(0, -5, 0, 0), size = 8.5, face = 'bold', hjust = 1),
        panel.grid.major = element_blank(), 
        plot.caption = element_text(size = 6.5, hjust = -0.4, vjust = -10), 
        strip.background = element_rect(fill = 'grey98'), 
        legend.justification = .23
  ) +
  labs(
    title = "NBA Positions Distribution",
    subtitle = "Distribution of listed positions for rostered players on each NBA team",
    fill = "Three Positions",
    caption = 'Source: NBA | Chart: Davide Tissino')




ggsave("/Users/davidetissino/Desktop/Roster_3Pos.png", w = 7, h = 7, dpi = 'retina')






### Spotrac Data - 5 POS ====
# NO Two-Way or E10 
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
df <- salaries %>%
  select(-c(2, 4:7)) %>%
  group_by(team, Pos) %>% 
  summarise(
    count = n()
  )


# if in ecf, eastern conf, otherwise be western conf
df <- df %>%
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
    caption = '* Does not include Two-Way,\n
Source: Spotrac | Chart: Davide Tissino')


ggsave("/Users/davidetissino/Desktop/Roster_Positions.png", w = 7, h = 7, dpi = 'retina')

















