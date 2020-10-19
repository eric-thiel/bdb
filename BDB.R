#Loading pre-installed libraries
library(tidyverse)
library(gganimate)
library(cowplot)
library(repr)
library(gifski)
library(teamcolors)
library(nflfastR)

#turning off warnings
options(warn=-1)

#setting plot width and height
options(repr.plot.width=15, repr.plot.height = 10)


#includes schedule info for games
df_games <- read_csv("~/Documents/BigDataBowl/nfl-big-data-bowl-2021/games.csv",
                     col_types = cols())

#includes play-by-play info on specific plays
df_plays <- read_csv("~/Documents/BigDataBowl/nfl-big-data-bowl-2021/plays.csv",
                     col_types = cols())

#includes background info for players
df_players <- read_csv("~/Documents/BigDataBowl/nfl-big-data-bowl-2021/players.csv",
                       col_types = cols())

#weeks of NFL season
weeks <- seq(1, 17)

#blank dataframe to store tracking data
df_tracking <- data.frame()

#iterating through all weeks
for(w in weeks){
  
  #temperory dataframe used for reading week for given iteration
  df_tracking_temp <- read_csv(paste0("~/Documents/BigDataBowl/nfl-big-data-bowl-2021/week",w,".csv"),
                               col_types = cols())
  
  #storing temporary dataframe in full season dataframe
  df_tracking <- bind_rows(df_tracking_temp, df_tracking)                            
  
}

## certainly need to change the direction, orientation.


#Standardizing tracking data so its always in direction of offense vs raw on-field coordinates.
df_tracking <- df_tracking %>%
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y))


df_tracking$Dir_std_1 = NULL
df_tracking$Dir_std_2 = NULL
df_tracking = df_tracking%>%
  mutate(dir_std = ifelse(playDirection == "left" & dir <= 90, 180 + dir,
                          ifelse(playDirection == "left" & dir <180, 180+dir,
                                 ifelse(playDirection == "left"& dir <= 270, dir-180,
                                        ifelse(playDirection =="left" & dir <= 360, dir-180,dir )))))

df_tracking = df_tracking%>%
  mutate(o_std = ifelse(playDirection == "left" & o <= 90, 180 + o,
                          ifelse(playDirection == "left" & o <180, 180+o,
                                 ifelse(playDirection == "left"& o <= 270, o-180,
                                        ifelse(playDirection =="left" & o <= 360, o-180,o )))))
df_tracking$angle = ifelse(df_tracking$position == "QB", df_tracking$o_std, NA)
## declaring values for field coordinates

# General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3


#picking a random play
set.seed(Sys.time())

example_play <- df_plays %>%
  filter(#4th quarter
    quarter < 4,
    
    #shotgun formation
   # offenseFormation == "SHOTGUN",
    
    playResult >10,
    
    #complete passes
   # passResult == "C",
    
    #third down
    down < 4) %>%
  
  select(gameId, playId, playDescription) %>%
  
  sample_n(1)


#merging games data to play
example_play <- inner_join(example_play,
                           df_games,
                           by = c("gameId" = "gameId"))

#merging tracking data to play
example_play <- inner_join(example_play,
                           df_tracking,
                           by = c("gameId" = "gameId",
                                  "playId" = "playId"))

h = nflfastR::teams_colors_logos

example_play = left_join(example_play, h[c("team_abbr","team_color")], by = c("homeTeamAbbr"="team_abbr"))
example_play = example_play %>% rename("home_color"="team_color")
example_play = left_join(example_play, h[c("team_abbr","team_color")], by = c("visitorTeamAbbr"="team_abbr"))
example_play = example_play %>% rename("away_color"="team_color")

home_color = head(example_play$home_color,1)
away_color = head(example_play$away_color,1)

#DEN vs OAK
# first is Den, second is Oak



cols_fill <- c(away_color, "#663300", home_color)
cols_col <- c("#000000", "#663300", "#000000")

plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(example_play$playDescription[1])))

# Specific boundaries for a given play
ymin <- max(round(min(example_play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example_play$x, na.rm = TRUE) + 10, -1), 120)

#hash marks
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

print(head(example_play$playDescription,1))

#plotting
ggplot() +
  
  #setting size and color parameters
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = cols_fill, guide = FALSE) + 
  scale_colour_manual(values = cols_col, guide = FALSE) +
  
  #adding hash marks
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  
  #adding yard lines
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  
  #adding field yardline text
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  
  #adding field exterior
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  
  #adding players
  geom_point(data = example_play, aes(x = (xmax-y),
                                      y = x, 
                                      shape = team,
                                      fill = team,
                                      group = nflId,
                                      size = team,
                                      colour = team), 
             alpha = 0.7) +  
  
  #adding jersey numbers
  geom_text(data = example_play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  
  #geom_spoke(data = example_play, aes(x = (xmax-y),
  #                                        y = x, angle = angle), radius = 15) + 
  
  #applying plot limits
  ylim(ymin, ymax) + 
  coord_fixed() +
  
  #applying theme
  theme_nothing() + 
  theme(plot.title = element_text()) +
  
  #titling plot with play description
  labs(title = plot_title) +
  
  #setting animation parameters
  transition_time(frameId)  +
  ease_aes('linear') + 
  NULL















