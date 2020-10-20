
fracking = df_tracking


fracking$y2 = ifelse(fracking$angle <= 90, tan((fracking$angle*pi/180))*fracking$y + fracking$x, 
                     
                     
                        ifelse(fracking$angle <= 180, (tan((180-fracking$angle)*pi/180))* ((160/3-fracking$y)) + fracking$x,0)) ### xs become ys, etc

fracking$x2 = ifelse(fracking$angle <= 90, 0, 160/3)

fracking$qb_eyes_angle = ifelse(fracking$angle <=90, 180- fracking$angle,
                                ifelse(fracking$angle <=180, 180-fracking$angle,
                                       ifelse(fracking$angle <= 270, 540-fracking$angle,
                                              ifelse(fracking$angle <=360, 540-fracking$angle))))

#example_play = subset(fracking, gameId == 2018120209)
#example_play = subset(example_play, playId == 1905)
#example_play = subset(example_play, frameId <43)


#picking a random play
set.seed(Sys.time())

example_play <- df_plays %>%
  filter(#4th quarter
    quarter < 4,
    
    #shotgun formation
     offenseFormation == "SHOTGUN",
    
    playResult >30,
    
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
                           fracking,
                           by = c("gameId" = "gameId",
                                  "playId" = "playId"))

h = nflfastR::teams_colors_logos

example_play = left_join(example_play, h[c("team_abbr","team_color")], by = c("homeTeamAbbr"="team_abbr"))
example_play = example_play %>% rename("home_color"="team_color")
example_play = left_join(example_play, h[c("team_abbr","team_color")], by = c("visitorTeamAbbr"="team_abbr"))
example_play = example_play %>% rename("away_color"="team_color")

home_color = head(example_play$home_color,1)
away_color = head(example_play$away_color,1)


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
  
  geom_spoke(data = example_play, aes(x = (xmax-y),
                                          y = x, angle = (qb_eyes_angle*pi/180)), radius = 15) + 
  

  #geom_segment(data = example_play, aes(x = (xmax-y), y = x, xend = x2, yend = y2),
  #             arrow = arrow(length = unit(0.25, "cm"),
  #                           type = "closed")) +
  
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




