
fracking = df_tracking


fracking$y2 = ifelse(fracking$angle <= 90, tan((fracking$angle*pi/180))*fracking$y + fracking$x, 
                     
                     
                        ifelse(fracking$angle <= 180, (tan((180-fracking$angle)*pi/180))* ((160/3-fracking$y)) + fracking$x,0)) ### xs become ys, etc

fracking$x2 = ifelse(fracking$angle <= 90, 0, 160/3)

fracking$qb_eyes_angle = ifelse(fracking$angle <=90, 180- fracking$angle,
                                ifelse(fracking$angle <=180, 180-fracking$angle,
                                       ifelse(fracking$angle <= 270, 540-fracking$angle,
                                              ifelse(fracking$angle <=360, 540-fracking$angle))))

fracking$qb_eyes_angle_lower = ifelse(fracking$angle <=90, 180-fracking$angle - 21,
                                ifelse(fracking$angle <=180, 180-fracking$angle - 21,
                                       ifelse(fracking$angle <= 270, 540-fracking$angle - 21,
                                              ifelse(fracking$angle <=360, 540-fracking$angle - 21))))
fracking$qb_eyes_angle_upper = ifelse(fracking$angle <=90, 180-fracking$angle + 21,
                                      ifelse(fracking$angle <=180, 180-fracking$angle + 21,
                                             ifelse(fracking$angle <= 270, 540-fracking$angle + 21,
                                                    ifelse(fracking$angle <=360, 540-fracking$angle + 21))))

#example_play = subset(fracking, gameId == 2018120209)
#example_play = subset(example_play, playId == 1905)
#example_play = subset(example_play, frameId <43)

passing_angles = fracking %>%
  filter(displayName == "Football", event == "pass_forward" | event == "pass_arrived")


passing_angles = passing_angles %>%
  group_by(gameId, playId) %>%
  mutate(thrown_from_x = lag(x, order_by=gameId), thrown_from_y = lag(y, order_by = gameId))

passing_angles = subset(passing_angles, !is.na(passing_angles$thrown_from_x))


passing_angles$angle_ball_thrown = ifelse(passing_angles$x > passing_angles$thrown_from_x & passing_angles$y > passing_angles$thrown_from_y,
                                            atan(((passing_angles$x - passing_angles$thrown_from_x) / (passing_angles$y - passing_angles$thrown_from_y)))*180/pi,
                                          ifelse(passing_angles$x > passing_angles$thrown_from_x & passing_angles$y < passing_angles$thrown_from_y,
                                                 180-(atan(((passing_angles$x - passing_angles$thrown_from_x) / (passing_angles$thrown_from_y - passing_angles$y)))*180/pi), 
                                                 ifelse(passing_angles$x == passing_angles$thrown_from_x, 90, "coords messed up")))


just_angles = passing_angles %>% dplyr::select(angle_ball_thrown, gameId, playId)
## couple messed up coords, pass even though it went backwards
just_angles$angle_ball_thrown = as.numeric(just_angles$angle_ball_thrown)
just_angles$angle_ball_thrown = round(just_angles$angle_ball_thrown, 2)

#qplot(just_angles$angle_ball_thrown)

fracking = left_join(fracking, just_angles, by = c("gameId"="gameId", "playId"="playId"))

plotting = fracking %>%
  filter(!is.na(angle_ball_thrown), !is.na(qb_eyes_angle), event =="pass_forward", angle <200)


qplot(plotting$angle, plotting$angle_ball_thrown)

reg = lm(angle_ball_thrown ~ angle, plotting) ## should we remove when playuers are looking backwards (idk)

plotting$predicted_throw_angle = predict(reg, plotting)
fracking$predicted_throw_angle = predict(reg, fracking)

fracking$qb_eyes_angle_lower = ifelse(fracking$predicted_throw_angle <=90, 180-fracking$predicted_throw_angle - 21,
                                      ifelse(fracking$predicted_throw_angle <=180, 180-fracking$predicted_throw_angle - 21,
                                             ifelse(fracking$predicted_throw_angle <= 270, 540-fracking$predicted_throw_angle - 21,
                                                    ifelse(fracking$predicted_throw_angle <=360, 540-fracking$predicted_throw_angle - 21))))
fracking$qb_eyes_angle_upper = ifelse(fracking$predicted_throw_angle <=90, 180-fracking$predicted_throw_angle + 21,
                                      ifelse(fracking$predicted_throw_angle <=180, 180-fracking$predicted_throw_angle + 21,
                                             ifelse(fracking$predicted_throw_angle <= 270, 540-fracking$predicted_throw_angle + 21,
                                                    ifelse(fracking$predicted_throw_angle <=360, 540-fracking$predicted_throw_angle + 21))))
