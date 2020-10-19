
fracking = df_tracking


fracking$y_end = ifelse(fracking$angle <= 90, tan(fracking$angle)*fracking$x, 
                        ifelse(fracking$angle <= 180, tan(180-fracking$angle)* (160/3-fracking$x),0))

fracking$x_end = ifelse(fracking$angle <= 90, 0, 160/3)


