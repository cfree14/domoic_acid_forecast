
# Plot data
plot_data_hist <- function(data, fits){
  
  # Fit model and build trajectory
  ###################################################
  
  # Format data
  data1 <- data %>% 
    # Add event day
    mutate(day0=min(date),
           day=difftime(date, day0, units="days") %>% as.numeric())
  
  # Fit model
  lmfit <- lm(log(domoic_ppm_max) ~ day, data=data1)
  
  # Get params
  a <- coef(lmfit)[1] %>% exp()
  b <- coef(lmfit)[2]
  
  # What day would it hit 30? What about 5?
  day30ppm <- round(log(30/a)/b)
  day5ppm <-  round(log(5/a)/b)
  
  # Dates
  date0 <- min(data$date)
  date30ppm <- date0 + day30ppm
  date5ppm <- date0 + day5ppm
  
  # Make predictions
  x <- 0:day5ppm
  preds_list <- predict(lmfit, newdata=tibble(day=x), se.fit=T)
  preds <- tibble(day=x,
                  date=date0+x,
                  domoic_ppm_max=preds_list$fit %>% exp(),
                  domoic_ppm_max_lo=(preds_list$fit-preds_list$se.fit*1.96) %>% exp(),
                  domoic_ppm_max_hi=(preds_list$fit+preds_list$se.fit*1.96) %>% exp())
  
  # Build historical trajectories
  ###################################################
  
  # What is the current prediction?
  day_curr <- max(data1$day)
  ppm_curr <- predict(lmfit, newdata=tibble(day=day_curr)) %>% exp()
  
  # Build fits
  fits1 <- fits %>% 
    # Useable fits
    filter(b<=0 & pvalue<=0.05) %>% 
    # What day would the trajectory hit the current ppm?
    mutate(day_curr=round(log(ppm_curr/a)/b) %>% as.numeric())
  
  # Build data
  x <- 1
  hist_preds <- purrr::map_df(1:nrow(fits1), function(x){
    
    # Extract params
    a <- fits1$a[x]
    b <- fits1$b[x]
    
    # Build days to predict to
    day_targ <- fits1$day_curr[x]
    day0_targ <- day_targ - day_curr
    day2_targ <- day_targ + (day5ppm - day_curr)
    days <- day0_targ:day2_targ
    
    # Make predictions
    y <- a*exp(b*days)
    
    # Build data frame
    df <- tibble(event_id=fits$event_id[x],
                 k =b,
                 date=seq(date0, date5ppm, by="1 day"),
                 day=days, 
                 domoic_ppm_max=y)
    
    
  })
  
  # Plot data
  ###################################################
  
  # Theme
  my_theme <- theme(axis.text=element_text(size=14),
                    axis.title=element_text(size=16),
                    legend.text=element_text(size=14),
                    legend.title=element_text(size=16),
                    strip.text=element_text(size=14),
                    plot.title=element_text(size=16),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Plot data
  g <- ggplot(data, aes(x=date, y=domoic_ppm_max)) +
    # Plot CI
    geom_ribbon(data=preds, aes(x=date, ymin=domoic_ppm_max_lo, ymax=domoic_ppm_max_hi), fill="grey85") +
    # Plot historic
    geom_line(data=hist_preds, 
              mapping=aes(x=date, y=domoic_ppm_max, color=k, group=event_id), 
              inherit.aes = F) +
    # Plot fit
    geom_line(data=preds, aes(x=date, y=domoic_ppm_max), size=2) +
    # Plot points
    geom_point(size=5) +
    # Plot reference line
    geom_hline(yintercept=30, linetype="dotted") +
    annotate(geom="text", x=date0, y=30, hjust=0, vjust=-1, label="Action threshold", color="grey40", size=4) +
    # Plot 30 ppm day
    geom_point(x=date30ppm, y=30, pch=21, fill="red", color="black", size=5) +
    geom_text(x=date30ppm, y=30, label=date30ppm, color="red", size=5, hjust=0, vjust=-1, inherit.aes = F) +
    # Labels
    labs(x="Survey date", y="Maximum domoic acid (ppm)") +
    # Y-axis
    scale_y_continuous(lim=c(0, 200)) +
    # X-axis
    scale_x_date(date_breaks="1 month", date_labels = "%b\n'%y") +
    # Legend
    scale_color_gradientn(name="Daily depuration rate",
                      colors=RColorBrewer::brewer.pal(9, "Spectral"),
                      labels=scales::percent) +
    guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + my_theme +
    theme(legend.position = c(0.8, 0.8))
  g
  
}




