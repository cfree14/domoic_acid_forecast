
# Plot data
plot_data <- function(data){
  
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
  
  # Extact stats
  pvalue <- anova(lmfit)$"Pr(>F)"[1]
  pvalue_rd <- round(pvalue,3)
  pvalue_text <- ifelse(pvalue_rd==0, "p < 0.001", paste0("p = ", pvalue_rd))
  rate_text <- paste0("Depuration rate = ", round(b*100,2), "% daily")
  halflife_text <- paste("Half life =", round(log(2)/-b), "days")
  stat_text <- paste(pvalue_text, rate_text, halflife_text, sep="\n")
  
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
  
  # Method leveraging LM features
  # This creates wild confifence intervals - I don't trust it
  # preds <- predict(lmfit, newdata=tibble(day=x), interval="confidence", level=0.95) %>% 
  #   # Exponentiate
  #   exp() %>% 
  #   # Convert to dataframe
  #   as.data.frame() %>% 
  #   # Rename
  #   rename(domoic_ppm_max=fit,
  #          domoic_ppm_max_lo=lwr,
  #          domoic_ppm_max_hi=upr) %>% 
  #   # Add day/date and rearrange
  #   mutate(day=x, 
  #          date=date0+day) %>% 
  #   select(day, date,  everything())
  
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
    # Plot fit
    geom_line(data=preds, aes(x=date, y=domoic_ppm_max)) +
    # Plot points
    geom_point(size=4) +
    # Plot reference line
    geom_hline(yintercept=30, linetype="dotted") +
    annotate(geom="text", x=date0, y=30, hjust=0, vjust=-1, label="Action threshold", color="grey40", size=4) +
    # Plot 30 ppm day
    geom_point(x=date30ppm, y=30, pch=21, fill="red", color="black", size=4) +
    geom_text(x=date30ppm, y=30, label=date30ppm, color="red", size=5, hjust=0, vjust=-1, inherit.aes = F) +
    # Plot regression text
    annotate(geom="text",
             x=max(preds$date),
             y=pmax(max(data$domoic_ppm_max), max(preds$domoic_ppm_max_hi)),
             label=stat_text,
             hjust=1, vjust=1, size=5) +
    # Labels
    labs(x="Survey date", y="Maximum domoic acid (ppm)") +
    # Y-axis
    scale_y_continuous(lim=c(0, NA)) +
    # X-axis
    scale_x_date(date_breaks="1 month", date_labels = "%b\n'%y") +
    # Theme
    theme_bw() + my_theme
  g
  
}




