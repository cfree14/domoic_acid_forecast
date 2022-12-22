
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
# devtools::install_github("guiastrennec/ggplus")
library(tidyverse)
library(lubridate)
library(ggplus)

# Directories
indir <- "data/moore/processed"
outdir <- "data/da_samples/data"
plotdir <- "figures"
tabledir <- "tables"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_dcrab_surveys_with_event_id.Rds"))


# Build data
################################################################################

# Numbers of surveys required
nsurveys_req <- 5

# Event info
events <- data_orig %>% 
  # Stats
  group_by(state, event_id) %>% 
  rename(domoic_ppm_max1=domoic_ppm_max) %>% 
  summarize(nsurveys=n(),
            event_day_max=max(event_day),
            domoic_ppm_max=max(domoic_ppm_max1),
            domoic_ppm_max_sd=sd(domoic_ppm_max1)) %>% 
  ungroup() %>% 
  # Filter
  filter(nsurveys >= nsurveys_req & domoic_ppm_max_sd!=0) %>% 
  # Arrange
  arrange(state, desc(domoic_ppm_max)) %>% 
  mutate(event_id=factor(event_id, levels=event_id))

# Build data
data <- data_orig %>% 
  # Reduce to useable event
  filter(event_id %in% events$event_id) %>% 
  # Order events
  mutate(event_id=factor(event_id, levels=levels(events$event_id))) %>% 
  # Arrange
  arrange(event_id, event_day) %>% 
  # Change 0s to 1s
  mutate(domoic_ppm_max=pmax(domoic_ppm_max, 1))


# Fit exponential decay
################################################################################

# Loop through events
i <- 1
for(i in 1:nrow(events)){
  
  # Event info
  print(i)
  event_do <- events$event_id[i]
  event_day_max <- events$event_day_max[i]
  
  # Event data
  sdata <- data %>% 
    filter(event_id==event_do)
  
  # Fit model
  lmfit <- lm(log(domoic_ppm_max)~event_day, sdata)
  summary(lmfit)
  
  # Make predictions
  x <- 0:event_day_max
  preds_list <- predict(lmfit, newdata=tibble(event_day=x), se.fit = T)
  preds <- tibble(event_id=event_do,
                  event_day=x,
                  domoic_ppm_max=exp(preds_list$fit),
                  domoic_ppm_max_lo=exp(preds_list$fit-preds_list$se.fit*1.96),
                  domoic_ppm_max_hi=exp(preds_list$fit+preds_list$se.fit*1.96))
  
  # Extract parameters
  # Y=a*e^(bx)
  # log(Y)=log(a) + Xb
  a <- exp(coef(lmfit)[1])
  b <- coef(lmfit)[2]
  pvalue <- freeR::pval(lmfit)
  r2 <- freeR::r2(lmfit)
  r2_adj <- summary(lmfit)$adj.r.squared
  df_row <- tibble(event_id=event_do,
                   a=a, b=b, pvalue=pvalue, r2=r2, r2_adj=r2_adj)
  
  # Plot fit
  g <- ggplot() +
    # Plot CI
    geom_ribbon(data=preds, mapping=aes(x=event_day, ymin=domoic_ppm_max_lo, ymax=domoic_ppm_max_hi), fill="grey90") +
    # Plot fit
    geom_line(data=preds, mapping=aes(x=event_day, y=domoic_ppm_max), lwd=1.2) +
    # Plot data
    geom_point(data=sdata, mapping=aes(x=event_day, y=domoic_ppm_max), pch=21, color="black", fill="grey40", size=2) +
    # Labels
    labs(x="Event day", y="Max domoic acid (ppm)", title=event_do) +
    # Axes
    lims(y=c(0, NA)) +
    # Theme
    theme_bw()
  g
  
  # Record data
  if(i==1){
    params <- df_row
    preds_out <- preds
  }else{
    params <- bind_rows(params, df_row)
    preds_out <- bind_rows(preds_out, preds)
  }
  
}


# Format output
################################################################################

# Prediction stats (used for plotting)
pred_stats <- preds_out %>% 
  group_by(event_id) %>% 
  summarize(domoic_ppm_max_pred=max(domoic_ppm_max_hi),
            event_day_max=max(event_day)) %>% 
  ungroup()

# Format parameter estimates
params_out <- params %>% 
  # Half-life
  mutate(half_life=log(2)/-b) %>% 
  # Formatted parameters
  mutate(pvalue_rd=round(pvalue, digits=3)) %>% 
  # Plot text
  mutate(plot_text=ifelse(pvalue_rd==0, "p<0.001", paste0("p=", pvalue_rd))) %>% 
  # Add obs / prediction stats for plotting
  left_join(pred_stats %>% select(event_id, event_day_max, domoic_ppm_max_pred), by="event_id") %>% 
  left_join(events %>% select(event_id, domoic_ppm_max), by="event_id") %>% 
  mutate(domoic_ppm_max_plot=pmax(domoic_ppm_max, domoic_ppm_max_pred)) %>% 
  select(-c(domoic_ppm_max, domoic_ppm_max_pred))


# Plot fits
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=6),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   legend.position = c(0.8, 0.05),
                   legend.key.size = unit(0.3, "cm"))

# Plot data
g <- ggplot(data, aes(x=event_day, y=domoic_ppm_max)) +
  # Facet (don't facet here if using multi-page plot)
  facet_wrap(~event_id, ncol=4, scales="free") +
  # Fix CIs
  geom_ribbon(data=preds_out, mapping=aes(x=event_day, ymin=domoic_ppm_max_lo, ymax=domoic_ppm_max_hi), fill="grey90", color=NA) +
  # Fit predictions
  geom_line(data=preds_out, mapping=aes(x=event_day, y=domoic_ppm_max), color="black") +
  # Points
  geom_point() +
  # Plot text
  geom_text(data=params_out, mapping=aes(x=event_day_max, y=domoic_ppm_max_plot*0.95, label=plot_text), hjust=1, size=2.4) +
  # Reference line
  # geom_hline(yintercept=30, linetype="dotted") +
  # Labels
  labs(x="Day of event", y="Maximum domoic acid (ppm)") +
  # Axes
  scale_y_continuous(lim=c(0,NA)) +
  # Legend
  scale_color_discrete(name="State", guide="none") +
  # Theme
  theme_bw() + my_theme
#g

ggsave(g, filename=file.path(plotdir, "AppendixA_decay_curves.pdf"),
       width=8.5, height=25, units="in", dpi=600)

# Export plot
# pdf(file.path(plotdir, "AppendixA_decay_curves.pdf"), width=8.5, height=11)
# gg <- ggplus::facet_multiple(plot=g, facets="event_id", ncol = 4, nrow = 6)
# dev.off()

# Export
# ggsave(g, filename=file.path(plotdir, "AppendixA_decay_curves.pdf"), 
#        width=8.5, height=11.5, units="in", dpi=600)


# Dense version

# Plot data
g <- ggplot(data, aes(x=event_day, y=domoic_ppm_max)) +
  # Facet (don't facet here if using multi-page plot)
  facet_wrap(~event_id, ncol=8, scales="free") +
  # Fix CIs
  geom_ribbon(data=preds_out, mapping=aes(x=event_day, ymin=domoic_ppm_max_lo, ymax=domoic_ppm_max_hi), fill="grey90", color=NA) +
  # Fit predictions
  geom_line(data=preds_out, mapping=aes(x=event_day, y=domoic_ppm_max), color="black") +
  # Points
  geom_point(size=0.5) +
  # Plot text
  geom_text(data=params_out, 
            mapping=aes(x=event_day_max, y=domoic_ppm_max_plot*0.9, label=plot_text), 
            hjust=1, size=1.8) +
  # Reference line
  # geom_hline(yintercept=30, linetype="dotted") +
  # Labels
  labs(x="Day of event", y="Maximum domoic acid (ppm)") +
  # Axes
  scale_y_continuous(lim=c(0,NA)) +
  # Legend
  scale_color_discrete(name="State", guide="none") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text=element_text(size=4),
        axis.title=element_text(size=5),
        strip.text=element_text(size=4))
#g

ggsave(g, filename=file.path(plotdir, "AppendixA_decay_curves.png"),
       width=6.5, height=7.5, units="in", dpi=600)




# Plot fits in 1 plot
################################################################################

# Plot all
g <- ggplot(preds_out, aes(x=event_day, y=domoic_ppm_max, color=event_id)) +
  geom_line() +
  # Reference line
  geom_hline(yintercept=30, linetype="dotted") +
  # Labels
  labs(x="Event day", y="Max domoic acid (ppm)") +
  # Y-axis
  scale_y_continuous(breaks=seq(0,200,10)) +
  # Legend
  scale_color_discrete(name="Event", guide="none") +
  # Theme
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
g



# Plot all in 1 plot (centered)
################################################################################

# Events to examine
events_use <- params_out %>% 
  # Significant and decay
  filter(b<0 & pvalue<=0.05) %>% 
  # Calculate day when 30 ppm
  mutate(day_when_30ppm = ( log(30)-log(a) ) / b)

# Stats for MS
median(events_use$b*100)
median(events_use$half_life)

# Build data
x <- 1
data_centered <- purrr::map_df(1:nrow(events_use), function(x){
  
  # Extract parameters of interest
  event_id <- events_use$event_id[x]
  a <- events_use$a[x]
  b <- events_use$b[x]
  day30ppm <- events_use$day_when_30ppm[x]
  
  # Days to predict
  days_do <- -100:100+day30ppm
  days_do_centered <- days_do-day30ppm
  
  # Predictions
  da_ppm_max <- a*exp(b*days_do)
  
  # Record
  df <- tibble(event_id=event_id,
               b=b,
               event_day_centered=days_do_centered,
               da_ppm_max=da_ppm_max)
  
})

# Build median
data_centered_med <- data_centered %>% 
  mutate(event_day_centered=round(event_day_centered)) %>% 
  group_by(event_day_centered) %>% 
  summarize(da_ppm_max=median(da_ppm_max, na.rm=T)) %>% 
  ungroup()

# Calculate median reference points
# data_days_med_pts <- data_days_med %>% 
#   filter(domoic_ppm %in% seq(60, 180, 30)) %>% 
#   mutate(label=paste(round(days2action), "days"))

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    plot.tag = element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot all
g1 <- ggplot(data_centered, aes(x=event_day_centered, y=da_ppm_max, color=b, group=event_id)) +
  geom_line() +
  # Plot median
  geom_line(data=data_centered_med, 
            mapping=aes(x=event_day_centered, y=da_ppm_max),
            inherit.aes = F, color="black", lwd=1) +
  # Reference line
  geom_hline(yintercept=30, linetype="dotted") +
  # Labels
  labs(x="Days of first clean test\n(i.e., no samples above 30 ppm action threshold)", y="Maximum domoic acid (ppm)", tag="A") +
  # Y-axis
  scale_y_continuous(breaks=seq(0,200,10), lim=c(0,200)) +
  # Legend
  # scale_color_discrete(name="Event", guide="none") +
  scale_color_gradientn(name="Daily depuration rate", colors=RColorBrewer::brewer.pal(9, "Spectral"), labels=scales::percent) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8, 0.8),
        legend.key.size = unit(0.3, "cm"))
g1

# Plot % decay
g2 <- ggplot(events_use, aes(x="", y=b, fill=b)) +
  # geom_boxplot() +
  geom_violin(draw_quantiles = 0.5, ) +
  geom_jitter(alpha=0.9, pch=21, color="black", size=2, width=0.2) +
  # Labels
  labs(x=" \n ", y="Daily depuration rate\n", tag="B") +
  # Y-axix
  scale_y_continuous(labels = scales::percent) +
  # Legend
  scale_fill_gradientn(name="Daily depuration rate", colors=RColorBrewer::brewer.pal(9, "Spectral"), labels=scales::percent) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Plot half life
g3 <- ggplot(events_use, aes(x="", y=half_life, fill=b)) +
  # geom_boxplot() +
  geom_violin(draw_quantiles = 0.5, ) +
  geom_jitter(alpha=0.9, pch=21, color="black", size=2, width=0.2) +
  # Labels
  labs(x=" \n ", y="Half life (days)", tag="C") +
  # Y-axix
  scale_y_continuous(lim=c(0,NA)) +
  # Legend
  scale_fill_gradientn(name="Daily depuration rate", colors=RColorBrewer::brewer.pal(9, "Spectral"), labels=scales::percent) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.5, 0.25, 0.25))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_depuration_rate.png"), 
       width=6.5, height=3.5, units="in", dpi=600)



# Calculate days until 30 ppm
################################################################################

# DA to eval
da_ppm <- 31:200

# Build data
x <- 1
data_days <- purrr::map_df(1:nrow(events_use), function(x){
  
  # Get info
  event_do <- events_use$event_id[x]
  k <- events_use$b[x]
  
  # Calculate days to 30 ppm action thresh
  days2action <- log(da_ppm/30)/-k
  
  # Record
  df <- tibble(event_id=event_do,
               k=k,
               domoic_ppm=da_ppm,
               days2action=days2action)
  
})

# Calculate median
data_days_med <- data_days %>% 
  group_by(domoic_ppm) %>% 
  summarize(days2action=median(days2action)) %>% 
  ungroup()

# Calculate median reference points
data_days_med_pts <- data_days_med %>% 
  filter(domoic_ppm %in% seq(60, 180, 30)) %>% 
  mutate(label=paste(round(days2action), "days"))


# Plot data
g <- ggplot(data_days, aes(x=domoic_ppm, y=days2action, color=k, group=event_id)) +
  geom_line() +
  # Plot median
  geom_line(data=data_days_med, mapping=aes(x=domoic_ppm, y=days2action), inherit.aes=F, color="black") +
  # Plot median points
  geom_point(data=data_days_med_pts, mapping=aes(x=domoic_ppm, y=days2action), inherit.aes=F, size=2) +
  geom_text(data=data_days_med_pts, mapping=aes(x=domoic_ppm, y=days2action, label=label), 
            inherit.aes=F, size=2.5, hjust=1.2, vjust=-0.2) +
  # Labels
  labs(x="Maximum domoic acid (ppm)", y="Days until first clean test\n(i.e., no samples above 30 ppm action threshold)") +
  # X-axis
  scale_x_continuous(breaks=seq(30, 200, 10)) +
  # Legend
  scale_color_gradientn(name="Daily depuration rate", 
                        colors=RColorBrewer::brewer.pal(9, "Spectral"), 
                        labels=scales::percent) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.2,0.8),
        legend.key.size = unit(0.4, "cm"))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig3_time_until_1st_clean.png"), 
       width=4.5, height=4.5, units="in", dpi=600)





