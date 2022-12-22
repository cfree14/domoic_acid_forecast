
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
tabledir <- "tables"
outdir <- "output"

# Read data
preds <- readRDS(file=file.path(outdir, "model_predictions.Rds"))
fits <- readRDS(file=file.path(outdir, "model_fits.Rds"))


# Build data
################################################################################

# Events to examine
events_use <- fits %>% 
  # Significant and decay
  filter(b<0 & pvalue<=0.05) %>% 
  # Calculate day when 30 ppm
  mutate(day_when_30ppm = ( log(30)-log(a) ) / b)

# DA to eval
da_ppm <- 31:200

# Build data
# Calculate days until 30 ppm
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
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=8),
                    plot.tag = element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_days, aes(x=domoic_ppm, y=days2action, color=k, group=event_id)) +
  geom_line() +
  # Plot median
  geom_line(data=data_days_med, mapping=aes(x=domoic_ppm, y=days2action), inherit.aes=F, color="black") +
  # Plot median points
  geom_point(data=data_days_med_pts, mapping=aes(x=domoic_ppm, y=days2action), inherit.aes=F, size=2) +
  geom_text(data=data_days_med_pts, mapping=aes(x=domoic_ppm, y=days2action, label=label), 
            inherit.aes=F, size=3, hjust=1.2, vjust=-0.2) +
  # Labels
  labs(x="Maximum domoic acid (ppm)", y="Days until first clean test\n(i.e., no samples above 30 ppm action threshold)") +
  # Axes
  scale_x_continuous(breaks=seq(30, 200, 10)) +
  scale_y_continuous(breaks=seq(0, 500, 50)) +
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





