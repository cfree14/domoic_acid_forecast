

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
  labs(x="Day relative to day of the first clean test\n(i.e., no samples above 30 ppm action threshold)", y="Maximum domoic acid (ppm)", tag="A") +
  # Y-axis
  scale_y_continuous(breaks=seq(0,200,10), lim=c(0,200)) +
  # Legend
  # scale_color_discrete(name="Event", guide="none") +
  scale_color_gradientn(name="Daily depuration rate", colors=RColorBrewer::brewer.pal(9, "Spectral"), labels=scales::percent) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.7, 0.8),
        legend.key.size = unit(0.3, "cm"))
g1

# Plot % decay
g2 <- ggplot(events_use, aes(x="", y=b)) +
  # geom_boxplot() +
  geom_violin(draw_quantiles = 0.5) +
  geom_jitter(alpha=0.9, pch=21, color="black", size=2, width=0.2, mapping=aes(fill=b)) +
  # Labels
  labs(x=" \n ", y="Daily depuration rate\n", tag="B") +
  # Axes
  scale_y_continuous(labels = scales::percent, lim=c(NA, 0), breaks=seq(-0.05, 0.0, 0.005)) +
  # Legend
  scale_fill_gradientn(name="Daily depuration rate", colors=RColorBrewer::brewer.pal(9, "Spectral"), labels=scales::percent) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none")
g2

# Plot half life
g3 <- ggplot(events_use, aes(x="", y=half_life)) +
  # geom_boxplot() +
  geom_violin(draw_quantiles = 0.5, ) +
  geom_jitter(alpha=0.9, pch=21, color="black", size=2, width=0.2, mapping=aes(fill=b)) +
  # Labels
  labs(x=" \n ", y="Half life (days)", tag="C") +
  # Y-axix
  scale_y_continuous(lim=c(0,NA), breaks=seq(0,200,25)) +
  # Legend
  scale_fill_gradientn(name="Daily depuration rate", colors=RColorBrewer::brewer.pal(9, "Spectral"), labels=scales::percent) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none")
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.5, 0.25, 0.25))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_depuration_rate.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


