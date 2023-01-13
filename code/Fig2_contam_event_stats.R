

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
tabledir <- "tables"
datadir <- "data/da_samples/data"

# Read data
data_orig <- readRDS(file=file.path(datadir, "CA_OR_WA_2000_2020_biotoxin_data_dcrab_surveys_with_event_id.Rds"))


# Build data
################################################################################

# To-do: ignore

# Numbers of surveys required
nsurveys_req <- 5

# Build event stats
data <- data_orig %>% 
  # Event stats
  group_by(state, event_id) %>% 
  summarize(n=n(),
            date1=min(date),
            date2=max(date),
            da_ppm_max=max(domoic_ppm_max),
            da_ppm_sd=sd(domoic_ppm_max)) %>% 
  ungroup() %>% 
  mutate(duration_days=difftime(date2, date1, units="days") %>% as.numeric()) %>% 
  # Reduce to useable
  filter(n >= nsurveys_req & da_ppm_sd!=0) %>% 
  # Order states
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev()))


# Plot data
################################################################################

# Colors
ca_color <- RColorBrewer::brewer.pal(9, "Reds")[5]
or_color <- RColorBrewer::brewer.pal(9, "Blues")[5]
wa_color <- RColorBrewer::brewer.pal(9, "Greens")[5]
state_colors <- c(ca_color, or_color, wa_color) %>% rev()

# Base theme
base_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=9),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    plot.tag = element_text(size=10),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))


# Plot intensity
g1 <- ggplot(data, aes(x=da_ppm_max, fill=state)) +
  geom_density(alpha=0.5) +
  # Labels
  labs(x="Maximum domoic acid concentration (ppm)", y="Density", tag="A") +
  # X-axes
  scale_x_continuous(breaks=seq(0,390, 30)) +
  # Legend
  scale_fill_manual(name="", values=state_colors) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8, 0.8),
        legend.key.size = unit(0.4, "cm"))
g1

# Plot duration
g2 <- ggplot(data, aes(x=duration_days, fill=state)) +
  geom_density(alpha=0.5) +
  # Labels
  labs(x="Duration of monitoring (days)", y="Density", tag="B") +
  # X-axes
  scale_x_continuous(breaks=seq(0,390, 30), lim=c(0,NA)) +
  # Legend
  scale_fill_manual(name="", values=state_colors) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)


# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_contam_event_stats.png"), 
       width=6.5, height=3, units="in", dpi=600)


