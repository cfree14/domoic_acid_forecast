
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/moore/processed"
outdir <- "data/da_samples/data"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_dcrab_surveys.Rds"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Useable surveys
  filter(n>=3) %>% 
  # Add event id
  mutate(event_id=case_when(state=="Oregon" ~ paste(season, state_abbrev, zone, sep="-"),
                            T ~ paste(season, state_abbrev, site, sep="-"))) %>% 
  # Arrange
  select(state:site, lat_dd, long_dd, survey_id, event_id, everything()) 

# Inspect
freeR::complete(data)

# Compue event statistics (# of surveys per event)
events <- data %>% 
  group_by(event_id) %>% 
  summarise(nsurveys=n()) %>% 
  ungroup()

# Add event statistics to data
data_exp <- data %>% 
  # Add event stats
  left_join(events) %>% 
  # Classify as useable event (or not)
  mutate(event_use=ifelse(nsurveys > 5, "yes", "no"),
         event_id_use=ifelse(event_use=="yes", event_id, NA)) %>% 
  # Calculate day 0 and time since day 0
  group_by(event_id) %>% 
  mutate(event_day0=min(date),
         event_day=difftime(date, event_day0, units = "day") %>% as.numeric()) %>% 
  ungroup()


# Washington
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   plot.title = element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(data_exp %>% filter(state=="Washington"),
            aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=event_id_use)) +
  geom_point(pch=21, alpha=0.5) +
  # Labels
  labs(x="Date", y="Latitude (°N)", title="Washington") +
  # Color legend (events)
  scale_fill_manual(name="", guide="none",
                    na.value = "grey80",
                    values=rainbow(n_distinct(data$event_id)) %>% sample()) +
  # Size legend (DA conc)
  scale_size_continuous(name="Maximum\ndomoic acid (ppm)") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS2_event_identification_full_washington.png"),
       width=6.5, height=3.5, units="in", dpi=600)

# Plot data
g <- ggplot(data_exp %>% filter(state=="Washington" & season >=2014),
            aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=event_id_use)) +
  geom_point(pch=21, alpha=0.5) +
  # Labels
  labs(x="Date", y="Latitude (°N)", title="Washington") +
  # Color legend (events)
  scale_fill_manual(name="", guide="none",
                    na.value = "grey80",
                    values=rainbow(n_distinct(data$event_id)) %>% sample()) +
  # Size legend (DA conc)
  scale_size_continuous(name="Maximum\ndomoic acid (ppm)") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS3_event_identification_zoom_washington.png"),
       width=6.5, height=3.5, units="in", dpi=600)

# Oregon
################################################################################

# Plot data
g <- ggplot(data_exp %>% filter(state=="Oregon"),
            aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=event_id_use)) +
  geom_point(pch=21, alpha=0.5) +
  # Labels
  labs(x="Date", y="Latitude (°N)", title="Oregon") +
  # Color legend (events)
  scale_fill_manual(name="", guide="none",
                    na.value = "grey80",
                    values=rainbow(n_distinct(data$event_id)) %>% sample()) +
  # Size legend (DA conc)
  scale_size_continuous(name="Maximum\ndomoic acid (ppm)") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS2_event_identification_full_oregon.png"),
       width=6.5, height=3.5, units="in", dpi=600)

# California
################################################################################

# Plot data
g <- ggplot(data_exp %>% filter(state=="California" & season >=1999),
            aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=event_id_use)) +
  geom_point(pch=21, alpha=0.5) +
  # Labels
  labs(x="Date", y="Latitude (°N)", title="California") +
  # Color legend (events)
  scale_fill_manual(name="", guide="none",
                    na.value = "grey80",
                    values=rainbow(n_distinct(data$event_id)) %>% sample()) +
  # Size legend (DA conc)
  scale_size_continuous(name="Maximum\ndomoic acid (ppm)") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS2_event_identification_full_california.png"),
       width=6.5, height=3.5, units="in", dpi=600)

# Plot data
g <- ggplot(data_exp %>% filter(state=="California" & season >=2014 & season <= 2018),
            aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=event_id_use)) +
  geom_point(pch=21, alpha=0.5) +
  # Labels
  labs(x="Date", y="Latitude (°N)", title="California") +
  # Axes
  scale_y_continuous(lim=c(36.5, 42), breaks=36:42) +
  # Color legend (events)
  scale_fill_manual(name="", guide="none",
                    na.value = "grey80",
                    values=rainbow(n_distinct(data$event_id)) %>% sample()) +
  # Size legend (DA conc)
  scale_size_continuous(name="Maximum\ndomoic acid (ppm)") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS3_event_identification_zoom_california.png"),
       width=6.5, height=3.5, units="in", dpi=600)



# Inspect events
################################################################################

# Plot data
g <- ggplot(data_exp %>% filter(season>=1999), 
            aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=event_id_use)) +
  geom_point(pch=21, alpha=0.5) +
  # State line
  geom_hline(yintercept=c(42, 46.25, 48.48)) +
  geom_hline(yintercept=38+46.125/60, linetype="dashed") +
  # Labels
  labs(x="Test date", y="Latitude (°N)") +
  # Axes
  scale_x_date(breaks=seq(ymd("2000-01-02"), ymd("2023-01-01"), by="1 year"), date_labels="%Y") +
  scale_y_continuous(breaks=seq(32, 50, 2)) +
  # Color legend (events)
  scale_fill_manual(name="", guide="none",
                     na.value = "grey80",
                     values=rainbow(n_distinct(data$event_id)) %>% sample()) +
  # Size legend (DA conc)
  scale_size_continuous(name="Maximum\ndomoic acid (ppm)") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.key.size = unit(0.5, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS2_event_identification_full.png"), 
       width=6.5, height=7.5, units="in", dpi=600)

# Plot data
g <- ggplot(data_exp %>% filter(season>=2015), 
            aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=event_id_use)) +
  geom_point(pch=21, alpha=0.5) +
  # State line
  geom_hline(yintercept=c(42, 46.25, 48.48)) +
  geom_hline(yintercept=38+46.125/60, linetype="dashed") +
  # Labels
  labs(x="Test date", y="Latitude (°N)") +
  # Axes
  scale_x_date(breaks=seq(ymd("2000-01-02"), ymd("2023-01-01"), by="1 year"), date_labels="%Y") +
  scale_y_continuous(breaks=seq(32, 50, 2)) +
  # Color legend (events)
  scale_fill_manual(name="", guide="none",
                    na.value = "grey80",
                    values=rainbow(n_distinct(data$event_id)) %>% sample()) +
  # Size legend (DA conc)
  scale_size_continuous(name="Maximum\ndomoic acid (ppm)") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.key.size = unit(0.5, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS3_event_identification_zoom.png"), 
       width=6.5, height=7.5, units="in", dpi=600)


# Event decay
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
g <- ggplot(data_exp %>% filter(event_use=="yes"),
            aes(x=event_day, y=domoic_ppm_max, color=state)) +
  # Facet
  facet_wrap(~event_id, ncol=8, scales="free") +
  # Points
  geom_point() +
  # Reference line
  geom_hline(yintercept=30, linetype="dotted") +
  # Labels
  labs(x="Day of event", y="Maximum domoic acid (ppm)") +
  # Legend
  scale_color_discrete(name="State") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS4_event_decay.png"), 
       width=6.5, height=7.5, units="in", dpi=600)

