
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


# OREGON
################################################################################
################################################################################

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce to Oregon
  filter(state=="Oregon" & n>=3) %>% 
  # Simplify
  select(state_abbrev, season, zone, site, lat_dd, date, survey_id, n, domoic_ppm_max) %>% 
  # Add event id
  mutate(event_id=paste(season, state_abbrev, zone, sep="-")) %>% 
  # Arrange
  arrange(site, date)

# Event stats
################################################################################

# Events
events <- data %>% 
  group_by(event_id) %>% 
  summarise(nsurveys=n()) %>% 
  ungroup()

# Extend data based on event stats
################################################################################

# Expand data
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

# Inspect events
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
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
g <- ggplot(data_exp, aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=event_id_use)) +
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
ggsave(g, filename=file.path(plotdir, "FigX_oregon_event_identification.png"), 
       width=6.5, height=3.5, units="in", dpi=600)

# Event decay
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
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
g <- ggplot(data_exp %>% filter(event_use=="yes"),
            aes(x=event_day, y=domoic_ppm_max)) +
  # Facet
  facet_wrap(~event_id, ncol=6) +
  # Ribbon
  # Reference line
  geom_hline(yintercept=30, linetype="dotted") +
  # Points
  geom_point() +
  # Labels
  labs(x="Day of event", y="Maximum domoic acid (ppm)") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_oregon_event_decay_proof_of_concept.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



# WASHINGTON
################################################################################
################################################################################

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce to Oregon
  filter(state=="Washington" & n>=3) %>% 
  # Simplify
  select(state_abbrev, season, zone, site, lat_dd, date, survey_id, n, domoic_ppm_max) %>% 
  # Add event id
  mutate(event_id=paste(season, state_abbrev, site, sep="-")) %>% 
  # Arrange
  arrange(site, date)

# Site key 
site_key <- data %>% 
  count(lat_dd, site)

# Event stats
################################################################################

# Events
events <- data %>% 
  group_by(event_id) %>% 
  summarise(nsurveys=n()) %>% 
  ungroup()

# Extend data based on event stats
################################################################################

# Expand data
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

# Inspect events
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(data_exp, aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=event_id_use)) +
  geom_point(pch=21, alpha=0.5) +
  # Labels
  labs(x="Date", y="Latitude (°N)", title="Washington") +
  # scale_y_continuous(breaks=site_key$lat_dd, labels=site_key$site) +
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
ggsave(g, filename=file.path(plotdir, "FigX_washington_event_identification.png"), 
       width=6.5, height=3.5, units="in", dpi=600)

# Event decay
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
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
g <- ggplot(data_exp %>% filter(event_use=="yes"),
            aes(x=event_day, y=domoic_ppm_max)) +
  # Facet
  facet_wrap(~event_id, ncol=4) +
  # Ribbon
  # Reference line
  geom_hline(yintercept=30, linetype="dotted") +
  # Points
  geom_point() +
  # Labels
  labs(x="Day of event", y="Maximum domoic acid (ppm)") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_washington_event_decay_proof_of_concept.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



# CALIFORNIA
################################################################################
################################################################################

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce to Oregon
  filter(state=="California" & n>=3) %>% 
  # Simplify
  select(state_abbrev, season, zone, site, lat_dd, date, survey_id, n, domoic_ppm_max) %>% 
  # Work on sites
  mutate(site=recode(site,
                     "Point Arena"="Manchester Beach",
                     "Trinidad South"="Trinidad Head",
                     "Trinidad North"="Trinidad Head",
                     "Del Norte County, OFFSHORE"="Del Norte County (offshore)",
                     "Humboldt County, OFFSHORE"="Humboldt County (offshore)",
                     "Mendocino, Pt. Arena OFFSHORE"="Mendocino County (offshore)",
                     "Sonoma, OFFSHORE"="Sonoma County (offshore)"),
         site=gsub("Ft", "Fort", site),
         site=gsub("Pt", "Point", site),
         site=gsub("ShltCove", "Shelter Cove", site),
         site=gsub("10MileR", "Ten Mile Run", site),
         site=gsub(", Manchest", ", Manchester Beach", site)) %>% 
  # Format sites
  separate(site, into=c("block_id", "site_name"), sep=", ", remove=F) %>% 
  mutate(site=ifelse(is.na(site_name), block_id, paste(site_name, block_id, sep="-"))) %>% 
  select(-c(site_name, block_id)) %>% 
  # Add event id
  mutate(event_id=paste(season, state_abbrev, site, sep="-")) %>% 
  # Arrange
  arrange(site, date)

# Site key
site_key_ca <- data %>% 
  group_by(lat_dd, site) %>% 
  summarize(years=paste(sort(unique(season)), collapse=", "),
            n=n()) %>% 
  ungroup()


# Event stats
################################################################################

# Events
events <- data %>% 
  group_by(event_id) %>% 
  summarise(nsurveys=n()) %>% 
  ungroup()

# Extend data based on event stats
################################################################################

# Expand data
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

# Inspect events
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

# Lats
lats <- c(34, 35.7, 37, 37.4, 37.7, 37.95, 38.1, 38.35, 38.5, 38.8, 39.3, 40, 40.75,
          40.9, 41.1, 41.2, 41.35, 41.6)


# Plot data
g <- ggplot(data_exp %>% filter(season>2012), aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=event_id_use)) +
  geom_point(pch=21, alpha=0.5) +
  # Lines
  geom_hline(yintercept=lats, linetype="dotted") +
  # Labels
  labs(x="Date", y="Latitude (°N)", title="California") +
  scale_y_continuous(breaks=site_key_ca$lat_dd, labels = site_key_ca$site, lim=c(36.5, 42)) +
  # scale_y_continuous(breaks=seq(32,45, 0.5)) +
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
ggsave(g, filename=file.path(plotdir, "FigX_california_event_identification.png"), 
       width=6.5, height=3.5, units="in", dpi=600)

# Event decay
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
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
g <- ggplot(data_exp %>% filter(event_use=="yes"),
            aes(x=event_day, y=domoic_ppm_max)) +
  # Facet
  facet_wrap(~event_id, ncol=6) +
  # Ribbon
  # Reference line
  geom_hline(yintercept=30, linetype="dotted") +
  # Points
  geom_point() +
  # Labels
  labs(x="Day of event", y="Maximum domoic acid (ppm)") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_california_event_decay_proof_of_concept.png"), 
       width=6.5, height=6.5, units="in", dpi=600)









