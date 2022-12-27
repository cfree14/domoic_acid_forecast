
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
data_orig <- readRDS(file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_dcrab_surveys_with_event_id.Rds"))

# Read zones
zones_orig <- readxl::read_excel("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx")


# Season info
################################################################################

# Seasons
seasons_do <- 1999:2023

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60

# Washington season
# December 1st to September 15th
openers_wa <- paste0(seasons_do, "-12-01") %>% ymd()
closers_wa <- paste0(seasons_do+1, "-09-15") %>% ymd()
seasons_wa <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                     open=openers_wa,
                     close=closers_wa)

# Oregon season
# December 1st to August 14th
openers_or <- paste0(seasons_do, "-12-01") %>% ymd()
closers_or <- paste0(seasons_do+1, "-08-14") %>% ymd()
seasons_or <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                     open=openers_or,
                     close=closers_or)

# California-North season
# December 1st to July 15th
openers_ca_n <- paste0(seasons_do, "-12-01") %>% ymd()
closers_ca_n <- paste0(seasons_do+1, "-07-15") %>% ymd()
seasons_ca_n <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                       open=openers_ca_n,
                       close=closers_ca_n)

# California-Central season
# November 15th to June 30th
openers_ca_c <- paste0(seasons_do, "-11-15") %>% ymd()
closers_ca_c <- paste0(seasons_do+1, "-06-30") %>% ymd()
seasons_ca_c <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                       open=openers_ca_c,
                       close=closers_ca_c)


# Build zones data
################################################################################

# Build zones dataframe
zones_df <- zones_orig %>%
  filter(!is.na(lat_dd_north)) %>%
  select(state, lat_dd_north) %>%
  rename(y=lat_dd_north) %>%
  mutate(x1=recode(state,
                   "Washington"="2014-10-01",
                   "Oregon"="2017-11-01",
                   "California"="2020-11-01") %>% ymd(),
         x2=ymd("2023-10-30"))

# Build zones
zones1 <- zones_orig %>%
  filter(state=="Washington") %>%
  mutate(season="2015-16 season") %>%
  select(season, everything())
zones2 <- zones_orig %>%
  mutate(season="2020-21 season") %>%
  select(season, everything())
zones <- bind_rows(zones1, zones2) %>%
  mutate(lat_dd_avg=(lat_dd_north+lat_dd_south)/2) %>%
  # Alter Zone I lat
  mutate(lat_dd_avg=ifelse(zone_id=="H", 35.3, lat_dd_avg))


# All events
################################################################################

# Starting date
date_min_do <- ymd(paste0(min(seasons_do), "-01-01"))
date_max_do <- ymd(paste0(max(seasons_do)+1, "-01-01"))

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
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_orig %>% filter(season>=1999), 
            aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=event_id_use)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=son_mend_county, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=son_mend_county, fill="grey90") +
  # Management zone lines
  geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
               inherit.aes = F, color="grey40", size=0.35) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id), x=ymd("2023-10-01"), hjust=0, size=1.5, inherit.aes = F, color="grey50") +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="grey30", size=2.5) +
  # Plot surveys
  geom_point(pch=21, alpha=0.5) +
  # Labels
  labs(x="Survey date", y="Latitude (°N)") +
  # Axes
  scale_y_continuous(breaks=seq(32, 50, 2)) +
  scale_x_date(breaks=seq(date_min_do, date_max_do, by="1 year"), 
               lim=c(date_min_do, date_max_do),
               labels=year(date_min_do):year(date_max_do)) +
  # Color legend (events)
  scale_fill_manual(name="", guide="none",
                    na.value = "grey80",
                    values=rainbow(n_distinct(data_orig$event_id)) %>% sample()) +
  # Size legend (DA conc)
  scale_size_continuous(name="Maximum\ndomoic acid (ppm)") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.key.size = unit(0.5, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS3_event_identification_full.png"), 
       width=6.5, height=7.5, units="in", dpi=600)



# Recent events
################################################################################

# Starting date
date_min_do2 <- ymd("2014-01-01")

# Plot data
g <- ggplot(data_orig %>% filter(season>=2015), 
            aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=event_id_use)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=son_mend_county, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=son_mend_county, fill="grey90") +
  # Management zone lines
  geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
               inherit.aes = F, color="grey40", size=0.35) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id), x=ymd("2023-10-01"), hjust=0, size=1.5, inherit.aes = F, color="grey50") +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do2, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do2, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do2, y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do2, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="grey30", size=2.5) +
  # Plot surveys
  geom_point(pch=21, alpha=0.5) +
  # State line
  geom_hline(yintercept=c(42, 46.25, 48.48)) +
  geom_hline(yintercept=38+46.125/60, linetype="dashed") +
  # Labels
  labs(x="Survey date", y="Latitude (°N)") +
  # Axes
  scale_x_date(lim=c(date_min_do2, NA), 
               breaks=seq(date_min_do2, date_max_do, by="1 year"), 
               date_labels="%Y") +
  scale_y_continuous(breaks=seq(32, 50, 2)) +
  # Color legend (events)
  scale_fill_manual(name="", guide="none",
                    na.value = "grey80",
                    values=rainbow(n_distinct(data_orig$event_id)) %>% sample()) +
  # Size legend (DA conc)
  scale_size_continuous(name="Maximum\ndomoic acid (ppm)") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.key.size = unit(0.5, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS4_event_identification_zoom.png"), 
       width=6.5, height=7.5, units="in", dpi=600)


# Tricky events
################################################################################


# Plot data
g1 <- ggplot(data_orig %>% filter(season==2015 & state=="California"), 
            aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=event_id_use)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=son_mend_county, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=son_mend_county, fill="grey90") +
  # Management zone lines
  geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
               inherit.aes = F, color="grey40", size=0.35) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id), x=ymd("2023-10-01"), hjust=0, size=1.5, inherit.aes = F, color="grey50") +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do2, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do2, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do2, y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do2, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="grey30", size=2.5) +
  # Plot surveys
  geom_point(pch=21, alpha=0.5) +
  # State line
  geom_hline(yintercept=c(42, 46.25, 48.48)) +
  geom_hline(yintercept=38+46.125/60, linetype="dashed") +
  # Labels
  labs(x="Survey date", y="Latitude (°N)") +
  # Axes
  scale_x_date(lim=c(ymd("2015-07-15"), ymd("2016-08-15")), 
               breaks=seq(date_min_do2, date_max_do, by="1 month"), 
               date_labels="%b\n%Y") +
  scale_y_continuous(breaks=seq(32, 42, 1), lim=c(35, 42)) +
  # Color legend (events)
  scale_fill_manual(name="", guide="none",
                    na.value = "grey80",
                    values=rainbow(n_distinct(data_orig$event_id)) %>% sample()) +
  # Size legend (DA conc)
  scale_size_continuous(name="Maximum\ndomoic acid (ppm)") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.8,0.2),
        legend.key.size = unit(0.5, "cm"))
g1

# Subset data
wa2002 <- data_orig %>% filter(season==2002 & state=="Washington")

# Plot data
g2 <- ggplot(wa2002, 
             aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=event_id_use)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=son_mend_county, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=son_mend_county, fill="grey90") +
  # Management zone lines
  geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
               inherit.aes = F, color="grey40", size=0.35) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id), x=ymd("2023-10-01"), hjust=0, size=1.5, inherit.aes = F, color="grey50") +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do2, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do2, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do2, y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do2, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="grey30", size=2.5) +
  # Plot surveys
  geom_point(pch=21, alpha=0.5) +
  # State line
  geom_hline(yintercept=c(42, 46.25, 48.48)) +
  geom_hline(yintercept=38+46.125/60, linetype="dashed") +
  # Labels
  labs(x="Survey date", y="Latitude (°N)") +
  # Axes
  scale_x_date(lim=c(ymd("2002-10-01"), ymd("2003-06-15")),
               breaks=seq(date_min_do, date_max_do, by="1 month"),
               date_labels="%b\n%Y") +
  scale_y_continuous(breaks=seq(32, 50, 0.1), lim=c(46.6, 47.25)) +
  # Color legend (events)
  scale_fill_manual(name="", guide="none",
                    na.value = "grey80",
                    values=rainbow(n_distinct(wa2002$event_id)) %>% sample()) +
  # Size legend (DA conc)
  scale_size_continuous(name="Maximum\ndomoic acid (ppm)") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none",
        legend.key.size = unit(0.5, "cm"))
g2

