
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(lubridate)

# Directories
indir <- "data/moore/processed"
outdir <- "data/da_samples/data"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_dcrab_surveys.Rds"))

# Read zones
zones_orig <- readxl::read_excel("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx")

# Read sites
sample_sites <- readxl::read_excel("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/merged/processed/WC_dcrab_sampling_sites.xlsx", sheet=2) %>% 
  mutate(location=ifelse(state=="Washington", NA, location)) %>% 
  mutate(location=recode(location,
                         "Pillar Point (Half Moon Bay)"="Pillar Point"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Season info
################################################################################

# Seasons
seasons_do <- 2014:2023

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

# No NCal line
zones_no_ncal_line <- zones %>%
  filter(landmark_north!="Sonoma/Mendocino County Line")

# Zone points
zone_pts <- zones %>%
  mutate(!is.na(lat_dd)) %>% 
  mutate(zone_id=recode(zone_id,
                        "60D"="60D/50-O"))

# Borders
border_n <- zones %>% arrange(desc(lat_dd_north)) %>% slice(1) %>% pull(lat_dd_north)
borders_s <- zones %>%
  filter(grepl("border", landmark_south)) %>% pull(lat_dd_south)
borders <- c(border_n, borders_s) %>% unique()


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  filter(year > 1998 & n >= 3)


# Plot data
################################################################################

# Starting date
date_min_do <- ymd(paste0(min(seasons_do), "-01-01"))
date_max_do <- ymd(paste0(max(seasons_do)+1, "-01-01"))

# Theme
base_theme <-  theme(axis.text=element_text(size=6),
                     axis.title=element_text(size=7),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
                     plot.title=element_blank(),
                     plot.tag = element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))


# Plot data
g1 <- ggplot(zones) +
  # Plot management zones
  geom_hline(data=zones_no_ncal_line, mapping=aes(yintercept=lat_dd_north), linetype="dotted", size=0.2) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id), x=-126.5, hjust=0, size=1.8, color="grey50", show.legend = F) +
  geom_hline(yintercept=borders, linetype="solid", color="black", size=0.2) +
  # Plot Sonoma-Mendocino country line
  geom_hline(yintercept=son_mend_county, linetype="dashed", size=0.2) +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Plot management zone points
  geom_text(data=zone_pts, mapping=aes(x=long_dd, y=lat_dd, label=zone_id), size=1.5, color="grey50", hjust=0) +
  # Plot sampling sites
  geom_point(data=sample_sites, mapping=aes(x=long_dd, y=lat_dd), size=1, color="darkred") +
  geom_text(data=sample_sites, mapping=aes(x=long_dd+0.3, y=lat_dd, label=location), 
            hjust=0, size=1.5, color="darkred", show.legend = F) +
  # Labels
  labs(x="", y="", tag="A") +
  scale_x_continuous(breaks=seq(-128,120,2)) +
  # Crop
  coord_sf(xlim = c(-127, -120), ylim = c(35, 48.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = c(0.1, 0.15))
g1

# Plot survey data
g2 <- ggplot(data, aes(x=date, y=lat_dd, size=domoic_ppm_max, fill=pover)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=son_mend_county, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=son_mend_county, fill="grey90") +
  # Management zone lines
  geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
               inherit.aes = F, linetype="solid", size=0.4, color="grey30") +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id), x=ymd("2023-10-01"), hjust=0, size=1.8, inherit.aes = F, color="grey50") +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.4) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.2) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0.2, vjust=1.5, label="Washington", color="grey30", size=2.2) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0.2, vjust=1.5, label="Oregon", color="grey30", size=2.2) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0.2, vjust=1.5, label="N. California", color="grey30", size=2.2) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0.2, vjust=1.5, label="C. California", color="grey30", size=2.2) +
  # Plot California N/As
  annotate(geom="text", x=ymd("2015-03-15"), y=c(40.38437, 36.88437), label="N/A", color="grey30", size=2) +
  # Points
  geom_point(alpha=0.8, pch=21, stroke=0.3) +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2), labels=paste0(seq(34, 48, 2), "°N")) +
  scale_x_date(breaks=seq(date_min_do, date_max_do, by="1 year"), 
               lim=c(date_min_do, date_max_do),
               labels=year(date_min_do):year(date_max_do)) +
  # Labels
  labs(x="Survey date", y="Latitude (°N)", tag="B") +
  # Legends
  scale_size_continuous(name="Maximum\ndomoic acid (ppm)", range = c(0.01, 4)) +
  scale_fill_gradientn(name="% over 30 ppm\naction threshold",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       labels=scales::percent) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g2

# Merge plot
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.3, 0.7))

# Export figure
ggsave(g, filename=file.path(plotdir, "Fig1_survey_timeline_map.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


