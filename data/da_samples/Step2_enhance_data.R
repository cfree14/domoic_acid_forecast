
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/moore/processed"
outdir <- "data/da_samples/data"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data.Rds"))

# Blocks
blocks <- wcfish::blocks

# Read zones
zone_df <- readxl::read_excel("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx")
zone_lats <- c(zone_df$lat_dd_south, zone_df$lat_dd_north) %>% unique() %>% sort()
zones <- zone_df %>% 
  filter(!is.na(landmark_north)) %>% 
  pull(zone_id) %>% rev()


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Add zone
  mutate(zone=cut(lat_dd, breaks=zone_lats, labels=zones)) %>% 
  # Arrange
  select(state, organization,
         comm_name, sci_name, 
         year, month, date,
         zone, site, lat_dd, long_dd, 
         domoic_id, domoic_tissue, domoic_n, domoic_ppm, domoic_mod,
         psp_id, psp_tissue, psp_ug100g,
         dsp_id, dsp_tissue, dsp_ug100g, notes)

# Inspect
freeR::complete(data)


# Plot data
################################################################################

# Map theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title = element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"), 
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))


# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
canada <- rnaturalearth::ne_countries(country="Canada", returnclass = "sf")

# Plot data by state
g <- ggplot(data, aes(x=long_dd, y=lat_dd, color=state)) +
  # Plot states
  geom_sf(data=usa, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  geom_sf(data=canada, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  # Plot points
  geom_point() +
  # Legend
  scale_color_discrete(name="") +
  # Crop
  coord_sf(xlim = c(-126, -116), ylim = c(32, 50)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.key.size = unit(0.3, "cm"),
        legend.position = c(0.25, 0.1))
g

# Plot data by zone
g <- ggplot(data, aes(x=long_dd, y=lat_dd, color=zone)) +
  # Plot states
  geom_sf(data=usa, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  geom_sf(data=canada, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  # Plot points
  geom_point() +
  # Legend
  scale_color_discrete(name="") +
  # Crop
  coord_sf(xlim = c(-126, -116), ylim = c(32, 50)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.key.size = unit(0.3, "cm"),
        legend.position = "right")
g

# Plot data
g <- ggplot(data, aes(x=date, y=lat_dd, size=domoic_ppm, color=zone)) +
  # Plot
  geom_point() +
  # X-axis
  scale_x_date(lim=c(lubridate::ymd("2000-01-01"), NA)) +
  # Theme
  theme_bw() + base_theme
g


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_enhanced.Rds"))


