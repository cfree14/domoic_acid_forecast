
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
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data.Rds")) %>% 
  mutate(row_id=1:n())

# Read zones
zone_df <- readxl::read_excel("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/merged/processed/WC_dcrab_da_mgmt_zones.xlsx")
zone_lats <- c(zone_df$lat_dd_south, zone_df$lat_dd_north) %>% unique() %>% sort()
zones <- zone_df %>% 
  filter(!is.na(landmark_north)) %>% 
  pull(zone_id) %>% rev() %>% as.character()


# Id blocks
################################################################################

# Blocks
blocks <- wcfish::blocks
blocks_sp <- blocks %>% 
  select(block_id) %>% 
  sf::as_Spatial()

# Convert to sp
data_sp <- data_orig %>% 
  filter(!is.na(lat_dd) & !is.na(long_dd)) %>% 
  select(row_id, lat_dd, long_dd) %>% 
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=sf::st_crs(blocks)) %>% 
  sf::as_Spatial()

# Step 1. Identify points that fall directly inside a block
block_key1 <- sp::over(data_sp, blocks_sp) %>% 
  as.data.frame() %>% 
  mutate(row_id=data_sp$row_id) %>% 
  filter(!is.na(block_id))

# Step 2. Identify points outside the block system
data_sp2 <- data_sp[!data_sp$row_id %in% block_key1$row_id,]

# Step 3. Identify nearest polygon (block) to unmatched site
block_key2_mat <- rgeos::gDistance(data_sp2, blocks_sp, byid=TRUE)
block_key2 <- block_key2_mat %>%
  # Convert to dataframe
  as.data.frame() %>% 
  # Add ids
  setNames(data_sp2$row_id) %>% 
  mutate(block_id=blocks$block_id) %>% 
  select(block_id, everything()) %>% 
  # Gather
  gather(key="row_id", value="distance", 2:ncol(.)) %>%
  mutate(row_id=as.numeric(row_id)) %>% 
  # Get closest block
  arrange(row_id, distance) %>% 
  group_by(row_id) %>% 
  slice(1) %>% 
  ungroup()

# Step 4. Merge block keys
block_key <- bind_rows(block_key1, block_key2) %>% 
  select(row_id, block_id) %>% 
  arrange(row_id)


# Format data
################################################################################

# Grays Harbor sites
grays_harbor <- c("Grays Harbor", "Quinault Marina", "Westport",
                  "North Bay - West", "North Bay - East", "Mid Bay", "South Bay", 
                  "Whitcomb Spit", "Elk River")

# Willapa Bay (60C)
willapa_bay <- c("Shoalwater Bay Reservation", "Tokeland Marina", "Tokeland Area", "North River Area",
                 "Bruceport Area", 'Ellen Sands', "Stony Point", "Palix River", "Bay Center Dock",
                 "Bay Center Area", "Stackpole South", "Stackpole Harbor", "Seal Spit", "Oysterville Area", 
                 "Diamond Point", "Nemah Area", "Nahcotta Area")

# Format data
data <- data_orig %>% 
  # Add zone
  mutate(zone=cut(lat_dd, breaks=zone_lats, labels=zones) %>% as.character()) %>% 
  # Fill in some missing zones
  mutate(zone=case_when(site=="Del Norte County, OFFSHORE" ~ "A",
                        site=="Point Arena" ~ "D",
                        site=="Santa Barbara Ch., EI Lease" ~ "H",
                        T ~ zone)) %>% 
  # Add Grays Harbor (60B)
  mutate(zone=ifelse(site %in% grays_harbor, "60B", zone)) %>% 
  # Add Grays Harbor (60C)
  mutate(zone=ifelse(grepl("Willapa", site) | site %in% willapa_bay, "60C", zone)) %>% 
  # Add WA-Columbia River (60D)
  mutate(zone=ifelse(site=="Illwaco", "60D", zone)) %>% 
  # Add OR-Columbia River (50O)
  mutate(zone=ifelse(state=="Oregon" & (grepl("Columbia", site) | site=="Astoria"), "50-O", zone)) %>% 
  # Add block id
  left_join(block_key, by="row_id") %>% 
  # Remove row id
  select(-row_id) %>% 
  # Arrange
  select(state, organization,
         comm_name, sci_name, 
         year, month, date,
         zone, block_id, site, lat_dd, long_dd, 
         domoic_id, domoic_tissue, domoic_n, domoic_ppm, domoic_mod,
         psp_id, psp_tissue, psp_ug100g,
         dsp_id, dsp_tissue, dsp_ug100g, notes)

# Inspect
freeR::complete(data)
table(data$zone)

# Site key
site_key <- data %>% 
  count(state, zone, site, lat_dd, long_dd) #%>% 
  # filter(lat_dd>46.2 & lat_dd<46.8)


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

# Plot data by zone (zoom in on WA which has bay zones)
g <- ggplot(site_key %>% filter(state=="Washington"), aes(x=long_dd, y=lat_dd, color=zone)) +
  # Plot states
  geom_sf(data=usa, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  geom_sf(data=canada, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  # Plot points
  geom_point() +
  # Plot site name
  # ggrepel::geom_text_repel(mapping=aes(label=site), size=2, min.segment.length = 0) +
  # Legend
  scale_color_discrete(name="") +
  # Crop
  coord_sf(xlim = c(-125, -123.5), ylim = c(46.2, 48.4)) + # All WA
  # coord_sf(xlim = c(-124.4, -123.8), ylim = c(46.7, 47.2)) + # Grays Harbor
  # coord_sf(xlim = c(-124.4, -123.8), ylim = c(46.2, 46.8)) + # Willapa Bay
  # Theme
  theme_bw() + base_theme +
  theme(axis.title=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.key.size = unit(0.3, "cm"),
        legend.position = "right")
g

# Plot data by zone (zoom in on OR which has bay zones)
g <- ggplot(site_key %>% filter(state=="Oregon"), aes(x=long_dd, y=lat_dd, color=zone)) +
  # Plot states
  geom_sf(data=usa, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  geom_sf(data=canada, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  # Plot points
  geom_point() +
  # Plot site name
  ggrepel::geom_text_repel(mapping=aes(label=site), size=2, min.segment.length = 0) +
  # Legend
  scale_color_manual(name="", 
                     values=rainbow(site_key %>% filter(state=="Oregon") %>% pull(zone) %>% n_distinct()) %>% sample()) +
  # Crop
  coord_sf(xlim = c(-125, -123.5), ylim = c(42, 46.2)) + # All OR
  # Theme
  theme_bw() + base_theme +
  theme(axis.title=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.key.size = unit(0.3, "cm"),
        legend.position = "right")
g

# Plot data by zone
g <- ggplot(data, aes(x=long_dd, y=lat_dd, color=block_id)) +
  # Plot states
  geom_sf(data=usa, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  geom_sf(data=canada, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  # Plot points
  geom_point() +
  # Legend
  scale_color_gradientn(name="", colors=RColorBrewer::brewer.pal(9, "Spectral")) +
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


