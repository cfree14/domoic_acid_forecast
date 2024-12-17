
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
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_dcrab.Rds"))

# Source
source("code/get_sst.R")


# Add temperature data
################################################################################

# Add SST data
data <- get_sst(data_orig)

# Export data
saveRDS(data, file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_dcrab_with_temps.Rds"))



# Build data
################################################################################

# Read BT data
bt <- raster::brick("data/glorys/processed/1993_2020_WC_bottom_temps.tiff")
dates <- seq(ymd("1993-01-01"), ymd("2020-12-31"), by="1 day")

# Unique coordinates
data_xy <- data_orig %>% 
  # Reduce to unique corrds
  select(lat_dd, long_dd) %>% 
  unique() %>% 
  na.omit() %>%
  # Add id
  mutate(xy_id=1:nrow(.))

# Unique coords and dates
data_xy_date <- data_orig %>% 
  # Reduce to unique corrds
  select(date, lat_dd, long_dd) %>% 
  unique() %>% 
  na.omit() %>% 
  # Add id
  left_join(data_xy) %>% filter(date>=ymd("1995-01-01"))
  
# Convert to SF/SP
data_xy_sp <- data_xy %>% 
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=sf::st_crs("+proj=longlat +datum=WGS84")) %>% 
  sf::as_Spatial()

# Extract bottom temperature
data_xy_bt <- raster::extract(x=bt, y=data_xy_sp, na.rm=F)
rownames(data_xy_bt) <- data_xy$xy_id
colnames(data_xy_bt) <- dates


# Format extracted bottom temperature
data <- data_xy_bt %>% 
  # Convert to df
  as.data.frame() %>% 
  # Add column names
  setNames(dates) %>% 
  # Row name to column
  rownames_to_column(var="xy_id") %>% 
  # Gather
  gather(key="date", value="bt_c", 2:ncol(.)) %>% 
  # Format date
  mutate(date=ymd(date))

# Inspect
str(data)
freeR::complete(data)


# Plot data
################################################################################

data1 <- data %>% 
  sample_frac(0.05)

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y=element_blank(),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=date, y=xy_id, fill=bt_c)) +
  geom_tile() +
  # Plot points
  geom_point(data=data_xy_date, aes(x=date, y=xy_id), inherit.aes=F, pch=1) +
  # Labels
  labs(x="Date", y="Sample site") +
  scale_x_date(breaks=seq(ymd("1995-01-01"), 
                          ymd("2020-01-01"), by="5 years"),
               date_label="%Y") +
  # Legend
  scale_fill_gradientn(name="Bottom temp (°C)", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), 
                       na.value="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
#g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_bottom_temps_sites.png"), 
       width=6.5, height=3.5, units="in", dpi=600)

