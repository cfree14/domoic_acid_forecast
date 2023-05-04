
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Library
library(ncdf4)
library(tidync)
library(tidyverse)
library(lubridate)

# Directories
rawdir <- "data/glorys/raw"

# GLORYS
# Global Ocean Physics Reanalysis
# https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/description
# Standard regular grid at 1/12° (approximatively 8 km) and on 50 standard levels.


# The 'tidync' approach
################################################################################

# Learn about data - ncdf4 way
data_orig <- nc_open("https://cfree:Overmars.14!@my.cmems-du.eu/thredds/dodsC/cmems_mod_glo_phy_my_0.083_P1D-m") # daily
names(data_orig$dim)
names(data_orig$var)

# Learn about data - tidync way
data0 <- tidync("https://cfree:Overmars.14!@my.cmems-du.eu/thredds/dodsC/cmems_mod_glo_phy_my_0.083_P1D-m")
hyper_vars(data0)
hyper_dims(data0)

# Read only bottom temperature
data1 <- tidync("https://cfree:Overmars.14!@my.cmems-du.eu/thredds/dodsC/cmems_mod_glo_phy_my_0.083_P1D-m", what="bottomT")
hyper_vars(data1)
hyper_dims(data1)

# Get times 
time <- ncdf4::ncvar_get(data_orig, "time") # hours since 1950-01-01
dates <- ( ymd("1950-01-01") + hours(time) ) %>%  date()
time_key <- tibble(time=time, 
                   date=dates) %>% 
  mutate(year=year(date),
         month=month(date) %>% as.character() %>%  stringr::str_pad(., width=2, side="left", pad="0"),
         year_month=paste(year, month, sep="-"))

# Filter to region of interest
data2 <- data1 %>% 
  hyper_filter(latitude = latitude >= 34 & latitude <= 48.5) %>% 
  hyper_filter(longitude = longitude >= -125.5 & longitude <= -119.5) 

# Filter to date of interest
data3 <- data2 %>% 
  hyper_filter(time = time==390324)

# Convert to dataframe
data4 <- data3 %>% 
  hyper_tibble()

# Plot data
world <- rnaturalearth::ne_countries(country="United States of America", scale="small", returnclass = "sf")
g <- ggplot(data4, aes(x=longitude, y=latitude, fill=bottomT)) +
  geom_sf(data=world, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  geom_tile() +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim=c(-125.5, -119.5), ylim=c(34, 48.5)) +
  # Legend
  scale_fill_gradientn(name="BT (°C)", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()
g

# Loop through time steps
months <- sort(unique(time_key$year_month))
for(i in 1:length(months)){
  
  # Filter to dates of interest
  print(i)
  month_do <- months[i]
  times_do <- time_key %>% 
    filter(year_month==month_do) %>% 
    pull(time)
  data3 <- data2 %>% 
    hyper_filter(time = time %in% times_do)
  
  # Convert to dataframe
  data4 <- data3 %>% 
    hyper_tibble()
  
  # Format
  data5 <- data4 %>% 
    rename(lat_dd=latitude, long_dd=longitude, btemp_c=bottomT) %>% 
    left_join(time_key %>% select(time, date), by="time") %>% 
    select(date, long_dd, lat_dd, btemp_c)
    
  # Export
  outfile <- paste0(month_do, ".Rds")
  saveRDS(data5, file=file.path(rawdir, outfile))
  
}


# The 'ncdf4' approach
################################################################################

# # Read data
# # data_orig <- nc_open("https://cfree:Overmars.14!@my.cmems-du.eu/thredds/dodsC/cmems_mod_glo_phy_my_0.083_P1M-m") # monthly
# data_orig <- nc_open("https://cfree:Overmars.14!@my.cmems-du.eu/thredds/dodsC/cmems_mod_glo_phy_my_0.083_P1D-m") # daily
# 
# # Inspect NetCDF
# data_orig
# names(data_orig$dim)
# names(data_orig$var)
# 
# # Extract dimensions
# longs <- ncdf4::ncvar_get(data_orig, "longitude")
# lats <- ncdf4::ncvar_get(data_orig, "latitude")
# time <- ncdf4::ncvar_get(data_orig, "time") # hours since 1950-01-01
# dates <- ( ymd("1950-01-01") + hours(time) ) %>%  date()
# range(dates)
# depths <- ncdf4::ncvar_get(data_orig, "depth")

# Doesn't work
# data_ras <- raster::brick("https://my.cmems-du.eu/thredds/dodsC/cmems_mod_glo_phy_my_0.083_P1D-m.html",
#                           varname="bottomT",
#                           xmn=-118,
#                           xmx=-116,
#                           ymn=40,
#                           ymx=42)
