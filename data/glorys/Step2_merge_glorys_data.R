

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Library
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)

# Directories
rawdir <- "data/glorys/raw"
outdir <- "data/glorys/processed"

# Files to merge
files2merge <- list.files(rawdir)


# Build lat/long correction key
################################################################################

# Read one
test <- readRDS(file=file.path(rawdir, files2merge[1]))

# Latitude correction key
lats <- test$lat_dd %>% unique() %>% sort()
lats[2:length(lats )]-lats[1:(length(lats)-1)] 
lats_use <- seq(min(lats), max(lats), length.out=length(lats))
lat_key <- tibble(lat_dd=lats,
                  lat_dd_use=lats_use)

# Longitude correction key
longs <- test$long_dd %>% unique() %>% sort()
longs[2:length(longs )]-longs[1:(length(longs)-1)] 
longs_use <- seq(min(longs), max(longs), length.out=length(longs))
long_key <- tibble(long_dd=longs,
                  long_dd_use=longs_use)


# Build data
################################################################################

# Loop through data and merge
i <- 1
for(i in 1:length(files2merge)){
  
  # Read data
  print(i)
  infile <- files2merge[i]
  data_orig <- readRDS(file.path(rawdir,  infile ))
  
  # Get dates
  dates <- unique(data_orig$date)
  
  # Convert to brick
  data_ras <- data_orig %>% 
    # Spread
    spread(key="date", value="btemp_c") %>% 
    # Add corrected lat/long
    left_join(lat_key, by="lat_dd") %>% 
    left_join(long_key, by="long_dd") %>% 
    dplyr::select(-c(lat_dd, long_dd)) %>% 
    rename(lat_dd=lat_dd_use, long_dd=long_dd_use) %>% 
    dplyr::select(long_dd, lat_dd, everything()) %>% 
    # Convert to raster
    raster::rasterFromXYZ(crs="+proj=longlat +datum=WGS84")
  
  # Merge data
  if(i==1){data_ras_merge <- data_ras}else{data_ras_merge <- raster::stack(data_ras_merge, data_ras)}
  
}

# Export data
raster::writeRaster(data_ras_merge, filename = file.path(outdir, "1993_2020_WC_bottom_temps.tiff"), overwrite=T)



