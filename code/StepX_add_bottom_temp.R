
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

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_dcrab.Rds"))


# https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/description

# Build data
################################################################################

# Unique coordinates
data_xy <- data_orig %>% 
  select(date,lat_dd, long_dd) %>% 
  unique() %>% 
  na.omit()

# Get bottom temps
range(data_xy$lat_dd)
range(data_xy$long_dd)




