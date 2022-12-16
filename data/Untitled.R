

# Library
library(ncdf4)
library(tidync)

# GLORYS
# Global Ocean Physics Reanalysis
# https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/description
# Standard regular grid at 1/12° (approximatively 8 km) and on 50 standard levels.

# Read data
data_orig <- nc_open("https://cfree:Overmars.14!@my.cmems-du.eu/thredds/dodsC/cmems_mod_glo_phy_my_0.083_P1M-m")

data_ras <- raster::brick("https://cfree:Overmars.14!@my.cmems-du.eu/thredds/dodsC/cmems_mod_glo_phy_my_0.083_P1M-m", varname="thetao")

# Inspect NetCDF
data_orig
attributes(data_orig)
data_orig$ndims
data_orig$natts
names(data_orig$dim)
data_orig$var

# Extract dimensions
longs <- ncdf4::ncvar_get(data_orig, "longitude")
lats <- ncdf4::ncvar_get(data_orig, "latitude")
time <- ncdf4::ncvar_get(data_orig, "time") # hours since 1950-01-01
depths <- ncdf4::ncvar_get(data_orig, "depth")

# Extract data
temps <- ncdf4::ncvar_get(data_orig, "thetao")

ncdf4::ncatt_get(data_orig, "longitude")


# TIDYNC approach

# Read data
data <- tidync("https://cfree:Overmars.14!@my.cmems-du.eu/thredds/dodsC/cmems_mod_glo_phy_my_0.083_P1M-m")

# Filter data
data1 <- data %>% 
  hyper_filter(latitude = latitude >= 40 & latitude <= 42) %>% 
  hyper_filter(longitude = longitude >= -125 & longitude <= -124) %>% 
  hyper_filter(time= time==622020)

# Extract data
data2 <- data1 %>% 
  hyper_array()

# Format data
data3 <- data2$thetao



