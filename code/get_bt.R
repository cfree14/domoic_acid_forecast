
# Function to get SST data
# data <- data_orig
get_bt <- function(data){
  
  # OISST info
  # datasets_grids <- rerddap::ed_datasets(which="griddap", url="https://upwell.pfeg.noaa.gov/erddap/")
  # datasets_tables <-rerddap::ed_datasets(which="tabledap", url="https://upwell.pfeg.noaa.gov/erddap/")
  data_info <- rerddap::info("ncdcOisst21Agg_LonPM180")
  
  # OISST date range
  # From here: https://github.com/noaa-onms/onmsR/blob/master/R/calculate_data.R
  data_date_range <- data_info$alldata$time %>%
    dplyr::filter(attribute_name=="actual_range") %>%
    pull(value) %>%
    str_split(", ", simplify = T) %>%
    as.numeric() %>%
    as.POSIXct(origin = "1970-01-01", tz = "GMT") %>% 
    lubridate::ymd_hms() %>% lubridate::date()
  data_date_min <- data_date_range[1] %>% lubridate::ymd()
  data_date_max <- data_date_range[2] %>% lubridate::ymd()
  
  # Full key
  # Get unique date+GPS combos
  key <- data %>% 
    # Reduce 
    select(date, long_dd, lat_dd) %>% 
    # Unique
    unique() %>% 
    na.omit() %>% 
    # Reduce to dates with data
    mutate(date=lubridate::ymd(date)) %>% 
    filter(date >= data_date_min & date <= data_date_max)
  
  # Parameters
  dates <- unique(key$date) %>% sort()
  long_range <- range(key$long_dd)
  lat_range <- range(key$lat_dd)
  
  # Lat/long key
  gps_key <- key %>% 
    select(long_dd, lat_dd) %>% 
    unique()
  
  # OISST raster cell coordinates
  suppressMessages({
    oisst_gps <- rerddap::griddap(x=data_info, 
                                  time = c("2020-01-01", "2020-01-01"),
                                  longitude = long_range, 
                                  latitude = lat_range, 
                                  zlev = c(0, 0),
                                  fields="sst")$data %>% 
      filter(!is.na(sst)) %>% 
      select(lon, lat) %>% 
      unique() %>% 
      rename(long_dd=lon, lat_dd=lat)
  })
  
  # Loop through each row in gps_key
  # and find closest coordinate in raster and distance to closest coordinate
  gps_key_snap <- gps_key %>% 
    mutate(long_dd_sst=NA, lat_dd_sst=NA, dist_km=NA)
  for (i in 1:nrow(gps_key)) {
    
    # Extract latitude and longitude from gps_key
    coords <- gps_key[i, c("long_dd", "lat_dd")] %>% as.numeric()
    
    # Calculate distances between each row in gps_key and all rows in oisst_gps
    distances <- geosphere::distm(coords, oisst_gps[, c( "long_dd", "lat_dd")], fun = distHaversine)
    
    # Find the index of the minimum distance
    min_dist_index <- which.min(distances)
    distances[min_dist_index]
    
    # Extract the closest coordinates from oisst_gps
    coords_closest <- oisst_gps[min_dist_index, c("long_dd", "lat_dd")] %>% as.numeric()
    
    # Calculate distance between coordinates
    dist_m <- distHaversine(coords, coords_closest)
    dist_km <- dist_m / 1000
    
    # Add closest coordinates and distance to gps_key
    gps_key_snap$long_dd_sst[i] <- coords_closest[1]
    gps_key_snap$lat_dd_sst[i] <- coords_closest[2]
    gps_key_snap$dist_km[i] <- dist_km
    
  }
  
  # Download OISST data
  # Loop through dates b/c apparently non-sequential dates don't work
  x <- dates[617]
  oisst_df <- furrr::future_map_dfr(dates, function(x){
    
    # Try to extract OISST
    suppressMessages({
      oisst <- try(rerddap::griddap(x=data_info, 
                              time = c(x, x),
                              longitude = long_range, 
                              latitude = lat_range, 
                              zlev = c(0, 0),
                              fields="sst"), silent = T)
    })

    # Create df
    if(inherits(oisst, "try-error")){
      oisst_df <- NULL
    }else{
      # Convert OISST to df
      oisst_df <- oisst$data %>% 
        mutate(date=substr(time, 1, 10)) %>% 
        select(date, lon, lat, sst)
      
    }
    
  })

  # Spread df
  oisst_df_wide <- oisst_df %>%
    spread(key=date, value=sst)
  
  # Convert OISST to brick
  oisst_ras <- raster::rasterFromXYZ(xyz=oisst_df_wide)
  
  # Plot brick
  raster::plot(oisst_ras[[1]])
  
  # Extract SST data
  ssts <- raster::extract(x=oisst_ras, y=gps_key_snap[,c("long_dd", "lat_dd")])
  ssts_snap <- raster::extract(x=oisst_ras, y=gps_key_snap[,c("long_dd_sst", "lat_dd_sst")])
  
  # Format extracted data
  ssts_df <- ssts %>%
    # Data frame
    as.data.frame() %>% 
    # Add GPS coords
    mutate(long_dd=gps_key$long_dd,
           lat_dd=gps_key$lat_dd) %>% 
    # Arrange
    select(long_dd, lat_dd, everything()) %>% 
    gather(key="date", value="sst_c", 3:ncol(.)) %>% 
    # Date
    mutate(date=gsub("X", "", date) %>% lubridate::ymd())
  
  # Format extracted data
  ssts_snap_df <- ssts_snap %>%
    # Data frame
    as.data.frame() %>% 
    # Add GPS coords
    mutate(long_dd=gps_key_snap$long_dd,
           lat_dd=gps_key_snap$lat_dd,
           dist_km=gps_key_snap$dist_km) %>% 
    # Arrange
    select(long_dd, lat_dd, dist_km, everything()) %>% 
    gather(key="date", value="sst_c_snap", 4:ncol(.)) %>% 
    # Date
    mutate(date=gsub("X", "", date) %>% lubridate::ymd())
  
  # Add to key
  key1 <- key %>% 
    # Add SST from points
    left_join(ssts_df, by=c("date", "long_dd", "lat_dd")) %>% 
    # Add SST from snapped points
    left_join(ssts_snap_df, by=c("date", "long_dd", "lat_dd")) %>% 
    # Choose SST source
    mutate(sst_source=ifelse(!is.na(sst_c), "Observed", 
                             ifelse(!is.na(sst_c_snap), "Nearest cell", NA)),
           sst_source_km=ifelse(sst_source=="Nearest cell", dist_km, NA),
           sst_c_final=ifelse(!is.na(sst_c), sst_c, sst_c_snap)) %>% 
    # Arrange
    select(date, long_dd, lat_dd, sst_c_final, sst_source, sst_source_km) %>% 
    rename(sst_c=sst_c_final)
  
  # Add SSTs to data
  data_out <- data %>% 
    left_join(key1, by=c("date", "long_dd", "lat_dd"))
  
  # Return data
  return(data_out)
  
 
}



library(tidyverse) # A staple for most modern data management in R
library(RCurl) # For helping R to make sense of URLs for web hosted data
library(XML) # For reading the HTML tables created by RCurl
library(tidync) # For easily dealing with NetCDF data
library(doParallel) # For parallel processing
library(threadr) # For downloading from FTP sites that require user credentials
library(RCMEMS) # For subsetting CMEMS data before download

# The GLORYS script
# This is a dummy script first generated by using the UI on the CMEMS website
# No need to change anything here except for the --user and --pwd at the end
# Please place your CMEMS username and password in those fields
GLORYS_script <- 'python ~/motuclient-python/motuclient.py --motu http://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -180 --longitude-max 179.9166717529297 --latitude-min -80 --latitude-max 90 --date-min "2018-12-25 12:00:00" --date-max "2018-12-25 12:00:00" --depth-min 0.493 --depth-max 0.4942 --variable thetao --variable bottomT --variable so --variable zos --variable uo --variable vo --variable mlotst --variable siconc --variable sithick --variable usi --variable vsi --out-dir data --out-name test.nc --user cfree --pwd Overmars.14!'

date_choice <- as.Date("2019-01-01")

# Prep the necessary URL pieces
date_start <- parse_date(date_choice, format = "%Y-%m")
# A clever way of finding the end date of any month!
# I found this on stackoverflow somewhere...
date_end <- date_start %m+% months(1) - 1

# Cannot get data past 2018-12-25
if(date_end > as.Date("2018-12-25")) date_end <- as.Date("2018-12-25")

# Set the file name
file_name <- paste0("GLORYS_",date_choice,".nc")

# Take the chunk of code above and turn it into something useful
cfg <- parse.CMEMS.script(GLORYS_script, parse.user = T)

# This is where one should make any required changes to the subsetting of the data
# This is now the magic of the RCMEMS package, which allows us to interface with the Python code as though it were R
cfg_update <- RCMEMS::update(cfg, variable = "thetao --variable bottomT --variable so --variable zos --variable uo --variable vo --variable mlotst --variable siconc --variable sithick --variable usi --variable vsi",
                             longitude.min = "-80.5",
                             longitude.max = "-40.5",
                             latitude.min = "31.5",
                             latitude.max = "63.5",
                             date.min = as.character(date_start),
                             date.max = as.character(date_end),
                             out.name = file_name)

devtools::install_github("markpayneatwork/RCMEMS")

# Install Python
# Install Java
# Install setuptools

# https://theoceancode.netlify.app/post/dl_env_data_r/
# https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/services
# https://github.com/markpayneatwork/RCMEMS
?RCMEMS::update


GLORYS_script <- 'python ~/motuclient-python/motuclient.py --motu http://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -180 --longitude-max 179.9166717529297 --latitude-min -80 --latitude-max 90 --date-min "2018-12-25 12:00:00" --date-max "2018-12-25 12:00:00" --depth-min 0.493 --depth-max 0.4942 --variable thetao --variable bottomT --variable so --variable zos --variable uo --variable vo --variable mlotst --variable siconc --variable sithick --variable usi --variable vsi --out-dir data --out-name test.nc --user cfree --pwd Overmars.14!'

cfg <- RCMEMS::parse.CMEMS.script(GLORYS_script)

cfg_update <- RCMEMS::update(cfg, 
                             variable = "thetao --variable bottomT --variable so --variable zos --variable uo --variable vo --variable mlotst --variable siconc --variable sithick --variable usi --variable vsi",
                             longitude.min = "-80.5",
                             longitude.max = "-40.5",
                             latitude.min = "31.5",
                             latitude.max = "63.5",
                             date.min = "2016-01-01",
                             date.max = "2016-01-02",
                             out.name = "test.nc")

RCMEMS::CMEMS.download(cfg_update)

