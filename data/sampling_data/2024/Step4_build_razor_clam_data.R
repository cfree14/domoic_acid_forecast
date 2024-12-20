
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(lubridate)

# Directories
indir <- "data/sampling_data/2024/raw"
outdir <- "data/sampling_data/2024/processed"
plotdir <- "figures"

# Read data
ca_orig <- readRDS(file.path(outdir, "CA_biotoxin_data_cdph.Rds"))
or_orig <- readRDS(file.path(outdir, "OR_biotoxin_data.Rds"))
wa_orig <- readRDS(file.path(outdir, "WA_biotoxin_data.Rds"))


# Read data
################################################################################

# California
ca <- ca_orig %>% 
  # Razors
  filter(comm_name=="Razor clam") %>% 
  # Add
  mutate(state="California") %>% 
  # Rename
  rename(site=sample_site,
         comments=notes) %>% 
  # Simplify
  select(state, year, date, 
         site, lat_dd, long_dd, 
         comm_name, tissue, type, n_indivs, domoic_ppm, domoic_ppm_mod, comments)

# Oregon
or <- or_orig %>% 
  # Razors
  filter(comm_name=="Razor clam") %>% 
  # Add
  mutate(state="Oregon") %>% 
  # Simplify
  select(state, year, date, 
         site, #lat_dd, long_dd, 
         comm_name, tissue, domoic_ppm, comments)

# Washington
wa <- wa_orig %>% 
  # Razors
  filter(comm_name=="Razor clam") %>% 
  # Add
  mutate(state="Washington") %>% 
  # Rename
  rename(date=date_collected,
         tissue=domoic_tissue,
         domoic_ppm=domoic_result) %>% 
  # Simplify
  select(state, year, date, 
         site, #lat_dd, long_dd, 
         comm_name, tissue) #, domoic_ppm)

# Merge
data <- bind_rows(ca, or, wa)

# Inspect
str(data)
freeR::complete(data)


table(data$tissue)
table(data$type)




