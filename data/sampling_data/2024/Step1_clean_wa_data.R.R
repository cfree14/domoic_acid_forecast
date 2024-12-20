
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
data_orig <- readxl::read_excel(file.path(indir, "WA Coast Biotoxins 010191_112624.xlsx"))

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Clean names
  janitor::clean_names("snake") %>% 
  rename(psp_id=psp_number,
         da_id=da_number,
         dsp_id=dsp_number,
         date_collected=collect_date,
         date_submitted=submit_date,
         organization=org,
         organization_id=cert_number,
         site=site_name,
         date_received=receive_date,
         comm_name=species) %>% 
  # Format species
  mutate(comm_name=str_to_sentence(comm_name)) %>% 
  # Add year
  mutate(year=lubridate::year(date_collected)) %>% 
  # Arrange
  select(year, date_collected, date_submitted, date_received,
         county, waterbody, organization, organization_id,
         site_id, site, subsite,
         everything())

# Inspect
str(data)

# Inspect
table(data$comm_name)
table(data$county)
table(data$fresh_frozen)
table(data$shell_shucked)

# Organization key (not perfect)
org_key <- data %>% 
  count(organization_id, organization)

# County/waterbody
data %>% 
  count(county, waterbody)

# Site key
site_key <- data %>% 
  count(site_id, site, subsite)

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "WA_biotoxin_data.Rds"))
