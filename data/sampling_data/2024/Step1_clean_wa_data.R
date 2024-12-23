
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
data_orig <- readxl::read_excel(file.path(indir, "WA Coast Biotoxins 010191_112624.xlsx"), col_types = "text")

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Clean names
  janitor::clean_names("snake") %>% 
  rename(psp_id=psp_number,
         domoic_id=da_number,
         dsp_id=dsp_number,
         date_collected=collect_date,
         date_submitted=submit_date,
         organization=org,
         organization_id=cert_number,
         site=site_name,
         date_received=receive_date,
         comm_name=species,
         domoic_ppm=domoic_result,
         psp_ppm=psp_result,
         dsp_ppm=dsp_result) %>% 
  # Format species
  mutate(comm_name=str_to_sentence(comm_name)) %>% 
  # Format shell/shucked
  mutate(shell_shucked=stringr::str_to_sentence(shell_shucked)) %>% 
  # Format dates
  mutate(date_collected=as.numeric(date_collected) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.),
         date_submitted=as.numeric(date_submitted) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.),
         date_received=as.numeric(date_received) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.),
         domoic_date=as.numeric(domoic_date) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.),
         psp_date=as.numeric(psp_date) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.),
         dsp_date=as.numeric(dsp_date) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.)) %>% 
  # Add year
  mutate(year=lubridate::year(date_collected)) %>%
  # Format domoic (ppm) - 1 is lowest detected
  mutate(domoic_ppm=case_when(domoic_ppm %in% c("<1", "NTD") ~ "0", 
                              domoic_ppm %in% c("NoTest", "UNSAT") ~ NA,
                              T ~ domoic_ppm) %>% as.numeric()) %>% 
  # Format PSP ppm - 38 is lowest detected
  mutate(psp_ppm=case_when(psp_ppm %in% c("<38", "NTD") ~ "0", 
                           psp_ppm %in% c("NoTest", "UNSAT") ~ NA,
                           T ~ psp_ppm) %>% as.numeric()) %>% 
  # Format DSP (ppm) - 1 is lowest detected
  mutate(dsp_ppm=case_when(dsp_ppm %in% c("<1", "NTD") ~ "0",
                           dsp_ppm %in% c("No Test") ~ NA,
                           T ~ dsp_ppm) %>% as.numeric()) %>%
  # Arrange
  select(year, date_collected, date_submitted, date_received,
         county, waterbody, site_id, site, subsite,
         monitoring_type, sample_type, 
         organization, organization_id,
         comm_name, fresh_frozen, shell_shucked,
         domoic_id, domoic_date, domoic_tissue, domoic_ppm,
         psp_id, psp_date, psp_tissue, psp_ppm,
         dsp_id, dsp_date, dsp_tissue, dsp_ppm,
         everything())

# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$comm_name)
table(data$fresh_frozen)
table(data$shell_shucked)
table(data$monitoring_type)
table(data$sample_type)

# PPM
freeR::uniq(data$domoic_ppm)
freeR::uniq(data$psp_ppm)
freeR::uniq(data$dsp_ppm)


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

