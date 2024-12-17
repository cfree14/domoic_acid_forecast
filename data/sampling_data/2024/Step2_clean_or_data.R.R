
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
data_orig <- readxl::read_excel(file.path(indir, "OR biotoxin Data 12.9.2024.xlsx"), col_types = "text")

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Clean names
  janitor::clean_names("snake") %>%
  rename(species_tissue=shellfish_type,
         psp_ppm=psp_toxins,
         domoic_ppm=domoic_acid) %>% 
  # Build data
  mutate(date=paste(harvest_month, harvest_date, harvest_year) %>% lubridate::mdy(.)) %>% 
  # Format species
  separate(species_tissue, into=c("species", "tissue"), sep=" \\(") %>% 
  mutate(species=stringr::str_to_sentence(species) %>% stringr::str_squish(.)) %>% 
  # Format tissue
  mutate(tissue=gsub(")", "", tissue)) %>% 
  # Convert numbers
  mutate(psp_ppm=as.numeric(psp_ppm),
         domoic_ppm=as.numeric(domoic_ppm)) %>% 
  # Arrange
  select(-c(harvest_year, harvest_month, harvest_date)) %>% 
  select(date, area, site, dc_harvest_area, species, tissue, psp_ppm, domoic_ppm, comments, everything())

 
# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$species)
table(data$tissue)

# Area
table(data$area)
table(data$site)
table(data$dc_harvest_area)



