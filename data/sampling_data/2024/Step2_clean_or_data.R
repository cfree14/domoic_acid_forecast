
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

# To-do list
# Add coordinates

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Clean names
  janitor::clean_names("snake") %>%
  rename(species_tissue=shellfish_type,
         psp_ppm=psp_toxins,
         domoic_ppm=domoic_acid, 
         dcrab_area=dc_harvest_area,
         site_orig=site) %>% 
  # Build data
  mutate(date=paste(harvest_month, harvest_date, harvest_year) %>% lubridate::mdy(.)) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Format species
  separate(species_tissue, into=c("comm_name", "tissue"), sep=" \\(") %>% 
  mutate(comm_name=stringr::str_to_sentence(comm_name) %>% stringr::str_squish(.)) %>% 
  mutate(comm_name=recode(comm_name, 
                          "Bay clams"="Bay clam",
                          "Butters"="Butter clam",
                          "Cockles"="Cockle",
                          "Gapers"="Gaper clam", 
                          "Gooseneck barnacles"="Gooseneck barnacle",
                          "Littleneck clams"="Littleneck clam",
                          "Mussels"="Unidentified mussel",
                          "Oysters"="Unidentified oyster",
                          "Purple varnish clams"="Purple varnish clam",
                          "Razor clams"="Razor clam",
                          "Scallops"="Unidentified scallop",
                          "Soft shell clams"="Soft shell clam")) %>% 
  # Format tissue
  mutate(tissue=gsub(")", "", tissue)) %>% 
  # Convert numbers
  mutate(psp_ppm=as.numeric(psp_ppm),
         domoic_ppm=as.numeric(domoic_ppm)) %>% 
  # Format area
  mutate(area=recode(area, 
                     "UNK"="Unknown")) %>% 
  # Format Dcrab area
  mutate(dcrab_area=ifelse(dcrab_area==0, NA, dcrab_area),
         dcrab_area=stringr::str_to_title(dcrab_area),
         dcrab_area=recode(dcrab_area,
                           "50 J (Bandon)"="50-J (Bandon)",
                           "50-O (Columbia River))"="50-O (Columbia River)")) %>% 
  # Format site
  mutate(site=stringr::str_to_title(site_orig)) %>% 
  # Arrange
  select(-c(harvest_year, harvest_month, harvest_date)) %>% 
  select(year, date, area, site_orig, site, dcrab_area, 
         comm_name, tissue, psp_ppm, domoic_ppm, comments, everything())

 
# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$comm_name)
freeR::uniq(data$comm_name)
table(data$tissue)

# Area
table(data$area)
freeR::uniq(data$site)
table(data$dcrab_area)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "OR_biotoxin_data.Rds"))


