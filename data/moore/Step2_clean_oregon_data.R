
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/moore/raw"
outdir <- "data/moore/processed"


# Format data
################################################################################

# Read data
data_orig1 <- readxl::read_excel(file.path(indir, "oregon", "2021 Oregon Shellfish Toxin Data .xlsx"))

# Format data
data1 <- data_orig1 %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=id,
         month=harvest_month,
         day=harvest_date,
         year=harvest_year,
         domoic_ppm=domoic_acid,
         psp_ug100g=psp_toxins,
         dcrab_area=dc_harvest_area) %>% 
  # Format date
  mutate(date=lubridate::ymd(paste(year, month, day, sep="-"))) %>% 
  # Format site
  mutate(site=stringr::str_to_title(site),
         dcrab_area=stringr::str_to_title(dcrab_area)) %>% 
  # Format comm name
  mutate(comm_name=recode(shellfish_type,
                          "Dungeness Crab (viscera)"="Dungeness crab",                  
                          "Mussels"="Mussel" ,            
                          "Razor Clams"="Razor clam")) %>% 
  # Format tissue type
  mutate(tissue=ifelse(shellfish_type=="Dungeness Crab (viscera)", "viscera", "meat")) %>% 
  # Format sample id
  mutate(sample_id=as.character(sample_id)) %>% 
  # Arrange
  select(-day) %>% 
  select(year, month, date, sample_id,
         area, site, dcrab_area, 
         shellfish_type, comm_name, tissue, domoic_ppm, psp_ug100g, comments, everything())
  
# Inspect
str(data1)
freeR::complete(data1)

# Inspect ids
freeR::which_duplicated(data1$sample_id)

# Inspect more
table(data1$area)
table(data1$site)
table(data1$dcrab_area)
table(data1$shellfish_type)
table(data1$comm_name)


# Format data
################################################################################

# Read data
data_orig2 <- readxl::read_excel(file.path(indir, "oregon", "Oregon Toxin Data 2022.xlsx"))

# Format data
data2 <- data_orig2 %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(month=harvest_month,
         day=harvest_date,
         year=harvest_year,
         domoic_ppm=domoic_acid,
         psp_ug100g=psp_toxins,
         dcrab_area=dc_harvest_area) %>% 
  # Format date
  mutate(date=lubridate::ymd(paste(year, month, day, sep="-"))) %>% 
  # Format site
  mutate(site=stringr::str_to_title(site),
         dcrab_area=stringr::str_to_title(dcrab_area)) %>% 
  # Format comm name
  mutate(comm_name=recode(shellfish_type,
                          "Dungeness Crab (viscera)"="Dungeness crab",
                          "Dungeness Crab (meat)"="Dungeness crab",
                          "Mussels"="Mussel",
                          "Oysters"="Oyster",
                          "Purple Varnish Clams"="Purple varnish clam",
                          "Razor Clams"="Razor clam")) %>% 
  # Format tissue type
  mutate(tissue=ifelse(shellfish_type=="Dungeness Crab (viscera)", "viscera", "meat")) %>% 
  # Add id
  mutate(sample_id=paste("OR", date, area, comm_name, tissue, sep="-") %>% make.unique()) %>% 
  # Arrange
  select(-day) %>% 
  select(year, month, date, 
         area, site, dcrab_area, 
         shellfish_type, comm_name, tissue, domoic_ppm, psp_ug100g, comments, everything())

# Inspect
str(data2)
freeR::complete(data2)

# Unique ids?
freeR::which_duplicated(data2$sample_id)

# Inspect more
table(data2$area)
table(data2$site)
table(data2$dcrab_area)
table(data2$shellfish_type)
table(data2$comm_name)
table(data2$tissue)


# Merge data
################################################################################

# Merge data
data <- bind_rows(data1, data2) %>% 
  arrange(date) %>% 
  # Add state
  mutate(state="Oregon") %>% 
  # Add scientific name
  mutate(sci_name=recode(comm_name,
                         "Dungeness crab"="Metacarcinus magister",
                         "Mussel"="Mytilus spp.",
                         "Oyster"="Oyster spp.",
                         "Purple varnish clam"="Nuttallia obscurata",          
                         "Razor clam"="Siliqua patula")) %>% 
  # Format month
  mutate(month=lubridate::month(date)) %>% 
  # Arrange
  select(sample_id, year, month, date, 
         state, area, site, dcrab_area, 
         shellfish_type, comm_name, sci_name, tissue, domoic_ppm, psp_ug100g, comments, everything())

# Inspect
str(data)
freeR::complete(data)

# Check ids
freeR::which_duplicated(data$sample_id)

# Inspect more
range(data$date)
table(data$area)
table(data$site)
table(data$dcrab_area)
table(data$shellfish_type)
table(data$comm_name)
table(data$tissue)

# Site key
site_key <- data %>% 
  count(area, site, dcrab_area)


# Export data
################################################################################

# Save data
saveRDS(data, file=file.path(outdir, "OR_2021_2022_biotoxin_data.Rds"))


