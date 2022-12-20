
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
  # Add Dcrab area as site name (more detailed than site (Pacific Ocean))
  mutate(site=ifelse(!is.na(dcrab_area), dcrab_area, site)) %>% 
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
         shellfish_type, comm_name, sci_name, tissue, domoic_ppm, psp_ug100g, comments, everything()) %>% 
  # Remove useless
  select(-c(dcrab_area, shellfish_type))

# Inspect
str(data)
freeR::complete(data)

# Check ids
freeR::which_duplicated(data$sample_id)

# Inspect more
range(data$date)
table(data$area)
table(data$site)
# table(data$dcrab_area)
# table(data$shellfish_type)
table(data$comm_name)
table(data$tissue)


# Add lat/long
################################################################################

# Get lat/long for sites in old data

# Read old data
or_old <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/oregon/processed/ODA_2000_2020_da_sampling_data_final.Rds")
site_key_old <- or_old %>% 
  select(location, lat_dd, long_dd) %>% 
  unique()

# Site key
site_key <- data %>% 
  count(area, site) %>% 
  arrange(site) %>% 
  mutate(site_match=recode(site, 
                           # "15th Street"    
                           "Agate Beach"="Newport Beach-Agate Beach",
                           # "Baker Beach"            
                           "Bob Creek"="Bob Creek Wayside",
                           "Cape Meares"="Cape Meares-Bayocean Spit",
                           # "Coos Bay"              
                           "Coos N Jetty & Spit"="Coos Bay-North Jetty",
                           "Gold Beach"="Gold Beach-Myers Creek",
                           "Neptune St Park"="Cape Perpetua-Neptune State Park",
                           "North Jetty"="Newport Beach-North Jetty",
                           "Seal Rock State Park"="Seal Rock State Park",
                           "Silver Point"="Silver Point-Arcadia Beach State Park",
                           "South Jetty"="Clatsop Beach-South Jetty",
                           # "Sparrow Park"           
                           "Sunset Beach"="Clatsop Beach-Sunset",
                           "Whiskey Run"="Whiskey Run Beach",
                           "Yachats"="Yachats River",
                           "50-A (Astoria)"="50-A - OR/WA Border to Cape Falcon",       
                           "50-B (Garibaldi)"="50-B - Cape Falcon to Cape Lookout",  
                           "50-C (Pacific City)"="50-C - Cape Lookout to Cascade Head",   
                           "50-D (Depoe Bay)"="50-D - Cascade Head to Cape Foulweather",      
                           "50-E (Newport)"="50-E - Cape Foulweather to Waldport",      
                           "50-F (Waldport)"="50-F - Waldport to Heceta Head",       
                           "50-G (Florence)"="50-G - Heceta Head to Takenitch Creek",        
                           "50-H (Winchester Bay)"="50-H - Takenitch Creek to North Bend",
                           "50-I (Coos Bay)"="50-I - North Bend to Bandon",       
                           "50-J (Bandon)"="50-J - Bandon to Cape Blanco",        
                           "50-K (Port Orford)"="50-K - Cape Blanco to Gold Beach",     
                           "50-L (Brookings)"="50-L - Gold Beach to OR/CA Border",      
                           "50-O (Columbia River))"="50-O - Columbia River")) %>% 
  # Add coordinates
  left_join(site_key_old, by=c("site_match"="location")) %>% 
  # Add missing manually
  mutate(lat_dd=ifelse(site=="15th Street", 44.975419, lat_dd),
         long_dd=ifelse(site=="15th Street", -124.017121, long_dd))
# write.csv(site_key, file.path(indir, "oregon_site_key.csv"), row.names = F)

# Add
data_xy <- data %>% 
  # Add lat/long
  left_join(site_key %>% select(site, lat_dd, long_dd), by="site") %>% 
  # Arrange
  select(sample_id, year, month, date, 
         state, area, site, lat_dd, long_dd,
         comm_name, sci_name, tissue, domoic_ppm, psp_ug100g, comments, everything())

# Inspect
str(data_xy)
freeR::complete(data_xy)
  

# Export data
################################################################################

# Save data
saveRDS(data_xy, file=file.path(outdir, "OR_2021_2022_biotoxin_data.Rds"))


