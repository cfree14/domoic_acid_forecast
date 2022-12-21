
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/moore/raw"
outdir <- "data/moore/processed"

# Read WA site key
site_key_orig <- read.csv("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/washington/da_sampling/data/WA_DOH_biotoxin_sampling_site_key_with_site_id.csv", as.is=T)


# Format data
################################################################################

# Read data
data_orig1 <- readxl::read_excel(file.path(indir, "washington", "Crab and Razor Clam Biotoxins 2022.xlsx"), 
                                 skip=2, na=c("NoTest"))

# Format data
data1 <- data_orig1 %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(psp_id=psp_number,
         da_id=da_number,
         dsp_id=dsp_number,
         date=collect_date,
         organization=org,
         site=site_name,
         comm_name=species, 
         psp_ug100g=psp_result,
         domoic_ppm=domoic_result,
         dsp_ug100g=dsp_result,
         comments=sample_comments) %>% 
  # Format date
  mutate(date=lubridate::ymd(date),
         year=lubridate::year(date), 
         month=lubridate::month(date)) %>% 
  # Format species
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Format results
  mutate(domoic_ppm=ifelse(domoic_ppm %in% c("<1", "NTD"), 0, domoic_ppm) %>% as.numeric(),
         psp_ug100g=ifelse(psp_ug100g %in% c("<38", "NTD"), 0, psp_ug100g) %>% as.numeric(),
         dsp_ug100g=ifelse(dsp_ug100g %in% c("<1", "NTD"), 0, dsp_ug100g) %>% as.numeric()) %>% 
  # Arrange
  select(year, month, date, organization, everything())
  

# Inspect
str(data1)
freeR::complete(data1)
table(data1$county)
table(data1$waterbody)
table(data1$organization)
table(data1$site)
table(data1$subsite)
table(data1$comm_name)
table(data1$tissue)
sort(unique(data1$domoic_ppm))
sort(unique(data1$psp_ug100g))
sort(unique(data1$dsp_ug100g))


# Format data
################################################################################

# Read data
data_orig2 <- readxl::read_excel(file.path(indir, "washington", "2021 WDOH DA for clams and crab.xlsx"), 
                                 skip=2, na=c("NoTest"))

# Format data
data2 <- data_orig2 %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(
         da_id=da_number,
         date=collect_date,
         organization=org,
         site=site_name,
         comm_name=species, 
         domoic_ppm=domoic_result) %>% 
  # Format date
  mutate(date=lubridate::ymd(date),
         year=lubridate::year(date), 
         month=lubridate::month(date)) %>% 
  # Format species
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Format results
  mutate(domoic_ppm=ifelse(domoic_ppm %in% c("<1", "NTD"), 0, domoic_ppm) %>% as.numeric()) %>% 
  # Arrange
  select(year, month, date, organization, everything())


# Inspect
str(data2)
freeR::complete(data2)
table(data2$county)
table(data2$waterbody)
table(data2$organization)
table(data2$site)
table(data2$subsite)
table(data2$comm_name)
table(data2$tissue)
sort(unique(data2$domoic_ppm))


# Format data
################################################################################

# Read data
data_orig3 <- readxl::read_excel(file.path(indir, "washington", "2021 WDOH DA for clams and crab December.xlsx"), 
                                 skip=2, na=c("NoTest"))

# Format data
data3 <- data_orig3 %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(
    da_id=da_number,
    date=collect_date,
    organization=org,
    site=site_name,
    comm_name=species, 
    domoic_ppm=domoic_result) %>% 
  # Format date
  mutate(date=lubridate::ymd(date),
         year=lubridate::year(date), 
         month=lubridate::month(date)) %>% 
  # Format species
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Format results
  mutate(domoic_ppm=ifelse(domoic_ppm %in% c("<1", "NTD"), 0, domoic_ppm) %>% as.numeric()) %>% 
  # Arrange
  select(year, month, date, organization, everything())

# Inspect
str(data3)
freeR::complete(data3)
table(data3$county)
table(data3$waterbody)
table(data3$organization)
table(data3$site)
table(data3$subsite)
table(data3$comm_name)
table(data3$tissue)
sort(unique(data3$domoic_ppm))


# Merge data
################################################################################

# Merge data
data <- bind_rows(data1, data2, data3) %>% 
  # Arrange
  arrange(date) %>% 
  # Add columns
  mutate(state="Washington",
         sci_name=recode(comm_name,
                         "Dungeness crab"="Metacarcinus magister",
                         "Razor clam"="Siliqua patula")) %>% 
  # Add lat/long
  left_join(site_key_orig) %>% 
  # Arrange
  select(year, month, date,
         state, county, waterbody, site_id, site, subsite, lat_dd, long_dd,
         organization,
         comm_name, sci_name, tissue, 
         da_id, domoic_ppm, psp_id, psp_ug100g, dsp_id, dsp_ug100g, comments, everything())

# Any duplicates?
freeR::which_duplicated(data$da_id)
freeR::which_duplicated(data$psp_id)
freeR::which_duplicated(data$dsp_id)

# Inspect
str(data)
freeR::complete(data)

# Inspect more
range(data$date)
table(data$county)
table(data$waterbody)
table(data$organization)
table(data$site)
table(data$subsite)
table(data$comm_name)
table(data$tissue)
sort(unique(data$domoic_ppm))


# Export data
################################################################################

# Save data
saveRDS(data, file=file.path(outdir, "WA_2021_2022_biotoxin_data.Rds"))


