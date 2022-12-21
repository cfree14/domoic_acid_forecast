
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
  # Format subsite quickly
  mutate(subsite=subsite %>% gsub("[^[:alnum:]]", " ", .) %>% stringr::str_squish() %>% stringr::str_to_title(),
         subsite=gsub(" To ", " to ", subsite),
         subsite=gsub("Wa Or", "WA/OR", subsite)) %>% 
  # Format subsite manually
  mutate(subsite=recode(subsite,
                        # "10 Fathom"=,                                         
                        # "15 Fathom"=,                                       
                        # "20 Fathom"=,                                       
                        # "240 Spit"=,                                      
                        # "Copalis"=,
                        # "From Southern End Of Unit Near Destruction Island"=,
                        # "Hogs Back"=,
                        # Columbia River
                        "Columbia River Inside North Jetty"="Inside Columbia River - North Jetty",
                        "Inside Columbia River North Jetty"="Inside Columbia River - North Jetty",
                        # Grays Harbor
                        "Grays Harbor Inside South Jetty"="Inside Grays Harbor - South Jetty",
                        "Inside Grays Harbor Damon Point"="Inside Grays Harbor - Damon Point",
                        "Inside Grays Harbor South Jetty"="Inside Grays Harbor - South Jetty",
                        # Long Beach - Cranberry
                        "Long Beach 7 Fathom Cranberry"="Long Beach - Cranberry (7 fathoms)",
                        "Long Beach Peninsula 7 Fathom Cranberry"="Long Beach - Cranberry (7 fathoms)",
                        "Long Beach 15 Fathom Cranberry"="Long Beach - Cranberry (15 fathoms)",
                        "Long Beach Peninsula 15 Fathom Cranberry"="Long Beach - Cranberry (15 fathoms)",
                        "Long Beach Peninsula 15 Fathom Cranberry"="Long Beach - Cranberry (15 fathoms)",
                        "Long Beach Peninsula 30 Fathom Cranberry"="Long Beach - Cranberry (30 fathoms)",
                        "Long Beach Peninsula 30 Fathom Cranberry"="Long Beach - Cranberry (30 fathoms)",
                        # Long Beach - Peacock Spit
                        "Long Beach 15 Fathom Peacock Spit"="Long Beach - Peacock Spit (15 fathoms)",
                        "Long Beach 7 Fathom Peacock Spit"="Long Beach - Peacock Spit (7 fathoms)",
                        "Long Beach Peacock Spit"="Long Beach - Peacock Spit",
                        "Long Beach Peninsula 7 Fathom Peacock Spit"="Long Beach - Peacock Spit (7 fathoms)",
                        "Long Beach Peninsula 15 Fathom Peacock Spit"="Long Beach - Peacock Spit (15 fathoms)",
                        # Long Beach - Seaview
                        "Long Beach Peninsula Seaview"="Long Beach - Seaview",
                        "Long Beach 7 Fathom Seaview"="Long Beach - Seaview (7 fathoms)",
                        "Long Beach 15 Fathom Seaview"="Long Beach - Seaview (15 fathoms)",
                        "Long Beach Peninsula 7 Fathom Seaview"="Long Beach - Seaview (7 fathoms)",
                        "Long Beach Peninsula 7 Fathom Seaview"="Long Beach - Seaview (7 fathoms)",
                        "Long Beach Peninsula 15 Fathom Seaview"="Long Beach - Seaview (15 fathoms)",
                        # "North"=
                        # "North Willapa Bay Tokeland"=
                        # "North Willapa Tokeland"=
                        # "Ocean Shores"=
                        # Point Chehalis to somewhere
                        "Point Chehalis to Copalis River Copalis"="Point Chehalis to Copalis River - Copalis",
                        "Point Chehalis to Copalis River Ocean Shores"="Point Chehalis to Copalis River - Ocean Shores",
                        "Point Chehalis to Destruction Island Copalis"="Point Chehalis to Destruction Island - Copalis",
                        "Point Chehalis to Destruction Island Ocean Shores"="Point Chehalis to Destruction Island - Ocean Shores",
                        "Point Chehalis to Klipsan Â Grayland"="Point Chehalis to Klipsan - Grayland",
                        "Point Chehalis to Klipsan Â Outside Willapa"="Point Chehalis to Klipsan - Outside Willapa",
                        "Point Chehalis to Klipsan Grayland"="Point Chehalis to Klipsan - Grayland",
                        "Point Chehalis to Klipsan Ocean Park"="Point Chehalis to Klipsan - Ocean Park",
                        "Point Chehalis to Klipsan Outside Willapa"="Point Chehalis to Klipsan - Outside Willapa",
                        "Point Chehalis to Klipsan Westport"="Point Chehalis to Klipsan - Westport",
                        # "Queets"=
                        # "South"=
                        # "Subsite A"=
                        # "Subsite B"=
                        # "Tokeland Area"=
                        # "Tokeland Reservation"=
                        # "Tunnel Island"=
                        # WA/OR border to Klipsan
                        "Or Wa Border to Klipsan Ocean Park"="WA/OR Border to Klipsan - Ocean Park",
                        "Or Wa Border to Klipsan Seaview"="WA/OR Border to Klipsan - Seaview",
                        "WA/OR Border to Klipsan Â Peacock Spit"="WA/OR Border to Klipsan - Peacock Spit",
                        "WA/OR Border to Klipsan Cranberry"="WA/OR Border to Klipsan - Cranberry",
                        "WA/OR Border to Klipsan Long Beach"="WA/OR Border to Klipsan - Long Beach",
                        "WA/OR Border to Klipsan Peacock Spit"="WA/OR Border to Klipsan - Peacock Spit",
                        "WA/OR Border to Klipsan Seaview"="WA/OR Border to Klipsan - Seaview",
                        # Westport - Grayland
                        "Inside Westport Boat Basin"="Westport - Inside Westport Boat Basin",
                        "Westport Grayland"="Westport - Grayland",
                        "Westport 7 Fathom Grayland"="Westport - Grayland (7 fathoms)",
                        "Westport 15 Fathom Grayland"="Westport - Grayland (15 fathoms)",
                        "Westport Test 7 Fathom Grayland"="Westport - Grayland (7 fathoms)",
                        "Westport Test 15 Fathom Grayland"="Westport - Grayland (15 fathoms)",
                        # Westport - Outside Willapa
                        "Westport Outside Willapa"="Westport - Outside Willapa",
                        "Westport 7 Fathom Outside Willapa"="Westport - Outside Willapa (7 fathoms)",
                        "Westport 7 Fathom Willapa Outside"="Westport - Outside Willapa (7 fathoms)",
                        "Westport Test 7 Fathom Willapa Outside"="Westport - Outside Willapa (7 fathoms)",
                        "Westport 15 Fathom Outside Willapa"="Westport - Outside Willapa (15 fathoms)",
                        "Westport 15 Fathom Willapa Outside"="Westport - Outside Willapa (15 fathoms)",
                        "Westport Test 15 Fathom Willapa Outside"="Westport - Outside Willapa (15 fathoms)")) %>% 
  # Arrange
  select(year, month, date,
         state, county, waterbody, site, subsite, organization,
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


# Add lat/long
################################################################################

# Steps
# 1) Make progress programatically
# 2) Finihs it manually
# 3) Read in new and add

# Old data
old_data <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/washington/da_sampling/data/WA_DOH_2000_2020_biotoxin_sampling_data.Rds")

# Old site key
site_key_old <- old_data %>% 
  select(waterbody, site, subsite, lat_dd, long_dd) %>% 
  unique() %>% 
  mutate(site=stringr::str_squish(site), 
         subsite=ifelse(subsite=="", NA, subsite) %>% stringr::str_squish()) %>% 
  mutate(subsite=recode(subsite,
                        "Long Beach Peninsula Seaview (7 Fathoms)"="Long Beach - Seaview (7 fathoms)",
                        "Long Beach Peninsula Seaview (15 Fathoms)"="Long Beach - Seaview (15 fathoms)",
                        "Long Beach Peninsula Peacock Spit (7 Fathoms)"="Long Beach - Peacock Spit (7 fathoms)",
                        "Long Beach Peninsula Peacock Spit (15 Fathoms)"="Long Beach - Peacock Spit (15 fathoms)"))

# Step 1) Partial
site_key <- data %>% 
  count(county, waterbody, site, subsite) %>% 
  left_join(site_key_old)
write.csv(site_key, file=file.path(indir, "washington/site_key_partial.csv"))

# Step 2) Read site key
site_key_new <- readxl::read_excel(file.path(indir, "washington/site_key_manually_added.xlsx"), na="NA") 

# Step 3) Add data
data_xy <- data %>% 
  # Add data
  left_join(site_key_new %>% select(site, subsite, lat_dd, long_dd), by=c("site", "subsite")) %>% 
  # Arrange
  select(year, month, date,
         state, county, waterbody, site, subsite, lat_dd, long_dd, organization,
         comm_name, sci_name, tissue, 
         da_id, domoic_ppm, psp_id, psp_ug100g, dsp_id, dsp_ug100g, comments, everything())
  
# Inspect
str(data_xy)
freeR::complete(data_xy)


# Export data
################################################################################

# Save data
saveRDS(data_xy, file=file.path(outdir, "WA_2021_2022_biotoxin_data.Rds"))


