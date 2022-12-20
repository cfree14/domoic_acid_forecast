
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/moore/raw"
outdir <- "data/moore/processed"

# Read data
data_orig1 <- readxl::read_excel(file.path(indir, "california", "CDPH_Razor-clam_DA_2021.xls.xlsx"))
data_orig2 <- readxl::read_excel(file.path(indir, "california", "CDPH_razorclam_DA_2022.xls.xlsx"))
data_orig3 <- readxl::read_excel(file.path(indir, "california", "CDPH_Dcrab_DA_2000-2015.xls.xlsx"))
data_orig4a <- readxl::read_excel(file.path(indir, "california", "1. Domoic Acid Test Results 2021-2022 - HAB data request.xlsx"), sheet="Crab")
data_orig4b <- readxl::read_excel(file.path(indir, "california", "1. Domoic Acid Test Results 2021-2022 - HAB data request.xlsx"), sheet="Meat")
data_orig5a <- readxl::read_excel(file.path(indir, "california", "1. Domoic Acid Test Results 2021-2022 D-crab NOAA.xlsx"), sheet="Crab")
data_orig5b <- readxl::read_excel(file.path(indir, "california", "1. Domoic Acid Test Results 2021-2022 D-crab NOAA.xlsx"), sheet="Meat")
data_orig6 <- readxl::read_excel(file.path(indir, "california", "1. Domoic Acid Test Results 2022-2023 D-crab NOAA.xlsx"), na="NA")


# Format data 1
################################################################################

# Format data
data1 <- bind_rows(data_orig1, data_orig2, data_orig3) %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=srl_number,
         date=date_sampled,
         site=sample_site,
         lat_dd=latitude, 
         long_dd=longitude,
         sample_n=number_of_individuals,
         comments=notes,
         domoic_ppm=asp_ug_g,
         domoic_mod=mod_asp,
         sort_id=nto_s_code_can_sort_counties_in_n_to_s_order) %>% 
  # Format date
  mutate(date=lubridate::ymd(date),
         year=lubridate::year(date), 
         month=lubridate::month(date)) %>% 
  # Add common name
  mutate(comm_name=recode(sample_type, 
                          "Clam, razor"="Razor clam",               
                          "Crab, Dungeness, body meat"="Dungeness crab",
                          "Crab, Dungeness, leg meat"="Dungeness crab",
                          "Crab, Dungeness, viscera"="Dungeness crab",
                          "Razor Clam meat"="Razor clam")) %>% 
  # Add tissue
  mutate(tissue=recode(sample_type, 
                       "Clam, razor"="meat",               
                       "Crab, Dungeness, body meat"="body meat",
                       "Crab, Dungeness, leg meat"="leg meat",
                       "Crab, Dungeness, viscera"="viscera",
                       "Razor Clam meat"="meat")) %>% 
  # Arrange
  select(-sort_id) %>% 
  select(year, month, date, sample_id,
         county, site, lat_dd, long_dd, 
         sample_type, comm_name, tissue,
         sample_n, domoic_ppm, domoic_mod,
         comments, everything())  %>% 
  select(-sample_type)
  
# Inspect
str(data1)
table(data1$county)
table(data1$site)
table(data1$comm_name)
table(data1$tissue)
sort(unique(data1$sample_type))


# Format data 2
################################################################################

# Format data
data2 <- data_orig2 %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=srl_number,
         date=date_sampled,
         lat_dd=latitude,
         long_dd=longitude,
         sample_n=number_of_individuals,
         site=sample_site,
         domoic_ppm=asp_ug_g,
         domoic_mod=mod_asp) %>% 
  # Format date
  mutate(date=lubridate::ymd(date)) %>% 
  # Add species and type
  mutate(comm_name="Razor clam", 
         tissue="meat") 

# Inspect
str(data2)

# Inspect more
table(data2$county)
table(data2$site)

# Format data 3
################################################################################

# Format data
data3 <- data_orig3 %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=srl_number,
         date=date_sampled,
         lat_dd=latitude,
         long_dd=longitude,
         site=sample_site,
         comments=notes,
         domoic_ppm=asp_ug_g,
         domoic_mod=mod_asp,
         county_order=nto_s_code_can_sort_counties_in_n_to_s_order) %>% 
  # Format date
  mutate(date=lubridate::ymd(date)) %>% 
  # Add species and type
  mutate(comm_name="Dungeness crab",
         tissue=gsub("Crab, Dungeness, |", "", sample_type))

# Inspect
str(data3)

# Inspect more
table(data3$county)
table(data3$site)
table(data3$sample_type)
table(data3$tissue)


# Format data 4a
################################################################################

# Convert DMS to DD
x <- "34 55.66"
conv_dms <- function(x){
  x_split <- str_split(x, pattern=" ")
  x1 <- x_split[[1]][1] %>% as.numeric()
  x2 <- x_split[[1]][2] %>% as.numeric()
  x_dd <- x1 + x2/60
  return(x_dd)
}

# Format data
data4a <- data_orig4a %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=is_number,
         date=date_of_catch,
         comm_name=species_viscera,
         site=collection_sites,
         block_id=block_number, 
         latlong=lat_long_coordinates,
         domoic_ppm=result_ppm_fda_action_30) %>% 
  # Format date
  mutate(date=lubridate::ymd(date),
         year=lubridate::year(date), 
         month=lubridate::month(date)) %>% 
  # Format domoic result
  mutate(domoic_ppm=recode(domoic_ppm, "<2.5"="0") %>% as.numeric()) %>% 
  # Format common name and tissue
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         tissue="viscera") %>% 
  # Format lat/long
  mutate(latlong=gsub("- ", "-", latlong)) %>% 
  separate(latlong, into=c("lat_dms", "long_dms"), sep="-", remove=F) %>% 
  rowwise() %>% 
  mutate(lat_dd=conv_dms(lat_dms),
         long_dd=conv_dms(long_dms)*-1) %>% 
  ungroup() %>% 
  # Arrange
  select(year, month, date, sample_id,
         port, site, block_id, 
         latlong, lat_dms, long_dms, lat_dd, long_dd, 
         depth_fathoms,
         comm_name, tissue, domoic_ppm, everything())

# Inspect
str(data4a)
table(data4a$port)
table(data4a$site)
table(data4a$comm_name)


# Format data 4b 
################################################################################

# Format data
data4b <- data_orig4b %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=is_number,
         date=date_of_catch,
         comm_name=species,
         tissue=type,
         block_id=block_number, 
         site=area,
         meat_ppm=meat_result_ppm,
         viscera_ppm=viscera_result_ppm,
         meat_div_visc=meat_viscera_percent) %>% 
  # Format date
  mutate(date=lubridate::ymd(date),
         year=lubridate::year(date), 
         month=lubridate::month(date)) %>% 
  # Format common name and tissue
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         tissue="viscera") %>% 
  # Arrange
  select(year, month, date, sample_id,
         port, site, block_id, depth_fathoms,
         comm_name, tissue, meat_ppm, viscera_ppm, everything())


# Inspect
str(data4b)
table(data4b$port)
table(data4b$comm_name)



# Format data 5a
################################################################################

# Format data
data5a <- data_orig5a %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=is_number,
         date=date_of_catch,
         comm_name=species_viscera,
         site=collection_sites,
         block_id=block_number, 
         latlong=lat_long_coordinates,
         domoic_ppm=result_ppm_fda_action_30) %>% 
  # Format date
  mutate(date=lubridate::ymd(date),
         year=lubridate::year(date), 
         month=lubridate::month(date)) %>% 
  # Format domoic result
  mutate(domoic_ppm=recode(domoic_ppm, "<2.5"="0") %>% as.numeric()) %>% 
  # Format common name and tissue
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         tissue="viscera") %>% 
  # Format lat/long
  mutate(latlong=gsub("- ", "-", latlong)) %>% 
  separate(latlong, into=c("lat_dms", "long_dms"), sep="-", remove=F) %>% 
  rowwise() %>% 
  mutate(lat_dd=conv_dms(lat_dms),
         long_dd=conv_dms(long_dms)*-1) %>% 
  ungroup() %>% 
  # Arrange
  select(year, month, date, sample_id,
         port, site, block_id, 
         latlong, lat_dms, long_dms, lat_dd, long_dd, 
         depth_fathoms,
         comm_name, tissue, domoic_ppm, everything())

# Inspect
str(data5a)
table(data5a$port)
table(data5a$comm_name)


# Format data 5b
################################################################################

# Format data
data5b <- data_orig5b %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=is_number,
         date=date_of_catch,
         comm_name=species,
         tissue=type,
         block_id=block_number, 
         site=area,
         meat_ppm=meat_result_ppm,
         viscera_ppm=viscera_result_ppm) %>% 
  # Format date
  mutate(date=lubridate::ymd(date),
         year=lubridate::year(date), 
         month=lubridate::month(date)) %>% 
  # Format common name and tissue
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         tissue="viscera") %>% 
  # Arrange
  select(year, month, date, sample_id,
         port, site, block_id, depth_fathoms,
         comm_name, tissue, meat_ppm, viscera_ppm, everything())

# Inspect
str(data5b)
table(data5b$port)
table(data5b$comm_name)



# Format data 6
################################################################################

# Format data
data6 <- data_orig6 %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=is_number,
         date=date_of_catch,
         comm_name=species_viscera,
         site=collection_sites,
         block_id=block_number, 
         latlong=lat_long_coordinates,
         domoic_ppm=result_ppm_fda_action_30) %>% 
  # Format date
  mutate(date=lubridate::ymd(date),
         year=lubridate::year(date), 
         month=lubridate::month(date)) %>% 
  # Format domoic result
  mutate(domoic_ppm=recode(domoic_ppm, "<2.5"="0") %>% as.numeric()) %>% 
  # Format common name and tissue
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         tissue="viscera") %>% 
  # Format lat/long
  mutate(latlong=gsub("- ", "-", latlong)) %>% 
  separate(latlong, into=c("lat_dms", "long_dms"), sep="-", remove=F) %>% 
  rowwise() %>% 
  mutate(lat_dd=conv_dms(lat_dms),
         long_dd=conv_dms(long_dms)*-1) %>% 
  ungroup() %>% 
  # Arrange
  select(year, month, date, sample_id,
         port, area, site, 
         latlong, lat_dms, long_dms, lat_dd, long_dd, 
         block_id, depth_fathoms,
         comm_name, tissue, domoic_ppm,  everything())

# Inspect
str(data6)
table(data6$port)
table(data6$comm_name)


# Merge data
################################################################################

# Column names
colnames(data1)
colnames(data2)
colnames(data3)
colnames(data4a)
colnames(data4b) # exclude, meat vs. viscera experiment, viscera would get duplicated
colnames(data5a)
colnames(data5b) # exclude, meat vs. viscera experiment, viscera would get duplicated
colnames(data6)

# Merge data
data <- bind_rows(data1, data2, data3, data4a, data5a, data6) %>% 
  # Add state
  mutate(state="California") %>% 
  # Dervie year and month
  mutate(year=lubridate::year(date),
         month=lubridate::month(date)) %>% 
  # Format depths
  mutate(depth_fathoms=recode(depth_fathoms, 
                              "<30 inside"="<30",
                              ">30 outside"=">30",
                              "deep outside of 30 fathoms"=">30",
                              "shallow inside of 30 fathoms"="<30")) %>% 
  # Format common name
  mutate(comm_name=recode(comm_name,
                          "Box crab"="Brown box crab",
                          "King crab"="Scarlet king crab")) %>% 
  # Add scientific name
  mutate(sci_name=recode(comm_name, 
                         "Brown box crab"="Lopholithodes foraminatus",
                         "Dungeness crab"="Metacarcinus magister", 
                         "Scarlet king crab"="Lithodes couesi",
                         "Razor clam"="Siliqua patula",
                         "Rock crab"="Cancer spp.")) %>% 
  # Remove useless
  # I'm going to remove area because it only occurs in 6 and its basically port
  select(-c(county_order, latlong, lat_dms, long_dms, sample_type, area)) %>% 
  # Arrange
  select(sample_id, year, month, date, 
         state, county, port, site, block_id, depth_fathoms,
         lat_dd, long_dd,
         comm_name, sci_name, tissue, sample_n, 
         domoic_ppm, domoic_mod, 
         comments,
         # meat_ppm, viscera_ppm, meat_div_visc, 
         everything()) %>% 
  # Make unique
  unique() %>% 
  # Arrange
  arrange(date)

# Inspect
str(data)
freeR::complete(data)

# Sample id unique?
freeR::which_duplicated(data$sample_id)

# Location
table(data$county)
table(data$site)
table(data$port)
table(data$depth_fathoms)

# Sample
table(data$comm_name)
table(data$tissue)
table(data$domoic_mod)

# Export data
saveRDS(data, file=file.path(outdir, "CA_2021_2022_biotoxin_data.Rds"))

