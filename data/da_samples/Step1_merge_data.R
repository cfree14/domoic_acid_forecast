
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/moore/processed"
outdir <- "data/da_samples/data"

# Old directories
base_old <- "/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt"
wadir_old <- file.path(base_old, "data/washington/da_sampling/data")
ordir_old <- file.path(base_old, "data/oregon/processed")
cadir_old <- file.path(base_old, "data/california/da_sampling/data")

# Read old data (already merged)
#data_old <- readRDS("/data/merged/processed/CA_OR_WA_da_sampling_data.Rds")

# Read old data
wa1_orig <- readRDS(file.path(wadir_old, "WA_DOH_2000_2020_biotoxin_sampling_data.Rds"))
or1_orig <- readRDS(file.path(ordir_old, "ODA_2000_2020_da_sampling_data_final.Rds"))
ca1_crab_orig <- readRDS(file.path(cadir_old, "CDPH_2015_2021_crustacean_data.Rds"))
ca1_biv_orig <- readRDS(file.path(cadir_old, "CDPH_2000_2021_other_data.Rds"))

# Read new data
or2_orig <- readRDS(file.path(indir, "OR_2021_2022_biotoxin_data.Rds"))
wa2_orig <- readRDS(file.path(indir, "WA_2021_2022_biotoxin_data.Rds"))
ca2_orig <- readRDS(file.path(indir, "CA_2021_2022_biotoxin_data.Rds"))


# Merge OR data
################################################################################

# Columns
colnames(or1_orig)
colnames(or2_orig)

# Format OR old
or1 <- or1_orig %>% 
  # Rename
  rename(site=location,
         domoic_tissue=type, 
         domoic_ppm=da_ppm, 
         domoic_mod=da_oper,
         domoic_id=sample_id) %>% 
  # Add columns
  mutate(state="Oregon") %>% 
  # Arrange
  select(state, 
         year, month, date, 
         site, lat_dd, long_dd,
         comm_name, sci_name, 
         domoic_id, domoic_tissue, domoic_ppm, domoic_mod, everything()) %>% 
  # Remove select columns
  select(-c(source, time, type_orig))

# Inspect
freeR::complete(or1)

# Format OR new
or2 <- or2_orig %>% 
  # Rename
  rename(domoic_id=sample_id,
         domoic_tissue=tissue) %>% 
  # Add columns
  mutate(state="Oregon",
         psp_id=domoic_id,
         psp_tissue=domoic_tissue) %>% 
  # Arrange
  select(state, 
         year, month, date, 
         area, site, 
         comm_name, sci_name, 
         domoic_id, domoic_tissue, domoic_ppm, 
         psp_id, psp_tissue, psp_ug100g, comments,
         everything())

# Inspect
freeR::complete(or2)

# Merge
or <- bind_rows(or1, or2) %>% 
  # Add organization
  mutate(organization="ODA") %>% 
  # Arrange
  select(state, organization,
         year, month, date, 
         area, site, lat_dd, long_dd,
         comm_name, sci_name, 
         domoic_id, domoic_tissue, domoic_mod, domoic_ppm, 
         psp_id, psp_tissue, psp_ug100g, comments,
         everything()) %>% 
  # Unique
  unique()

# Inspect
freeR::complete(or) # maybe remove area?

# Unique IDs?
freeR::which_duplicated(or$domoic_id) # must be 0


# Merge WA data
################################################################################

# Columns
colnames(wa1_orig)
colnames(wa2_orig)

# Format data
wa1 <- wa1_orig %>% 
  # Rename
  rename(year=sample_year,
         month=sample_month,
         date=sample_date) %>% 
  # Add columns
  mutate(state="Washington") %>% 
  # Fix longitude
  mutate(long_dd=ifelse(long_dd>0, long_dd*-1, long_dd)) %>% 
  # Arrange
  select(state, sample_id,
         year, month, date, 
         organization, 
         county, waterbody, site, subsite, lat_dd, long_dd, 
         comm_name, sci_name,  
         domoic_id, domoic_tissue, domoic_ppm, 
         psp_id, psp_tissue, psp_ug100g,
         dsp_id, dsp_tissue, dsp_ug100g,
         everything()) %>% 
  # Remove select columns
  select(-c(sample_id, organization_id, site_id,
            submit_date, receive_date, domoic_date, psp_date, dsp_date, 
            monitoring_type, sample_type, shell_shucked, fresh_frozen))
  
# Inspect
str(wa1)
freeR::complete(wa1)

# Format data
wa2 <- wa2_orig %>%
  # Rename
  rename(domoic_id=da_id, 
         domoic_tissue=tissue) %>% 
  # Add tissue types
  mutate(psp_tissue=domoic_tissue,
         dsp_tissue=domoic_tissue) %>%
  # Format ids
  mutate(domoic_id=as.character(domoic_id),
         psp_id=as.character(psp_id),
         dsp_id=as.character(dsp_id)) %>% 
  # Arrange
  select(state,
         year, month, date, 
         organization, 
         county, waterbody, site, subsite,
         comm_name, sci_name,  
         domoic_id, domoic_tissue, domoic_ppm, 
         psp_id, psp_tissue, psp_ug100g, 
         dsp_id, dsp_tissue, dsp_ug100g,
         everything())

# Inspect
str(wa2)
freeR::complete(wa2)

# Merge data
wa <- bind_rows(wa1, wa2) %>% 
  # Format organization
  mutate(organization=recode(organization,
                             "Department of Fish & Wildlife"="WDFW",
                             "Department of Health"="WDPH"))

# Inspect
str(wa)
freeR::complete(wa)

# Unique ids?
freeR::which_duplicated(wa$domoic_id)
freeR::which_duplicated(wa$psp_id)
freeR::which_duplicated(wa$dsp_id)

# Inspect more
table(wa$year)
table(wa$month)
sort(unique(wa$organization))

# Location
table(wa$county)
table(wa$waterbody)
sort(unique(wa$site))

# Species key
table(wa$comm_name)
table(wa$sci_name)
spp_key_wa <- wa %>% 
  count(comm_name, sci_name)

# Tissues
table(wa$domoic_tissue)
table(wa$psp_tissue)
table(wa$dsp_tissue)


# Merge CA data
################################################################################

# CA1 - What are the lat/long sources?
# What to do with nindivs and sample type?

# Columns
colnames(ca1_crab_orig)
colnames(ca1_biv_orig)
colnames(ca2_orig)

# Format data
ca1_crab <- ca1_crab_orig %>% 
  # Rename
  rename(sci_name=species,
         site=area,
         depth_fathoms=fathoms,
         domoic_id=sample_id, 
         domoic_ppm=da_ppm,
         domoic_mod=da_prefix) %>% 
  # Add
  mutate(state="California",
         month=lubridate::month(date)) %>% 
  # Arrange
  select(state, 
         year, month, date, 
         port, site,  block_id,  lat_dd, long_dd, depth_fathoms,
         comm_name, sci_name, 
         domoic_id,  domoic_ppm, domoic_mod, 
         everything()) %>% 
  # Remove columns
  select(-c(survey_id, sex, block_lat_dd, block_long_dd, latlong_orig, lat_dd_rep, long_dd_rep)) 

# Inspect
str(ca1_crab)
freeR::complete(ca1_crab)

# Format data
ca1_biv <- ca1_biv_orig %>% 
  # Rename
  rename(sci_name=species,
         domoic_id=sampleid,
         domoic_tissue=tissue,
         domoic_n=nindivs,
         domoic_ppm=da_ppm,
         domoic_mod=da_oper, comments=notes) %>%
  # Add
  mutate(state="California") %>%
  # Arrange
  select(state, 
         year, month, date, 
         county, site, lat_dd, long_dd,
         comm_name, sci_name, 
         domoic_id,  domoic_tissue, domoic_ppm, domoic_mod, comments,
         everything()) %>% 
  # Remove columns
  select(-c(sample_type, type))

# Inspect
str(ca1_biv)
freeR::complete(ca1_biv)


# Format data
ca2 <- ca2_orig %>% 
  # Rename
  rename(domoic_id=sample_id,
         domoic_tissue=tissue,
         domoic_n=sample_n) %>% 
  # Arrange
  select(state, 
         year, month, date, 
         county, port, site, block_id, depth_fathoms, lat_dd, long_dd,
         comm_name, sci_name, 
         domoic_id, domoic_tissue, domoic_n, domoic_ppm, domoic_mod, comments,
         everything()) %>% 
  # Remove ids in these data
  filter(!domoic_id %in% c(ca1_biv$domoic_id, ca1_crab$domoic_id))

# Inspect
freeR::complete(ca2)

# Merge CA data
ca <- bind_rows(ca1_crab, ca1_biv, ca2) %>% 
  # Format columns
  mutate(port=recode(port, 
                     "Ft. Bragg"="Fort Bragg"),
         organization="CDFW") %>% 
  # Make unique
  unique()

# Inspect
str(ca)
freeR::complete(ca)

# IDs unique?
freeR::which_duplicated(ca$domoic_id)

# Inspect more
table(ca$year)
table(ca$month)
table(ca$port)
table(ca$depth_fathoms) # needs work because not all fathoms or constant


# Merge all data
################################################################################

# Merge data
data <- bind_rows(ca, or, wa) %>% 
  # Rename
  rename(notes=comments) %>% 
  # Arrange
  arrange(state, comm_name, date) %>% 
  # Format tissue
  mutate(domoic_tissue=stringr::str_to_lower(domoic_tissue),
         psp_tissue=stringr::str_to_lower(psp_tissue),
         dsp_tissue=stringr::str_to_lower(dsp_tissue),
         domoic_tissue=recode(domoic_tissue, "gut"="viscera"),
         psp_tissue=recode(psp_tissue, "gut"="viscera"),
         dsp_tissue=recode(dsp_tissue, "gut"="viscera"),
         domoic_tissue=ifelse(is.na(domoic_ppm), "none", domoic_tissue),
         psp_tissue=ifelse(is.na(psp_ug100g), "none", psp_tissue),
         dsp_tissue=ifelse(is.na(dsp_ug100g), "none", dsp_tissue)) %>% 
  # Format common names
  mutate(comm_name=recode(comm_name,
                          "Cockle"="Basket cockle",
                          "Bay clam spp."="Bay clam",
                          "Mussel spp."="Mussel",
                          "Clam spp."="Clam",
                          "Spiny lobster"="California spiny lobster",
                          "California (sea) mussel"="California mussel",
                          "Mussel spp. (tsunami debris)"="Mussel (tsunami debris)")) %>% 
  # Format scientific names
  mutate(sci_name=ifelse(sci_name %in% c("Unknown", "Oyster spp.", "Clam spp."), NA, sci_name)) %>% 
  # Arrange
  select(state, organization, comm_name, sci_name,  
         year, month, date,
         site,  lat_dd, long_dd,
         domoic_id, domoic_tissue, domoic_n, domoic_ppm, domoic_mod,  
         psp_id, psp_tissue, psp_ug100g, 
         dsp_id, dsp_tissue, dsp_ug100g, notes,
         everything()) %>% 
  # Remove
  select(-c(county, waterbody, area, port, subsite, block_id,  depth_fathoms))

# Inspect
freeR::complete(data)

# Unique ids?
freeR::which_duplicated(data$domoic_id)
freeR::which_duplicated(data$psp_id)
freeR::which_duplicated(data$dsp_id)

# Species key
spp_key <- data %>% 
  count(comm_name, sci_name)

# Inspect
table(data$state)
table(data$year)
table(data$month)

# Tissue
table(data$domoic_tissue)
table(data$psp_tissue)
table(data$dsp_tissue)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data.Rds"))


