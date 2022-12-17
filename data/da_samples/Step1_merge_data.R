
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
ca1_bic_orig <- readRDS(file.path(cadir_old, "CDPH_2000_2021_other_data.Rds"))

# Read new data
or2_orig <- readRDS(file.path(indir, "OR_2021_2022_biotoxin_data.Rds"))
wa2_orig <- readRDS(file.path(indir, "WA_2021_2022_biotoxin_data.Rds"))
ca2_orig <- readRDS(file.path(indir, "CA_2021_2022_biotoxin_data.Rds"))


# Merge OR data
################################################################################

# To-do
# Can you assign a DCrab area to all points?
# Can you get lat/long for the recent data?

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
freeR::complete(or1)

# Format OR new
# Missing these columns: lat_dd, long_dd, domoic_mod
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
         everything()) %>% 
  # Remove select columns
  select(-c(dcrab_area, shellfish_type))

# Merge
or <- bind_rows(or1, or2)

# Inspect
freeR::complete(or)


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
wa <- bind_rows(wa1, wa2)

# Inspect
str(wa)
freeR::complete(wa)

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


# Merge all data
################################################################################

# Merge data
data <- bind_rows(wa, or) %>% 
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
                          "Bay clam spp."="Bay clam",
                          "California (sea) mussel"="California mussel",
                          "Mussel spp. (tsunami debris)"="Mussel (tsunami debris)")) %>% 
  # Format scientific names
  mutate(sci_name=ifelse(sci_name %in% c("Unknown", "Oyster spp."), NA, sci_name)) %>% 
  # Arrange
  select(state, comm_name, sci_name,  
         year, month, date,
         waterbody, area, site, subsite, lat_dd, long_dd, 
         domoic_id, domoic_tissue, domoic_ppm, domoic_mod,  
         psp_id, psp_tissue, psp_ug100g, 
         dsp_id, dsp_tissue, dsp_ug100g,
         everything()) %>% 
  # Remove
  select(-c(organization, county))

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


