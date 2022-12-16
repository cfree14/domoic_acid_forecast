
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
         psp_ug=psp_result,
         domoic_ug=domoic_result,
         dsp_ug=dsp_result,
         comments=sample_comments) %>% 
  # Format date
  mutate(date=lubridate::ymd(date),
         year=lubridate::year(date), 
         month=lubridate::month(date)) %>% 
  # Format species
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Format results
  mutate(domoic_ug=ifelse(domoic_ug %in% c("<1", "NTD"), 0, domoic_ug) %>% as.numeric(),
         psp_ug=ifelse(psp_ug %in% c("<38", "NTD"), 0, psp_ug) %>% as.numeric(),
         dsp_ug=ifelse(dsp_ug %in% c("<1", "NTD"), 0, dsp_ug) %>% as.numeric()) %>% 
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
sort(unique(data1$domoic_ug))
sort(unique(data1$psp_ug))
sort(unique(data1$dsp_ug))


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
         domoic_ug=domoic_result) %>% 
  # Format date
  mutate(date=lubridate::ymd(date),
         year=lubridate::year(date), 
         month=lubridate::month(date)) %>% 
  # Format species
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Format results
  mutate(domoic_ug=ifelse(domoic_ug %in% c("<1", "NTD"), 0, domoic_ug) %>% as.numeric()) %>% 
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
sort(unique(data2$domoic_ug))


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
    domoic_ug=domoic_result) %>% 
  # Format date
  mutate(date=lubridate::ymd(date),
         year=lubridate::year(date), 
         month=lubridate::month(date)) %>% 
  # Format species
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Format results
  mutate(domoic_ug=ifelse(domoic_ug %in% c("<1", "NTD"), 0, domoic_ug) %>% as.numeric()) %>% 
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
sort(unique(data3$domoic_ug))


# Merge data
################################################################################

# Merge data
data <- bind_rows(data1, data2, data3) %>% 
  arrange(date)

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
sort(unique(data$domoic_ug))


# Export data
################################################################################

# Save data
saveRDS(data, file=file.path(outdir, "WA_2021_2022_biotoxin_data.Rds"))


