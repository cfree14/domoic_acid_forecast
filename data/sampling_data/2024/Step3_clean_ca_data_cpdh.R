
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
data_orig <- readxl::read_excel(file.path(indir, "CDPH_domoic-acid-data_121824.xlsx"), col_types = "text")

# Read key
key <- readxl::read_excel(file.path(indir, "CDPH_sample_key_empty.xlsx")) %>% 
  select(-c(n, species_tissue))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Clean names
  janitor::clean_names("snake") %>% 
  rename(sample_id=srl_number,
         agency=agency_code,
         date=date_sampled,
         lat_dd=latitude, 
         long_dd=longitude, 
         domoic_ppm=asp_ug_g,
         domoic_ppm_mod=mod_asp, 
         sample_code=shellfish_code,
         species_tissue=sample_type,
         n_indivs=number_of_individuals) %>% 
  # Format date
  mutate(date=as.numeric(date) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.)) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Convert numeric
  mutate(lat_dd=as.numeric(lat_dd),
         long_dd=as.numeric(long_dd),
         domoic_ppm=as.numeric(domoic_ppm),
         n_indivs=as.numeric(n_indivs)) %>% 
  # Correct longitude
  mutate(long_dd=abs(long_dd)*-1) %>% 
  # Format species
  mutate(species=recode(species, 
                        "Cancer magister"="Metacarcinus magister",          
                        "Clinocardium nuttalli"="Clinocardium nuttallii",           
                        "Lampetra tridentata"="Entosphenus tridentatus",            
                        # "Magallana sikamea"="",               
                        "Prototheca staminea"="Leukoma staminea",             
                        "Seriola lalandi dorsalis"="Seriola dorsalis",       
                        "Tapes japonica"="Ruditapes philippinarum",                  
                        "Tresus nuttalli"="Tresus nuttallii")) %>% 
  # Arrange
  select(year, date, sample_id, 
         agency, county, sample_site, lat_dd, long_dd,
         sample_code, species_tissue, species, n_indivs,
         domoic_ppm, domoic_ppm_mod, notes, everything()) %>% 
  arrange(year, date)
  
# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$year)
range(data$date)
table(data$county)
freeR::uniq(data$sample_site)
freeR::uniq(data$sample_id)
table(data$agency)
table(data$domoic_ppm_mod)
freeR::uniq(data$notes)

# Species
table(data$species)
freeR::check_names(data$species)

# Species key
spp_key <- data %>% 
  count(sample_code, species_tissue, species)
write.csv(spp_key, file=file.path(indir, "CDPH_sample_key_empty.csv"), row.names=F)

# Area
table(data$area)
table(data$site)
table(data$dcrab_area)


# Fix taxa/tissue info
################################################################################

# Format data
data1 <- data %>% 
  # Remove
  select(-c(species)) %>%
  rename(sample_long=species_tissue) %>% 
  # Add
  left_join(key, by="sample_code") %>% 
  # Arrange
  select(year, date, sample_id, 
        agency, county, sample_site, lat_dd, long_dd,
        sample_code, sample_long, comm_name, species, tissue, type, n_indivs,
        domoic_ppm, domoic_ppm_mod, notes, everything()) %>% 
  arrange(year, date)

# Inspect key
key1 <- data1 %>% 
  count(sample_code, sample_long, comm_name, species, tissue, type)
freeR::which_duplicated(key1$sample_code)


# Export data
################################################################################

# Export
saveRDS(data1, file=file.path(outdir, "CA_biotoxin_data_cdph.Rds"))


# Plot data
################################################################################

ggplot(data1, aes(y=lat_dd, x=date, size=domoic_ppm, color=comm_name)) +
  geom_point() +
  # Labels
  labs(y="Latitude (°N)", x="Date") +
  # Legend
  scale_color_discrete(guide="none") +
  scale_size_continuous(name="Domoic acid (ppm)") +
  # Theme
  theme_bw()



