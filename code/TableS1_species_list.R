
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(lubridate)

# Directories
indir <- "data/moore/processed"
outdir <- "data/da_samples/data"
plotdir <- "figures"
tabledir <- "tables"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_enhanced.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Reduce to DA results
  filter(!is.na(domoic_ppm)) %>% 
  # Count by species and state
  count(comm_name, sci_name, state) %>% 
  # Compute total
  spread(key="state", value="n", fill = 0) %>% 
  janitor::clean_names("snake") %>% 
  mutate(total=california+oregon+washington) %>% 
  # Format species
  mutate(species=ifelse(is.na(sci_name), comm_name, paste0(comm_name, " (", sci_name, ")"))) %>% 
  # Arrange
  select(species, total, california, oregon, washington) %>% 
  arrange(desc(total))

52-3

# Export data
################################################################################

# Export data
write.csv(data, file=file.path(tabledir, "TableS1_species_list.csv"), row.names = F)


