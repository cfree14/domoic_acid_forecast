
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/moore/processed"
outdir <- "data/da_samples/data"

# Read old data
data_old <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/merged/processed/CA_OR_WA_da_sampling_data.Rds")

# Read new data
or2 <- readRDS(file.path(indir, "OR_2021_2022_biotoxin_data.Rds"))
wa2 <- readRDS(file.path(indir, "WA_2021_2022_biotoxin_data.Rds"))
ca2 <- readRDS(file.path(indir, "CA_2021_2022_biotoxin_data.Rds"))


# Merge data
################################################################################


