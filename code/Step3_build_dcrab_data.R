
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/moore/processed"
outdir <- "data/da_samples/data"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_enhanced.Rds"))


# Format data
################################################################################

# WHat day of the year is July 15
lubridate::yday("2020-07-15")
lubridate::yday("2020-08-14")
lubridate::yday("2020-09-15")

# Format data
data <- data_orig %>% 
  # Filter
  filter(comm_name=="Dungeness crab" & !is.na(domoic_ppm)) %>% 
  # Assume that mising tissue types are viscera
  mutate(domoic_tissue=ifelse(is.na(domoic_tissue), "viscera", domoic_tissue)) %>% 
  # Add state_abbreviation
  mutate(state_abbrev=recode(state, 
                             "California"="CA",
                             "Oregon"="OR",
                             "Washington"="WA")) %>% 
  # Add survey id
  mutate(survey_id=paste(date, state_abbrev, zone, site, sep="-")) %>% 
  # Add season 
  mutate(yday=lubridate::yday(date),
         season=ifelse( (state_abbrev=="CA" & yday >= 197) | 
                          (state_abbrev=="OR" & yday >= 227) | 
                          (state_abbrev=="WA" & yday >= 259), 
                       year, year-1)) %>% 
  # Simplify
  select(-c(psp_id:dsp_ug100g, domoic_n, yday)) %>% 
  # Arrange
  select(state, state_abbrev, organization:sci_name, season, year:long_dd, survey_id, everything())

# Inspect
freeR::complete(data)

# Unique ids?
freeR::which_duplicated(data$domoic_id)

# Inspect more
table(data$state)
table(data$comm_name)
table(data$sci_name)
table(data$year)
table(data$month)
table(data$zone)
table(data$domoic_tissue)
table(data$notes)

# Site key
site_key <- data %>% 
  count(state, zone, block_id, site)


# Check season
################################################################################

# Plot
g <- ggplot(data, aes(x=date, y=lat_dd, color=as.character(season))) +
  geom_point() +
  # Labels
  labs(x="Sample date", y="Latitude (°N)") +
  # Date
  scale_y_continuous(lim=c(32,50), breaks=seq(32, 50, 2)) +
  scale_x_date(limits=c("2000-01-01", "2023-01-01") %>% ymd()) +
  # Season
  scale_color_manual(name="Season", 
                    values=rainbow(n_distinct(data$year)+1) %>% sample()) +
  # Theme
  theme_bw()
g


# Survey results
################################################################################

# Survey results
results <- data %>% 
  # Reduce to viscera only
  filter(domoic_tissue=="viscera") %>% 
  # Summarize
  group_by(state, state_abbrev, organization, season, year, month, date, zone, block_id, site, survey_id) %>% 
  summarize(lat_dd=mean(lat_dd),
            long_dd=mean(long_dd),
            n=n(),
            nover=sum(domoic_ppm>=30),
            pover=nover/n,
            domoic_ppm_max=max(domoic_ppm),
            domoic_ppm_avg=mean(domoic_ppm),
            domoic_ppm_med=median(domoic_ppm)) %>% 
  ungroup()
  
# Unique ids?
freeR::which_duplicated(results$survey_id)

# Plot histogram of survey results
g <- ggplot(results, aes(x=n)) +
  geom_histogram(binwidth=1) +
  # Labels
  labs(x="# of samples in a survey", y="Number of surveys") +
  scale_x_continuous(breaks=seq(0,66,6)) +
  scale_y_continuous(trans="log10", breaks=c(1, 5,10, 20, 50, 100, 200, 500, 1000)) +
  # Theme
  theme_bw()
g


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_dcrab.Rds"))
saveRDS(results, file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_dcrab_surveys.Rds"))
write.csv(data, file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_dcrab.csv"), row.names = F)
write.csv(results, file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_dcrab_surveys.csv"), row.names = F)

