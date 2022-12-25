
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/moore/processed"
outdir <- "data/da_samples/data"
plotdir <- "figures"
tabledir <- "tables"

# Packages
library(tidyverse)


# Build data and fit model
################################################################################

# Lund 1997 data
# μg = ppm 
data <- tibble(day=c(1,7,14,21),
               n=c(8,6,7,7),
               da_ppm=c(69.5, 43.4, 19, 7.6),
               sd_ppm=c(18.6, 21, 21.8, 16.0),
               se_ppm=c(6.6, 6.6, 8.2, 6.0),
               perc=c(100, 62, 27, 11)) %>% 
  mutate(da_ppm_hi = da_ppm + sd_ppm,
         da_ppm_lo = da_ppm - sd_ppm)

# Fit exponential decay
lmfit <- lm(log(da_ppm) ~ day, data)

# Inspect 
lmfit
summary(lmfit)
b <- coef(lmfit)[2]
pval <- freeR::pval(lmfit)
halflife_days <- log(2) / abs(b)
halflife_hrs <- halflife_days * 24

# Predictions for plotting
x <- seq(1,30,1)
preds_list <- predict(lmfit, newdata = data.frame(day=x), se.fit=T)
preds <- tibble(x=x,
                y=exp(preds_list$fit),
                y_lo=exp(preds_list$fit-preds_list$se.fit*1.96),
                y_hi=exp(preds_list$fit+preds_list$se.fit*1.96))

# Build stats text
pval_text <- paste0("p=", round(pval, 3))
half_life_text <- paste(round(halflife_days, 1), " day half-life")
rate_text <- paste0(round(b*100, 1), "% depuration rate")
stat_text <- paste(pval_text, half_life_text, rate_text, sep="\n")

# Build data and fit model
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=9),
                  plot.title=element_blank(),
                  # Grid lines
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position="none")

# Plot data and fit
g <- ggplot(data, aes(x = day, y = da_ppm)) + 
  # Plot CI
  geom_ribbon(data=preds, mapping=aes(x=x, ymin=y_lo, ymax=y_hi),
              inherit.aes=F, fill="grey85", color=NA, ) +
  # Plot data
  geom_errorbar(data=data, 
                aes(x=day, y=da_ppm, ymin=da_ppm_lo, ymax=da_ppm_hi),
                width=0, color="grey40") +
  geom_point() + 
  # Plot line
  geom_line(preds, mapping=aes(x=x, y=y), inherit.aes = F, color = "black") +
  # Reference line
  geom_hline(yintercept=30, linetype="dotted") +
  annotate("text", x=30, y=32, hjust=1, label="Action threshold", size=2) +
  # Stat text
  annotate(geom="text", x=30, y=87, label=stat_text, size=2.4, hjust=1) +
  # Labels
  labs(x="Day", y="Mean domoic acid concentration (ppm)") +
  scale_x_continuous(breaks=seq(0,30,5)) +
  scale_y_continuous(breaks=seq(0,100,10)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS6_lund_depuration_rate.png"), 
       width=4.5, height=3.5, units="in", dpi=600)





