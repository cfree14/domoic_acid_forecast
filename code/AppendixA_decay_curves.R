
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
# devtools::install_github("guiastrennec/ggplus")
library(tidyverse)
library(lubridate)
library(ggplus)

# Directories
indir <- "data/moore/processed"
outdir <- "data/da_samples/data"
plotdir <- "figures"
tabledir <- "tables"
outdir <- "output"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CA_OR_WA_2000_2020_biotoxin_data_dcrab_surveys_with_event_id.Rds"))


# Build data
################################################################################

# Numbers of surveys required
nsurveys_req <- 5

# Event info
events <- data_orig %>% 
  # Stats
  group_by(state, event_id) %>% 
  rename(domoic_ppm_max1=domoic_ppm_max) %>% 
  summarize(nsurveys=n(),
            event_day_max=max(event_day),
            domoic_ppm_max=max(domoic_ppm_max1),
            domoic_ppm_max_sd=sd(domoic_ppm_max1)) %>% 
  ungroup() %>% 
  # Filter
  filter(nsurveys >= nsurveys_req & domoic_ppm_max_sd!=0) %>% 
  # Arrange
  arrange(state, desc(domoic_ppm_max)) %>% 
  mutate(event_id=factor(event_id, levels=event_id))

# Build data
data <- data_orig %>% 
  # Reduce to useable event
  filter(event_id %in% events$event_id) %>% 
  # Order events
  mutate(event_id=factor(event_id, levels=levels(events$event_id))) %>% 
  # Arrange
  arrange(event_id, event_day) %>% 
  # Change 0s to 1s
  mutate(domoic_ppm_max=pmax(domoic_ppm_max, 1))


# Fit exponential decay
################################################################################

# Loop through events
i <- 1
for(i in 1:nrow(events)){
  
  # Event info
  print(i)
  event_do <- events$event_id[i]
  event_day_max <- events$event_day_max[i]
  
  # Event data
  sdata <- data %>% 
    filter(event_id==event_do)
  
  # Fit model
  lmfit <- lm(log(domoic_ppm_max)~event_day, sdata)
  summary(lmfit)
  
  # Make predictions
  x <- 0:event_day_max
  preds_list <- predict(lmfit, newdata=tibble(event_day=x), se.fit = T)
  preds <- tibble(event_id=event_do,
                  event_day=x,
                  domoic_ppm_max=exp(preds_list$fit),
                  domoic_ppm_max_lo=exp(preds_list$fit-preds_list$se.fit*1.96),
                  domoic_ppm_max_hi=exp(preds_list$fit+preds_list$se.fit*1.96))
  
  # Extract parameters
  # Y=a*e^(bx)
  # log(Y)=log(a) + Xb
  a <- exp(coef(lmfit)[1])
  b <- coef(lmfit)[2]
  pvalue <- freeR::pval(lmfit)
  r2 <- freeR::r2(lmfit)
  r2_adj <- summary(lmfit)$adj.r.squared
  df_row <- tibble(event_id=event_do,
                   a=a, b=b, pvalue=pvalue, r2=r2, r2_adj=r2_adj)
  
  # Plot fit
  g <- ggplot() +
    # Plot CI
    geom_ribbon(data=preds, mapping=aes(x=event_day, ymin=domoic_ppm_max_lo, ymax=domoic_ppm_max_hi), fill="grey90") +
    # Plot fit
    geom_line(data=preds, mapping=aes(x=event_day, y=domoic_ppm_max), lwd=1.2) +
    # Plot data
    geom_point(data=sdata, mapping=aes(x=event_day, y=domoic_ppm_max), pch=21, color="black", fill="grey40", size=2) +
    # Labels
    labs(x="Event day", y="Max domoic acid (ppm)", title=event_do) +
    # Axes
    lims(y=c(0, NA)) +
    # Theme
    theme_bw()
  g
  
  # Record data
  if(i==1){
    params <- df_row
    preds_out <- preds
  }else{
    params <- bind_rows(params, df_row)
    preds_out <- bind_rows(preds_out, preds)
  }
  
}


# Format output
################################################################################

# Prediction stats (used for plotting)
pred_stats <- preds_out %>% 
  group_by(event_id) %>% 
  summarize(domoic_ppm_max_pred=max(domoic_ppm_max_hi),
            event_day_max=max(event_day)) %>% 
  ungroup()

# Format parameter estimates
params_out <- params %>% 
  # Half-life
  mutate(half_life=log(2)/-b) %>% 
  # Formatted parameters
  mutate(pvalue_rd=round(pvalue, digits=3)) %>% 
  # Plot text
  mutate(plot_text=ifelse(pvalue_rd==0, "p<0.001", paste0("p=", pvalue_rd))) %>% 
  # Add obs / prediction stats for plotting
  left_join(pred_stats %>% select(event_id, event_day_max, domoic_ppm_max_pred), by="event_id") %>% 
  left_join(events %>% select(event_id, domoic_ppm_max), by="event_id") %>% 
  mutate(domoic_ppm_max_plot=pmax(domoic_ppm_max, domoic_ppm_max_pred)) %>% 
  select(-c(domoic_ppm_max, domoic_ppm_max_pred))


#  Export data
################################################################################

# Export data
saveRDS(preds, file=file.path(outdir, "model_predictions.Rds"))
saveRDS(params_out, file=file.path(outdir, "model_fits.Rds"))


# Plot fits in 1 plot
################################################################################

# Plot all
g <- ggplot(preds_out, aes(x=event_day, y=domoic_ppm_max, color=event_id)) +
  geom_line() +
  # Reference line
  geom_hline(yintercept=30, linetype="dotted") +
  # Labels
  labs(x="Event day", y="Max domoic acid (ppm)") +
  # Y-axis
  scale_y_continuous(breaks=seq(0,200,10)) +
  # Legend
  scale_color_discrete(name="Event", guide="none") +
  # Theme
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
g


# Plot fits
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=6),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   legend.position = c(0.8, 0.05),
                   legend.key.size = unit(0.3, "cm"))

# Plot data
g <- ggplot(data, aes(x=event_day, y=domoic_ppm_max)) +
  # Facet (don't facet here if using multi-page plot)
  facet_wrap(~event_id, ncol=4, scales="free") +
  # Fix CIs
  geom_ribbon(data=preds_out, mapping=aes(x=event_day, ymin=domoic_ppm_max_lo, ymax=domoic_ppm_max_hi), fill="grey90", color=NA) +
  # Fit predictions
  geom_line(data=preds_out, mapping=aes(x=event_day, y=domoic_ppm_max), color="black") +
  # Points
  geom_point() +
  # Plot text
  geom_text(data=params_out, mapping=aes(x=event_day_max, y=domoic_ppm_max_plot*0.95, label=plot_text), hjust=1, size=2.4) +
  # Reference line
  # geom_hline(yintercept=30, linetype="dotted") +
  # Labels
  labs(x="Day of event", y="Maximum domoic acid (ppm)") +
  # Axes
  scale_y_continuous(lim=c(0,NA)) +
  # Legend
  scale_color_discrete(name="State", guide="none") +
  # Theme
  theme_bw() + my_theme
#g

ggsave(g, filename=file.path(plotdir, "AppendixA_decay_curves.pdf"),
       width=8.5, height=25, units="in", dpi=600)

# Export plot
# pdf(file.path(plotdir, "AppendixA_decay_curves.pdf"), width=8.5, height=11)
# gg <- ggplus::facet_multiple(plot=g, facets="event_id", ncol = 4, nrow = 6)
# dev.off()

# Export
# ggsave(g, filename=file.path(plotdir, "AppendixA_decay_curves.pdf"), 
#        width=8.5, height=11.5, units="in", dpi=600)


# Dense version

# Plot data
g <- ggplot(data, aes(x=event_day, y=domoic_ppm_max)) +
  # Facet (don't facet here if using multi-page plot)
  facet_wrap(~event_id, ncol=8, scales="free") +
  # Fix CIs
  geom_ribbon(data=preds_out, mapping=aes(x=event_day, ymin=domoic_ppm_max_lo, ymax=domoic_ppm_max_hi), fill="grey90", color=NA) +
  # Fit predictions
  geom_line(data=preds_out, mapping=aes(x=event_day, y=domoic_ppm_max), color="black") +
  # Points
  geom_point(size=0.5) +
  # Plot text
  geom_text(data=params_out, 
            mapping=aes(x=event_day_max, y=domoic_ppm_max_plot*0.9, label=plot_text), 
            hjust=1, size=1.8) +
  # Reference line
  # geom_hline(yintercept=30, linetype="dotted") +
  # Labels
  labs(x="Day of event", y="Maximum domoic acid (ppm)") +
  # Axes
  scale_y_continuous(lim=c(0,NA)) +
  # Legend
  scale_color_discrete(name="State", guide="none") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text=element_text(size=4),
        axis.title=element_text(size=5),
        strip.text=element_text(size=4))
#g

ggsave(g, filename=file.path(plotdir, "AppendixA_decay_curves.png"),
       width=6.5, height=7.5, units="in", dpi=600)



