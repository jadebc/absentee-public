
################################################################################ 
# Plots for bias analysis
################################################################################ 

rm(list=ls())
# Load base scripts, define directories, load libraries
source(here::here("0-config.R"))

ill = readRDS(paste0(res_dir, "bias_analysis_ill.RDS"))
all = readRDS(paste0(res_dir, "bias_analysis_all.RDS"))

reps = 1000

midblue = "#4296E5"

#----------------------------------------
# Illness-specific absences
#----------------------------------------
# truncate the data by dropping the data
# outside the 1st and 99th percentiles
ill.y1.bound = quantile(ill$estimate[ill$year=="2014-15"], probs = c(0.01, 0.99))
ill.y2.bound = quantile(ill$estimate[ill$year=="2015-16"], probs = c(0.01, 0.99))
ill.y3.bound = quantile(ill$estimate[ill$year=="2016-17"], probs = c(0.01, 0.99))
ill.y4.bound = quantile(ill$estimate[ill$year=="2017-18"], probs = c(0.01, 0.99))

ill_plot_df = ill %>%
  filter(
    (year == "2014-15" & estimate > ill.y1.bound[1] & estimate <ill.y1.bound[2]) |
    (year == "2015-16" & estimate > ill.y2.bound[1] & estimate <ill.y2.bound[2]) |
    (year == "2016-17" & estimate > ill.y3.bound[1] & estimate <ill.y3.bound[2]) |
    (year == "2017-18" & estimate > ill.y4.bound[1] & estimate <ill.y4.bound[2]) 
  )

ill_plot = ggplot(ill_plot_df, aes(x=estimate)) + 
  geom_histogram(bins=100, fill="#9F9F9F") + 
  
  facet_wrap(~year, scales = "free", ncol=4, nrow=1) +
  theme_bw() +
  
  # plot median 
  geom_point(aes(x = as.double(median), group = year, y = 0), 
             shape = 21, fill = midblue, size = 3, stroke = 0) +
  
  # # plot 2.5th and 97.5th percentiles
  geom_point(aes(x = as.double(lb), group = year, y = 0), 
             shape = 23, fill = midblue, size = 3, stroke = 0) +
  geom_point(aes(x = as.double(ub), group = year, y = 0), 
             shape = 23, fill = midblue, size = 3, stroke = 0) +
  
  # plot estimate from regression model 
  geom_point(aes(x = as.double(obs_est), group = year, y = 0), 
             shape = 21,  size = 3, stroke = 0.25) +
  
  # plot null 
  geom_vline(aes(xintercept = 0), linetype = "dotted") + 
  
  xlab("Bias-corrected difference-in-difference estimate") + 
  ylab("Number of estimates") +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 10)
  )

ggsave(plot = ill_plot, 
       filename = paste0(fig_dir, "fig_bias_analysis_ill.png"),
       width = 11,
       height = 3)

#----------------------------------------
# All-cause absences
#----------------------------------------
# truncate the data by dropping the data
# outside the 1st and 99th percentiles
all.y1.bound = quantile(all$estimate[all$year=="2014-15"], probs = c(0.01, 0.99))
all.y2.bound = quantile(all$estimate[all$year=="2015-16"], probs = c(0.01, 0.99))
all.y3.bound = quantile(all$estimate[all$year=="2016-17"], probs = c(0.01, 0.99))
all.y4.bound = quantile(all$estimate[all$year=="2017-18"], probs = c(0.01, 0.99))

all_plot_df = all %>%
  filter(
    (year == "2014-15" & estimate > all.y1.bound[1] & estimate <all.y1.bound[2]) |
      (year == "2015-16" & estimate > all.y2.bound[1] & estimate <all.y2.bound[2]) |
      (year == "2016-17" & estimate > all.y3.bound[1] & estimate <all.y3.bound[2]) |
      (year == "2017-18" & estimate > all.y4.bound[1] & estimate <all.y4.bound[2]) 
  )


all_plot = ggplot(all_plot_df, aes(x=estimate)) + 
  geom_histogram(bins=100, fill="#9F9F9F") + 
  
  facet_wrap(~year, scales = "free", ncol=4, nrow=1) +
  theme_bw() +

  # plot median 
  geom_point(aes(x = as.double(median), group = year, y = 0), 
             shape = 21, fill = midblue, size = 3, stroke = 0) +

  # plot 2.5th and 97.5th percentiles
  geom_point(aes(x = as.double(lb), group = year, y = 0), 
             shape = 23, fill = midblue, size = 3, stroke = 0) +
  geom_point(aes(x = as.double(ub), group = year, y = 0), 
             shape = 23, fill = midblue, size = 3, stroke = 0) +
  
  # plot estimate from regression model 
  geom_point(aes(x = as.double(obs_est), group = year, y = 0), 
             shape = 21,  size = 3, stroke = 0.25) +
  
  # plot null 
  geom_vline(aes(xintercept = 0), linetype = "dotted") + 
  
  xlab("Bias-corrected difference-in-difference estimate") + 
  ylab("Number of estimates") +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 10)
  )

ggsave(plot = all_plot, 
       filename = paste0(fig_dir, "fig_bias_analysis_all.png"),
       width = 11,
       height = 3)

#----------------------------------------
# Print range of estimates for caption
#----------------------------------------
ill %>% group_by(year) %>%
  summarise(
    min = min(estimate),
    max = max(estimate)
  )

all %>% group_by(year) %>%
  summarise(
    min = min(estimate),
    max = max(estimate)
  )



