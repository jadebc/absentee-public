##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2016

# Parameter 3: Association between school-level 
# vaccination coverage and absentee rates among 
# Shoo the Flu schools in each year of the program

# plot results
##############################################
rm(list =ls())
# load base scripts, define directories, load libraries
source(here::here("0-config.R"))

# load underlying data
flu.cov = readRDS(absentee_vaccination_coverage_p3_path)
flu.cov = flu.cov[,c("schoolyr", "school", "participation",
                     "absent_all", "absent_ill")]

# load superlearner fits
results_ill = readRDS(paste0(res_dir, "absentee-p3-ill-results.RDS"))
results_all = readRDS(paste0(res_dir, "absentee-p3-all-results.RDS"))

results_ill = results_ill %>% mutate(y = "absent_ill")
results_all = results_all %>% mutate(y = "absent_all")

results_plot = bind_rows(results_ill, results_all)

results_plot = results_plot %>% mutate(participation = participation * 100)
flu.cov = flu.cov %>% mutate(participation = participation * 100)
  
#----------------------------------------------------
# plot SL predicted mean against Shoo the Flu coverage
#----------------------------------------------------

all_plot = ggplot(results_plot %>% filter(y == "absent_all"), 
       aes(x=participation, y = pred)) +
  geom_point(data = flu.cov, aes(x = participation, y = absent_all), alpha = 0.5) +
  geom_line() +
  facet_grid( ~ schoolyr) +
  scale_x_continuous(breaks = seq(0, 60, 10),
                     labels = seq(0, 60, 10)) + 
  scale_y_continuous(breaks = seq(0, 0.1, 0.01),
                     labels = seq(0, 0.1, 0.01)) + 
  xlab("Percent of students participating in school-located influenza vaccination") +
  ylab("Adjusted school-level mean\nall-cause absences per 100 days") +
  theme_complete_bw() +
  theme(
    strip.text.x = element_text(size=12),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  ggtitle("A) All-cause absences")

ill_plot = ggplot(results_plot %>% filter(y == "absent_ill"), 
       aes(x=participation, y = pred)) +
  geom_point(data = flu.cov, aes(x = participation, y = absent_ill), alpha = 0.5) +
  geom_line() +
  facet_grid( ~ schoolyr) +
  scale_x_continuous(breaks = seq(0, 60, 10),
                     labels = seq(0, 60, 10)) + 
  scale_y_continuous(limits = c(0, 0.07),
                     breaks = seq(0, 0.07, 0.01),
                     labels = seq(0, 0.07, 0.01)) + 
  xlab("Percent of students participating in school-located influenza vaccination") +
  ylab("Adjusted school-level mean\nillness-related absences per 100 days")+
  theme_complete_bw() +
  theme(
    strip.text.x = element_text(size=12),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  ggtitle("B) Illness-specific absences")

both = grid.arrange(all_plot, ill_plot, nrow=2)
ggsave(plot = both,
       filename = paste0(fig_dir, "p3.pdf"),
       width = 14, height = 8)

