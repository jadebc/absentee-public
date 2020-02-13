##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018

# Distributions used in the bias analysis
##############################################
rm(list=ls())

# load base scripts, define directories, load libraries
source(here::here("0-config.R"))

bias_dists = readRDS(paste0(res_dir, "bias_distributions.RDS"))
bias_dists_l = melt(bias_dists)

bias_dists_l = bias_dists_l %>%
  mutate(tr = case_when(
    variable == "sens_dist_all_ousd_1116" ~ "Intervention district (2011-2016)",
    variable == "sens_dist_all_ousd_1718" ~ "Intervention district (2017-2018)",
    variable == "sens_dist_all_wcc" ~ "Comparison district",
    variable == "sens_dist_ill_ousd" ~ "Intervention district",
    variable == "sens_dist_ill_wcc" ~ "Comparison district",
    variable == "Fr_dist_all_ousd" ~ "Intervention district",
    variable == "Fr_dist_all_wcc" ~ "Comparison district",
    variable == "Fr_dist_ill_ousd" ~ "Intervention district",
    variable == "Fr_dist_ill_wcc" ~ "Comparison district"
  ),
  outcome = case_when(
    variable == "sens_dist_all_ousd_1116" ~ "All-cause absences",
    variable == "sens_dist_all_ousd_1718" ~ "All-cause absences",
    variable == "sens_dist_all_wcc" ~ "All-cause absences",
    variable == "sens_dist_ill_ousd" ~ "Illness-specific absences",
    variable == "sens_dist_ill_wcc" ~ "Illness-specific absences",
    variable == "Fr_dist_all_ousd" ~ "All-cause absences",
    variable == "Fr_dist_all_wcc" ~ "All-cause absences",
    variable == "Fr_dist_ill_ousd" ~ "Illness-specific absences",
    variable == "Fr_dist_ill_wcc" ~ "Illness-specific absences"
  ),
  measure = case_when(
    variable == "sens_dist_all_ousd_1116" ~ "Sensitivity",
    variable == "sens_dist_all_ousd_1718" ~ "Sensitivity",
    variable == "sens_dist_all_wcc" ~ "Sensitivity",
    variable == "sens_dist_ill_ousd" ~ "Sensitivity",
    variable == "sens_dist_ill_wcc" ~ "Sensitivity",
    variable == "Fr_dist_all_ousd" ~ "False-positive rate",
    variable == "Fr_dist_all_wcc" ~ "False-positive rate",
    variable == "Fr_dist_ill_ousd" ~ "False-positive rate",
    variable == "Fr_dist_ill_wcc" ~ "False-positive rate"
  )) 

# -------------------------------------
# sensitivity plot - all cause absence
# -------------------------------------
sens_all_plot = ggplot(bias_dists_l %>% 
         filter(measure == "Sensitivity" & outcome == "All-cause absences"), 
       aes(x = value)) +
  geom_histogram(bins = 200) + 
  facet_wrap( ~tr, scale="free_y", nrow=1) +
  theme_bw() +
  xlab("Sensitivity") +
  ylab("Count") + 
  ggtitle("A) Sensitivity distributions for all-cause absences")

# -------------------------------------
# sensitivity plot - illness specific absence
# -------------------------------------
sens_ill_plot = ggplot(bias_dists_l %>% 
         filter(measure == "Sensitivity" & outcome == "Illness-specific absences"), 
       aes(x = value)) +
  geom_histogram(bins = 200) + 
  facet_wrap( ~tr, scale="free_y", nrow=1) +
  theme_bw() +
  xlab("Sensitivity") +
  ylab("Count") + 
  ggtitle("B) Sensitivity distributions for illness-specific absences")

# -------------------------------------
# false positive rate  - all cause absence
# -------------------------------------
fpr_all_plot = ggplot(bias_dists_l %>% filter(measure == "False-positive rate" & outcome == "All-cause absences"), 
       aes(x = value)) + 
  geom_histogram(bins = 200) + 
  facet_wrap( ~tr, scale="free_y", nrow=1) +
  theme_bw() +
  xlab("False positive rate") +
  ylab("Count") + 
  ggtitle("C) False positive rate distributions for all-cause absences")


# -------------------------------------
# false positive rate  - illness absence
# -------------------------------------
fpr_ill_plot = ggplot(bias_dists_l %>% filter(measure == "False-positive rate" & outcome == "Illness-specific absences"), aes(x = value)) + 
  geom_histogram(bins = 200) + 
  facet_wrap( ~tr, scale="free_y", nrow=1) +
  theme_bw() +
  xlab("False positive rate") +
  ylab("Count")+ 
  ggtitle("D) False positive rate distributions for illness-specific absences")



# -------------------------------------
# arrange plots
# -------------------------------------
all_plots = grid.arrange(sens_all_plot, 
                         sens_ill_plot, 
                         fpr_all_plot, 
                         fpr_ill_plot,
                         nrow = 4, ncol = 1)


ggsave(all_plots, filename = paste0(fig_dir, "bias-distributions.pdf"),
       width = 7, height = 10)


