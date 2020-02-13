##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018

# Parameter 2 plot
# glm results, stratified by month
##############################################
# load base scripts, define directories, load libraries
source(here::here("0-config.R"))

#----------------------------------------------------
# Load datasets for p1-plots
#----------------------------------------------------
all.fit.glm.data = readRDS(paste0(res_dir,"5cm_abs_glm_p2_adj_did_all.RDS"))
ill.fit.glm.data = readRDS(paste0(res_dir,"5cm_abs_glm_p2_adj_did_ill.RDS"))
all.fit.glm.negc.data = readRDS(paste0(res_dir,"5cnm_abs_glm_p2_adj_negc_did_all.RDS"))
ill.fit.glm.negc.data = readRDS(paste0(res_dir,"5cnm_abs_glm_p2_adj_negc_did_ill.RDS"))

#----------------------------------------------------
# Prep Datasets for plotting
#----------------------------------------------------

fit.glm.newCols = list(
  list(model="GLM", type="Adjusted")
)

all.fit.glm = prep_for_plot(data=all.fit.glm.data,
                          newCols=fit.glm.newCols,
                          filterCondition="TRUE",
                          stratify_by = "month")

ill.fit.glm = prep_for_plot(data=ill.fit.glm.data,
                            newCols=fit.glm.newCols,
                            filterCondition="TRUE",
                            stratify_by = "month")

all.fit.negc.glm = prep_for_plot(data=all.fit.glm.negc.data,
                            newCols=fit.glm.newCols,
                            filterCondition="TRUE",
                            stratify_by = "month")

ill.fit.negc.glm = prep_for_plot(data=ill.fit.glm.negc.data,
                            newCols=fit.glm.newCols,
                            filterCondition="TRUE",
                            stratify_by = "month")

all.fit.glm = all.fit.glm %>% mutate(season = "flu")
ill.fit.glm = ill.fit.glm %>% mutate(season = "flu")
all.fit.negc.glm = all.fit.negc.glm %>% mutate(season = "nonflu")
ill.fit.negc.glm = ill.fit.negc.glm %>% mutate(season = "nonflu")

#----------------------------------------------------
# Plot the combined, prepped datasets 
#----------------------------------------------------

std_years        = c("2014-15", "2015-16", "2016-17","2017-18")
std_facet_label  = ""
std_legend_label = unique(all.fit.glm$month)
std_legend_title = "Month"
std_limits       = c(-10, 10)
std_breaks       = seq(-10, 10)
std_width        = 12
std_height       = 4
yaxis_lab       = "Mean difference in differences in\nabsences per 100 school days (95% CI)" 
cols             = c("#103E5C","#2185c5", "#38E2E9", "#983ECA", "#E93860",
                     "red","blue","green")
shapes           = c(16, 17, 18, 19, 20, 21, 22, 23)


# All prepped datasets
all_data = bind_rows(
  all.fit.glm, all.fit.negc.glm
)

ill_data = bind_rows(
  ill.fit.glm, ill.fit.negc.glm
)

all_data = all_data %>% mutate(month = factor(month,
                            levels = c(8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7))) %>%
  mutate(year = case_when(
    year == 1 ~ "2014-15",
    year == 2 ~ "2015-16",
    year == 3 ~ "2016-17",
    year == 4 ~ "2017-18"
  ))
ill_data = ill_data %>% mutate(month = factor(month,
                            levels = c(8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7)))%>%
  mutate(year = case_when(
    year == 1 ~ "2014-15",
    year == 2 ~ "2015-16",
    year == 3 ~ "2016-17",
    year == 4 ~ "2017-18"
  ))

# plot title
std_title_prefix = "Mean differences in differences in absences per 100 school days during flu season"
titles_list = paste0(std_title_prefix, 
                     list(
                       ""
                     )
)

# name of figure pdf 
filenames_list = paste0(fig_dir, 
                        list(
                          "fig-p2-glm-adj-month.pdf"
                        )
)

blue = "#4296E5"
orange = "#E56242"

all_plot = ggplot(all_data, aes(x = month, y = rd)) + 
  geom_point(aes(col = season, shape = season), size=2) + 
  geom_errorbar(aes(col = season, ymin = lb, ymax = ub), width=0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_shape_manual("", values = c(19, 21), 
                     labels = c("Flu season", "Non-flu season")) + 
  scale_color_manual("", values = c(orange, blue), 
                     labels = c("Flu season", "Non-flu season")) +
  facet_grid(~year) +
  xlab("Month") + 
  ylab("Mean differences in differences\nin absencesper 100 school days") +
  theme_complete_bw() +
  theme(legend.position = "none") +
  ggtitle("A) All-cause absences")

all_plot

ggsave(
  all_plot,
  file = paste0(fig_dir, "fig-p2-glm-adj-all-month.pdf"),
  width = 10,
  height = 4
)

ill_plot = ggplot(ill_data, aes(x = month, y = rd)) + 
  geom_point(aes(col = season, shape = season), size=2) + 
  geom_errorbar(aes(col = season, ymin = lb, ymax = ub), width=0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_shape_manual("", values = c(19, 21), 
                     labels = c("Flu season", "Non-flu season")) + 
  scale_color_manual("", values = c(orange, blue), 
                     labels = c("Flu season", "Non-flu season")) +
  facet_grid(~year) +
  xlab("Month") + 
  ylab("Mean differences in differences\nin absencesper 100 school days") +
  theme_complete_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=12))+ 
  ggtitle("B) Illness-specific absences")

ill_plot

ggsave(
  ill_plot,
  file = paste0(fig_dir, "fig-p2-glm-adj-ill-month.pdf"),
  width = 10,
  height = 4
)

both_plot = grid.arrange(all_plot, ill_plot, nrow=2, heights = c(2.9,3.3))

ggsave(
  both_plot,
  file = paste0(fig_dir, "fig-p2-glm-adj-month.pdf"),
  width = 10,
  height = 8
)
