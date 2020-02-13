##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018

# Parameter 2 plot
# glm results, stratified by grade
##############################################
# load base scripts, define directories, load libraries
source(here::here("0-config.R"))

#----------------------------------------------------
# Load datasets for p1-plots
#----------------------------------------------------
all.fit.glm.data = readRDS(paste0(res_dir,"5cg_abs_glm_p2_adj_did_all.RDS"))
ill.fit.glm.data = readRDS(paste0(res_dir,"5cg_abs_glm_p2_adj_did_ill.RDS"))

#----------------------------------------------------
# Prep Datasets for plotting
#----------------------------------------------------
all.fit.glm = bind_rows(all.fit.glm.data[1][[1]]) 

all.fit.glm = all.fit.glm %>%
  mutate(
    pt.est = pt.est * 100,
    lb = lb * 100,
    ub = ub * 100,
  schoolyr = case_when(
    year == 1 ~ "2014-15",
    year == 2 ~ "2015-16",
    year == 3 ~ "2016-17",
    year == 4 ~ "2017-18"
  )) 

ill.fit.glm = bind_rows(ill.fit.glm.data[1][[1]]) 

ill.fit.glm = ill.fit.glm %>%
  mutate(
    pt.est = pt.est * 100,
    lb = lb * 100,
    ub = ub * 100,
    schoolyr = case_when(
      year == 1 ~ "2014-15",
      year == 2 ~ "2015-16",
      year == 3 ~ "2016-17",
      year == 4 ~ "2017-18"
    )) 

#----------------------------------------------------
# Set shared plot parameters
#----------------------------------------------------
std_legend_title = "Grade"
yaxis_lab       = "Mean difference in differences in\nabsences per 100 school days\n(95% CI)" 
pdwidth = 0.5
cols = brewer.pal(n=9, name="Blues")[5:9]


#----------------------------------------------------
# all-cause absence plot
#----------------------------------------------------
all_plot = ggplot(all.fit.glm, aes(x=schoolyr, y=pt.est)) +
  geom_point(aes(col = grade), position=position_dodge(width=pdwidth),size=2.5) +
  
  geom_errorbar(aes(ymin=lb, ymax=ub,col = grade),
                position=position_dodge(width=pdwidth),
                width=0.1) +
  
  scale_y_continuous(limits=c(-1.8, 1.4), 
                     breaks=seq(-1.8, 1.4, 0.2), 
                     labels=seq(-1.8, 1.4, 0.2)) +
  
  scale_color_manual(std_legend_title,values=cols) +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab(yaxis_lab) +
  xlab("Program year") +
  theme_complete_bw() +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        legend.position = "None") +
  ggtitle("A) All-cause absences")

all_plot

#----------------------------------------------------
# illness-specific absence plot
#----------------------------------------------------
ill_plot = ggplot(ill.fit.glm, aes(x=schoolyr, y=pt.est)) +
  geom_point(aes(col = grade), position=position_dodge(width=pdwidth),size=2.5) +
  
  geom_errorbar(aes(ymin=lb, ymax=ub,col = grade),
                position=position_dodge(width=pdwidth),
                width=0.1) +
  
  scale_y_continuous(limits=c(-2, 1), 
                     breaks=seq(-2, 1, 0.2), 
                     labels=seq(-2, 1, 0.2)) +
  
  scale_color_manual(std_legend_title,values=cols) +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab(yaxis_lab) +
  xlab("Program year") +
  theme_complete_bw() +
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom") +
  ggtitle("B) Illness-specific absences")

ill_plot

both_plot = grid.arrange(all_plot, ill_plot, nrow=2, heights = c(0.4, 0.45))

ggsave(
  both_plot,
  file = paste0(fig_dir, "fig-p2-glm-adj-grade.pdf"),
  width = 8,
  height = 8
)

