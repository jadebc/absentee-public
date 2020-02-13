##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018

# Parameter 2 plot
# glm results, stratified by race
##############################################
# load base scripts, define directories, load libraries
source(here::here("0-config.R"))

#----------------------------------------------------
# Load datasets for p1-plots
#----------------------------------------------------
all.fit.glm.data = readRDS(paste0(res_dir,"5cr_abs_glm_p2_adj_did_all.RDS"))
ill.fit.glm.data = readRDS(paste0(res_dir,"5cr_abs_glm_p2_adj_did_ill.RDS"))

#----------------------------------------------------
# Prep Datasets for plotting
#----------------------------------------------------
all.fit.glm = bind_rows(all.fit.glm.data[1][[1]]) 

all.fit.glm = all.fit.glm%>%
  mutate(race = case_when(
    race == "MultipleEthnicity" ~ "Multiple Ethnicity",
    race == "NativeAmerican" ~ "Native American",
    race == "NotReported" ~ "Not Reported",
    race == "PacificIslander" ~ "Pacific Islander",
    race == "AfricanAmerican" ~ "African American",
    race == "Asian" ~ "Asian American",
    race == "Latino" ~ "Latino",
    race == "White" ~ "White"
  ),
  pt.est = pt.est * 100,
  lb = lb * 100,
  ub = ub * 100) %>%
  mutate(race = factor(race, levels = c(
    "African American",
    "Asian American",
    "Latino",
    "Native American",
    "Pacific Islander",
    "White",
    "Multiple Ethnicity",
    "Not Reported"
  )),
  schoolyr = case_when(
          year == 1 ~ "2014-15",
          year == 2 ~ "2015-16",
          year == 3 ~ "2016-17",
          year == 4 ~ "2017-18"
         )) 

ill.fit.glm = bind_rows(ill.fit.glm.data[1][[1]]) 

ill.fit.glm = ill.fit.glm%>%
  mutate(race = case_when(
    race == "MultipleEthnicity" ~ "Multiple Ethnicity",
    race == "NativeAmerican" ~ "Native American",
    race == "NotReported" ~ "Not Reported",
    race == "PacificIslander" ~ "Pacific Islander",
    race == "AfricanAmerican" ~ "African American",
    race == "Asian" ~ "Asian American",
    race == "Latino" ~ "Latino",
    race == "White" ~ "White"
  ),
  pt.est = pt.est * 100,
  lb = lb * 100,
  ub = ub * 100) %>%
  mutate(race = factor(race, levels = c(
    "African American",
    "Asian American",
    "Latino",
    "Native American",
    "Pacific Islander",
    "White",
    "Multiple Ethnicity",
    "Not Reported"
  )),
         schoolyr = case_when(
           year == 1 ~ "2014-15",
           year == 2 ~ "2015-16",
           year == 3 ~ "2016-17",
           year == 4 ~ "2017-18"
         )) 

#----------------------------------------------------
# Set shared plot parameters
#----------------------------------------------------
std_legend_title = "Race"
yaxis_lab       = "Mean difference in differences in\nabsences per 100 school days\n(95% CI)" 
pdwidth = 0.6

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = 7
cols = c(gg_color_hue(n), "#898888")


#----------------------------------------------------
# all-cause absence plot
#----------------------------------------------------
all_plot = ggplot(all.fit.glm, aes(x=schoolyr, y=pt.est)) +
    geom_point(aes(col = race), position=position_dodge(width=pdwidth),size=2.5) +
    
    geom_errorbar(aes(ymin=lb, ymax=ub,col = race),
                  position=position_dodge(width=pdwidth),
                  width=0.1) +

    scale_y_continuous(limits=c(-6, 8), 
                     breaks=seq(-6, 8, 1), 
                     labels=seq(-6, 8, 1)) +
  
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
  geom_point(aes(col = race), position=position_dodge(width=pdwidth),size=2.5) +
  
  geom_errorbar(aes(ymin=lb, ymax=ub,col = race),
                position=position_dodge(width=pdwidth),
                width=0.1) +
  
  scale_y_continuous(limits=c(-5, 5), 
                     breaks=seq(-5, 5, 1), 
                     labels=seq(-5, 5, 1)) +
  
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

#----------------------------------------------------
# Plot both types of absence
#----------------------------------------------------
both_plot = grid.arrange(all_plot, ill_plot, nrow=2, heights = c(0.4, 0.5))

ggsave(
  both_plot,
  file = paste0(fig_dir, "fig-p2-glm-adj-race.pdf"),
  width = 8,
  height = 8
)

