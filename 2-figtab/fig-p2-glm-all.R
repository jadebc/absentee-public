##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018

# Parameter 2 plot
# all results
##############################################
rm(list=ls())
# load base scripts, define directories, load libraries
source(here::here("0-config.R"))

#----------------------------------------------------
# Load datasets for p1-plots
#----------------------------------------------------

# load unadjusted results
ill.did.fit=readRDS(paste0(res_dir,"5a_abs_glm_p2_did_ill.RDS"))
all.did.fit=readRDS(paste0(res_dir,"5a_abs_glm_p2_did_all.RDS"))
ill.did.fit.cdc=readRDS(paste0(res_dir,"5as_abs_glm_p2_did_ill_cdc.RDS")) 
all.did.fit.cdc=readRDS(paste0(res_dir,"5as_abs_glm_p2_did_all_cdc.RDS")) 
ill.did.fit.cdph2=readRDS(paste0(res_dir,"5ac_abs_glm_p2_unadj_did_ill_cdph2.RDS")) 
all.did.fit.cdph2=readRDS(paste0(res_dir,"5ac_abs_glm_p2_unadj_did_all_cdph2.RDS")) 
ill.did.fit.cdph3=readRDS(paste0(res_dir,"5ac_abs_glm_p2_unadj_did_ill_cdph3.RDS")) 
all.did.fit.cdph3=readRDS(paste0(res_dir,"5ac_abs_glm_p2_unadj_did_all_cdph3.RDS")) 

# load adjusted results
ill.adj.did.fit=readRDS(paste0(res_dir,"5c_abs_glm_p2_adj_did_ill.RDS"))
all.adj.did.fit=readRDS(paste0(res_dir,"5c_abs_glm_p2_adj_did_all.RDS"))
ill.adj.did.fit.cdc=readRDS(paste0(res_dir,"5cs_abs_glm_p2_adj_did_ill_cdc.RDS"))
all.adj.did.fit.cdc=readRDS(paste0(res_dir,"5cs_abs_glm_p2_adj_did_all_cdc.RDS"))
ill.adj.did.fit.cdph2=readRDS(paste0(res_dir,"5cc_abs_glm_p2_adj_did_ill_cdph2.RDS")) 
all.adj.did.fit.cdph2=readRDS(paste0(res_dir,"5cc_abs_glm_p2_adj_did_all_cdph2.RDS")) 
ill.adj.did.fit.cdph3=readRDS(paste0(res_dir,"5cc_abs_glm_p2_adj_did_ill_cdph3.RDS")) 
all.adj.did.fit.cdph3=readRDS(paste0(res_dir,"5cc_abs_glm_p2_adj_did_all_cdph3.RDS")) 


#----------------------------------------------------
# Prep Datasets for plotting
#----------------------------------------------------

# ---

fit.glm.data = rbind(ill.did.fit, all.did.fit, 
                     ill.did.fit.cdc, all.did.fit.cdc,
                     ill.did.fit.cdph2, all.did.fit.cdph2,
                     ill.did.fit.cdph3, all.did.fit.cdph3,
                     
                     ill.adj.did.fit, all.adj.did.fit,
                     ill.adj.did.fit.cdc, all.adj.did.fit.cdc,
                     ill.adj.did.fit.cdph2, all.adj.did.fit.cdph2,
                     ill.adj.did.fit.cdph3, all.adj.did.fit.cdph3)

fit.glm.data$analysis = 
  c(
    c(
    rep("Primary",2*nrow(ill.did.fit)),
    rep("CDC flu season definition", 2*nrow(ill.did.fit.cdc)),
    rep("CDPH cutoff 2", 2*nrow(ill.did.fit.cdph2)),
    rep("CDPH cutoff 3", 2*nrow(ill.did.fit.cdph3))
  ),
    c(
    rep("Primary",2*nrow(ill.adj.did.fit)),
    rep("CDC flu season definition", 2*nrow(ill.adj.did.fit.cdc)),
    rep("CDPH cutoff 2", 2*nrow(ill.adj.did.fit.cdph2)),
    rep("CDPH cutoff 3", 2*nrow(ill.adj.did.fit.cdph3))
  )
  )

fit.glm.data$model = 
  c(
    rep("Unadjusted",8*nrow(ill.did.fit)),
    rep("Adjusted", 8*nrow(ill.did.fit.cdc))
  )

fit.glm.data = fit.glm.data %>%
  mutate(pt.est = pt.est * 100,
         lb = lb * 100,
         ub = ub * 100,
         school_year= case_when(
            year == 1 ~ "2014-15",
            year == 2 ~ "2015-16",
            year == 3 ~ "2016-17",
            year == 4 ~ "2017-18"
          )
         )

#----------------------------------------------------
# Plot the combined, prepped datasets 
#----------------------------------------------------

std_years        = c("2014-15", "2015-16", "2016-17", "2017-18")
# std_legend_label = c("Unadjusted","Adjusted")
std_legend_title = "Analysis"
std_facet_label  = c(absent_ill = "Absence due to illness", absent_all = "All-cause absence")
std_limits       = c(-1.3, 0.75)
std_breaks       = seq(-1.25, 0.75, 0.25)
std_width        = 9
std_height       = 4
yaxis_lab     = "Mean difference in differences in\nabsences per 100 school days\n(95% CI)" 


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# cols             = c("black","#009E73","#56B4E9","#E69F00", "pink", "yellow")
cols             = gg_color_hue(4)
shapes           = c(21:24)

# set factor levels
fit.glm.data = fit.glm.data %>%
  mutate(analysis=factor(analysis, levels=c("Primary", 
                                            "CDC flu season definition", 
                                            "Peak week of flu season", 
                                            "CDPH cutoff 2",
                                            "CDPH cutoff 3",
                                            "Negative control")))

fit.glm.data = fit.glm.data %>%
  mutate(model=factor(model, levels=c("Unadjusted", "Adjusted")))

#----------------------------------------------------
# all-cause absence plot
#----------------------------------------------------
all_plot = ggplot(fit.glm.data %>% filter(Outcome=="absent_all"), aes(x=school_year, y=pt.est, group=analysis)) +
  
  geom_point(aes(col=analysis,shape=analysis, fill = analysis),
             position=position_dodge(width=0.5),size=2.5) +
  
  geom_linerange(aes(ymin=lb, ymax=ub, col=analysis),
                 position=position_dodge(width=0.5)) +
  
  scale_y_continuous(limits=std_limits, 
                     breaks=std_breaks, 
                     labels=std_breaks) +
  
  scale_color_manual(std_legend_title,values=cols) +
  scale_fill_manual(std_legend_title,values=cols) +
  scale_shape_manual(std_legend_title,values=shapes) +
  
  facet_wrap(~ model, labeller = labeller(Outcome=std_facet_label)) +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab(yaxis_lab) +
  xlab("Program year") +
  ggtitle("A) All-cause absences") + 
  theme_complete_bw() +
  theme(legend.position = "None",
        strip.text.x = element_text(size=13))

all_plot

#----------------------------------------------------
# illness-specific absence plot
#----------------------------------------------------
ill_plot = ggplot(fit.glm.data %>% filter(Outcome=="absent_ill"), aes(x=school_year, y=pt.est, group=analysis)) +
  
  geom_point(aes(col=analysis,shape=analysis, fill = analysis),
             position=position_dodge(width=0.5),size=2.5) +
  
  geom_linerange(aes(ymin=lb, ymax=ub, col=analysis),
                 position=position_dodge(width=0.5)) +
  
  scale_y_continuous(limits=std_limits, 
                     breaks=std_breaks, 
                     labels=std_breaks) +
  
  scale_color_manual(std_legend_title,values=cols) +
  scale_fill_manual(std_legend_title,values=cols) +
  scale_shape_manual(std_legend_title,values=shapes) +
  
  facet_wrap(~ model, labeller = labeller(Outcome=std_facet_label)) +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab(yaxis_lab) +
  xlab("Program year") +
  ggtitle("B) Illness-specific absences") + 
  theme_complete_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size=13))

ill_plot

#----------------------------------------------------
# Plot both types of absence
#----------------------------------------------------
both_plot = grid.arrange(all_plot, ill_plot, nrow=2, heights = c(0.4, 0.5))

ggsave(
  both_plot,
  file = paste0(fig_dir, "fig-p2-glm-all.pdf"),
  width = 10,
  height = 8
)



