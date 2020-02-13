##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018

# Parameter 2 plot
# adjusted glm results
##############################################
# load base scripts, define directories, load libraries
source(here::here("0-config.R"))

#----------------------------------------------------
# Load datasets for p1-plots
#----------------------------------------------------

# load adjusted results
ill.adj.did.fit=readRDS(paste0(res_dir,"5c_abs_glm_p2_adj_did_ill.RDS"))
all.adj.did.fit=readRDS(paste0(res_dir,"5c_abs_glm_p2_adj_did_all.RDS"))

# load total absence results
totals = readRDS(absentee_flu_abs_total)

totals = totals %>% mutate(
  Outcome = case_when(
    Outcome == "absent_all" ~ "All-cause absences",
    Outcome == "absent_ill" ~ "Illness-specific absences"
  )
) %>%
  filter(Analysis == "Flu season")


#----------------------------------------------------
# Prep Datasets for plotting
#----------------------------------------------------
fit.glm.data = bind_rows(ill.adj.did.fit,
                         all.adj.did.fit)

years = length(unique(fit.glm.data$year))

fit.glm.data = fit.glm.data %>%
  mutate(
    school_year = case_when(
      year == 1 ~ "2014-15",
      year == 2 ~ "2015-16",
      year == 3 ~ "2016-17",
      year == 4 ~ "2017-18"
    ),
    
    pt.est = pt.est * 100,
    lb = lb * 100,
    ub = ub * 100,
    
    Outcome = case_when(
      Outcome == "absent_ill" ~ "Illness-specific absences",
      Outcome == "absent_all" ~ "All-cause absences"
    )
  ) 

#----------------------------------------------------
# Plot the combined, prepped datasets 
#----------------------------------------------------
cols             = c("#103E5C","#2185c5")
shapes           = c(16,17)

did_plot = ggplot(fit.glm.data, aes(x=school_year, y=pt.est)) +
  
  geom_hline(yintercept=0, col = "#7E7E7E") +
  geom_point(position=position_dodge(width=0.3),size=2.5) +
  
  geom_linerange(aes(ymin=lb, ymax=ub),
                 position=position_dodge(width=0.3)) +

  scale_y_continuous(limits = c(-1.5, 0.7),
                     breaks = seq(-1.5, 0.7, 0.25),
                     labels = seq(-1.5, 0.7, 0.25)) +
  
  # scale_y_continuous(limits = c(-1.5, 0.7),
  #                    breaks = seq(-1.5, 0.7, 0.5),
  #                    labels = seq(-1.5, 0.7, 0.5),
  #                    sec.axis = sec_axis(~.*totals$enrolled * totals$n_studentdays_fluseas, 
  #                                        name = "Difference-in-difference in\ntotal hospitalization",
  #                                        labels = seq(from=-13500, to=3300, by=1000),
  #                                        breaks = seq(from=-13500, to=3300, by=1000))) +
  
  facet_wrap(~Outcome) +
  ylab("Mean differences-in-differences\nin absences per 100 school days\n\n") +
  xlab("Season") +
  theme_complete_bw() +
  theme(
    strip.text.x = element_text(size = 14),
    legend.position = "bottom"
  ) +
  ggtitle("A) Difference-in-differences in mean absences")


#----------------------------------------------------
# Total absence difference
#----------------------------------------------------

total_plot = ggplot(totals, aes(x=schoolyr, y=total_fewer_abs)) +
  
  geom_hline(yintercept=0, col = "#7E7E7E") +
  geom_point(col = cols[1], shape = shapes[1],
             position=position_dodge(width=0.3),size=2.5) +
  
  geom_linerange(aes(ymin=total_fewer_abs_lb, ymax=total_fewer_abs_ub),
                 position=position_dodge(width=0.3)) +
  
  scale_color_manual("", values=cols[1]) +
  scale_shape_manual("", values=shapes[1]) +
  
  scale_y_continuous(limits = c(-14000, 6400), 
                     breaks = seq(-14000, 6400, 2000),
                     labels = seq(-14000, 6400, 2000)) + 
  
  facet_wrap(~Outcome) +
  ylab("Difference in absences during flu season\n(Intervention vs. comparison district)\n") +
  xlab("Season") +
  theme_complete_bw() +
  theme(
    strip.text.x = element_text(size = 14),
    legend.position = "bottom"
  )+
  ggtitle("B) Differences-in-differences in total absences")

both_plot = grid.arrange(did_plot, total_plot, ncol = 1, heights = c(3, 3))

ggsave(filename = paste0(fig_dir,"fig-p2-tot-glm-adj.pdf"), 
       plot = both_plot,
       width=8, height=8)
