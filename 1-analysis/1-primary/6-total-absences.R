##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018
# Total fewer illness absences in OUSD during flu season 
# compared to WCCUSD
##############################################
# Load base scripts, define directories, load libraries
source(here::here("0-config.R"))

################################################################################ 
# Load DID estimates
################################################################################ 

res_ill = readRDS(paste0(res_dir, "5c_abs_glm_p2_adj_did_ill.RDS"))
res_all = readRDS(paste0(res_dir, "5c_abs_glm_p2_adj_did_all.RDS"))

res_peak_ill = readRDS(paste0(res_dir,"5ap_abs_glm_p2_did_ill_peakwk.RDS")) 
res_peak_all = readRDS(paste0(res_dir,"5ap_abs_glm_p2_did_all_peakwk.RDS")) 

res_main = bind_rows(res_ill, res_all) %>% mutate(Analysis = "Flu season")
res_peak = bind_rows(res_peak_ill, res_peak_all) %>% mutate(Analysis = "Peak week of flu season")

res = bind_rows(res_main, res_peak)

################################################################################ 
# Get number of flu season days in school per school year
################################################################################ 

absentee_flu = read_rds(path = absentee_flu_path)

flu_seas_length = absentee_flu %>% 
  group_by(schoolyr) %>%
  distinct(schoolyr, date) %>%
  summarise(n_studentdays_fluseas = n()) %>%
  mutate(year = case_when(
    schoolyr == "2014-15" ~ 1,
    schoolyr == "2015-16" ~ 2,
    schoolyr == "2016-17" ~ 3,
    schoolyr == "2017-18" ~ 4,
    TRUE ~ 0
  )) %>%
    filter(year > 0)
  
peak_flu_seas_length = absentee_flu %>% 
  filter(peakwk_2.5 == 1) %>%
  group_by(schoolyr) %>%
  distinct(schoolyr, date) %>%
  summarise(n_studentdays_peakseas = n()) %>%
  mutate(year = case_when(
    schoolyr == "2014-15" ~ 1,
    schoolyr == "2015-16" ~ 2,
    schoolyr == "2016-17" ~ 3,
    schoolyr == "2017-18" ~ 4,
    TRUE ~ 0
  )) %>%
  filter(year > 0)

################################################################################ 
# Define enrollment in OUSD K-5 public schools
# https://dashboards.ousd.org/views/Enrollment/Snapshot?iframeSizedToWindow=true&:embed=y&:showAppBanner=false&:display_count=no&:showVizHome=no
################################################################################ 

enr = data.frame(
  year = seq(1,4),
  enrolled = c(20544, 20532, 20013, 19911)
)

seas_length = full_join(flu_seas_length, peak_flu_seas_length, by = c("schoolyr", "year"))

totals = full_join(res, enr, by = "year")
totals = full_join(totals, seas_length, by = "year")

totals = totals %>% mutate(
  total_fewer_abs = pt.est * enrolled * n_studentdays_fluseas,
  total_fewer_abs_lb = lb * enrolled * n_studentdays_fluseas,
  total_fewer_abs_ub = ub * enrolled * n_studentdays_fluseas,
  
  total_peak_fewer_abs = pt.est * enrolled * n_studentdays_peakseas,
  total_peak_fewer_abs_lb = lb * enrolled * n_studentdays_peakseas,
  total_peak_fewer_abs_ub = ub * enrolled * n_studentdays_peakseas
)

saveRDS(totals, file = absentee_flu_abs_total)

