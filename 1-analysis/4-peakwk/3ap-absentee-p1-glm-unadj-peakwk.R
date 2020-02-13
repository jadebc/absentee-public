##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018
# Parameter 1: Mean difference in days missed per student per month in 
# matched Shoo the Flu schools vs. comparison schools in each year of the program
# Unadjusted GLM for parameter 1
##############################################
# load base scripts, define directories, load libraries
source(here::here("0-config.R"))

################################################################################
# Load all data
################################################################################

absentee_flu_yr = read_rds(path = absentee_flu_yr_path)

################################################################################
# Clean data for analysis
################################################################################

# Sorting to ensure glm is fit sequentially
sorted_names = absentee_flu_yr %>% names() %>% sort()
yearly_data = absentee_flu_yr[sorted_names]

# Subset to peak week of flu season
yearly_data_pkwk = lapply(yearly_data , function(x) 
  x %>% filter(fluseasCDPH_2.5==1))

################################################################################
# Run Analyses
################################################################################

ill.fit       = abs_glm(
  yearly_data = yearly_data_pkwk,
  outcome     = "absent_ill",
  treatment   = "tr",
  adj         = FALSE,
  did         = FALSE,
  start_year  = 1,
  family      = "gaussian"
)

all.fit       = abs_glm(
  yearly_data = yearly_data_pkwk,
  outcome     = "absent_all",
  treatment   = "tr",
  adj         = FALSE,
  did         = FALSE,
  start_year  = 1,
  family      = "gaussian"
)

################################################################################
# Save results of analysis
################################################################################

write_rds(x = ill.fit,
          path = paste0(res_dir, "3ap_abs_glm_p1_unadj_ill_peakwk.RDS"))

write_rds(x = all.fit,
          path = paste0(res_dir, "3ap_abs_glm_p1_unadj_all_peakwk.RDS"))
