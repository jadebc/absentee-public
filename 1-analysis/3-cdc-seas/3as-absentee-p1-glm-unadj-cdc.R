##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018
# Unadjusted GLM for parameter 1
# cdc flu season definition 
##############################################
# load base scripts, define directories, load libraries
source(here::here("0-config.R"))

################################################################################
# Load all data
################################################################################

absentee_flu_CDC_yr = read_rds(path = absentee_flu_CDC_yr_path)

################################################################################
# Clean data for analysis
################################################################################

# Sorting to ensure glm is fit sequentially
sorted_names = absentee_flu_CDC_yr %>% names() %>% sort()
yearly_data = absentee_flu_CDC_yr[sorted_names]

################################################################################
# Run Analyses
################################################################################

ill.fit       = abs_glm(
  yearly_data = yearly_data,
  outcome     = "absent_ill",
  treatment   = "tr",
  adj         = FALSE,
  did         = FALSE,
  start_year  = 1,
  family      = "gaussian"
)

all.fit       = abs_glm(
  yearly_data = yearly_data,
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
          path = paste0(res_dir, "3as_abs_glm_p1_unadj_ill_cdc.RDS"))

write_rds(x = all.fit,
          path = paste0(res_dir, "3as_abs_glm_p1_unadj_all_cdc.RDS"))
