##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018
# Parameter 2: Difference-in-difference
# Unadjusted GLM for parameter 2 
# CDPH flu season definition (upper bound) 
##############################################
# load base scripts, define directories, load libraries
source(paste0(here::here(), "/0-config.R"))

################################################################################ 
# Load all data
################################################################################ 

absentee_flu_cdph3_unadj_did_prepped = read_rds(path = absentee_flu_cdph3_unadj_did_path)

################################################################################
# Clean data for analysis
################################################################################

# Sorting to ensure glm is fit sequentially
sorted_yearly_names = names(absentee_flu_cdph3_unadj_did_prepped) %>% sort()
yearly_data_sorted = absentee_flu_cdph3_unadj_did_prepped[sorted_yearly_names]

################################################################################
# Run Analyses
################################################################################

ill.fit       = abs_glm(
  yearly_data = yearly_data_sorted,
  outcome     = "absent_ill",
  treatment   = "tr",
  adj         = FALSE,
  did         = TRUE,
  start_year  = 1,
  family      = "gaussian"
)

all.fit       = abs_glm(
  yearly_data = yearly_data_sorted,
  outcome     = "absent_all",
  treatment   = "tr",
  adj         = FALSE,
  did         = TRUE,
  start_year  = 1,
  family      = "gaussian"
)

################################################################################
# Save results of analysis
################################################################################

write_rds(x = ill.fit,
          path = paste0(res_dir, "4a_abs_glm_p2_unadj_did_ill_cdph3.RDS"))

write_rds(x = all.fit,
          path = paste0(res_dir, "4a_abs_glm_p2_unadj_did_all_cdph3.RDS"))
