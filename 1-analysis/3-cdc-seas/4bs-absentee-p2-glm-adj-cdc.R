##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018
# Parameter 2: Difference-in-difference
# Adjusted GLM for parameter 2 
# cdc flu season definition 
##############################################
# load base scripts, define directories, load libraries
source(here::here("0-config.R"))

################################################################################
# Load all data
################################################################################

absentee_flu_CDC_adj_did = read_rds(path = absentee_flu_CDC_adj_did_path)

################################################################################
# Clean data for analysis
################################################################################

# Separating data
yearly_data_ill = absentee_flu_CDC_adj_did[str_which(string = names(absentee_flu_CDC_adj_did), pattern = "ill")]
yearly_data_all = absentee_flu_CDC_adj_did[str_which(string = names(absentee_flu_CDC_adj_did), pattern = "all")]

# Sorting to ensure glm is fit sequentially
sorted_ill_names = names(yearly_data_ill) %>% sort()
yearly_data_ill_sorted = yearly_data_ill[sorted_ill_names]

sorted_all_names = names(yearly_data_all) %>% sort()
yearly_data_all_sorted = yearly_data_all[sorted_all_names]

################################################################################
# Run Analyses
################################################################################

ill.fit = abs_glm(
  yearly_data   = yearly_data_ill_sorted,
  outcome       = "absent_ill",
  treatment     = "tr",
  covariates  = c("month","grade","race"),
  adj           = TRUE,
  did           = TRUE,
  start_year    = 1,
  family        = "gaussian"
)

all.fit = abs_glm(
  yearly_data   = yearly_data_all_sorted,
  outcome       = "absent_all",
  treatment     = "tr",
  covariates  = c("month","grade","race"),
  adj           = TRUE,
  did           = TRUE,
  start_year    = 1,
  family        = "gaussian"
)

################################################################################
# Save results of analysis
################################################################################

write_rds(x = ill.fit,
          path = paste0(res_dir, "5cs_abs_glm_p2_adj_did_ill_cdc.RDS"))

write_rds(x = all.fit,
          path = paste0(res_dir, "5cs_abs_glm_p2_adj_did_all_cdc.RDS"))
