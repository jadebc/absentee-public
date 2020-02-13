##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018
# Parameter 1: Mean difference in days missed per student per month in 
# matched Shoo the Flu schools vs. comparison schools in each year of the program
# Adjusted GLM for parameter 1
##############################################
# load base scripts, define directories, load libraries
source(here::here("0-config.R"))

################################################################################
# Load all data
################################################################################

absentee_peakwk_adj = read_rds(path = absentee_peakwk_adj_path)

################################################################################
# Clean data for analysis
################################################################################

# Subset to peak week of flu season
yearly_data_pkwk = lapply(absentee_peakwk_adj, function(x) x[peakwk==1])

# Separating data
yearly_data_ill = yearly_data_pkwk[str_which(string = names(absentee_peakwk_adj), pattern = "ill")]
yearly_data_all = yearly_data_pkwk[str_which(string = names(absentee_peakwk_adj), pattern = "all")]

# Sorting to ensure glm is fit sequentially
sorted_ill_names = names(yearly_data_ill) %>% sort()
yearly_data_ill_sorted = yearly_data_ill[sorted_ill_names]

sorted_all_names = names(yearly_data_all) %>% sort()
yearly_data_all_sorted = yearly_data_all[sorted_all_names]

################################################################################
# Run Analyses
################################################################################

ill.adj.fit   = abs_glm(
  yearly_data = yearly_data_ill_sorted,
  outcome     = "absent_ill",
  treatment   = "tr",
  covariates  = c("grade","race"),
  adj         = TRUE,
  did         = FALSE,
  start_year  = 1,
  family      = "gaussian"
)

all.adj.fit   = abs_glm(
  yearly_data = yearly_data_all_sorted,
  outcome     = "absent_all",
  treatment   = "tr",
  covariates  = c("grade","race"),
  adj         = TRUE,
  did         = FALSE,
  start_year  = 1,
  family      = "gaussian"
)

################################################################################
# Save results of analysis
################################################################################

write_rds(x = ill.adj.fit,
          path = paste0(res_dir, "3bp_abs_glm_p1_adj_ill_peakwk.RDS"))

write_rds(x = all.adj.fit,
          path = paste0(res_dir, "3bp_abs_glm_p1_adj_all_peakwk.RDS"))
