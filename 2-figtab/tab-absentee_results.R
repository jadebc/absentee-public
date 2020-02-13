##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2017

# generate table for absentee results
##############################################

source(here::here("0-config.R"))

mn.abs.w = readRDS(paste0(res_dir, 'tables/1-unadj-table-raw.RDS'))$mn.abs.w

#Load data for all absences
absence_rate_all = mn.abs.w[mn.abs.w$abstype == "mn_all", ] 
absence_rate_all = absence_rate_all[, c("schoolyrp", "OUSD", "WCC")]
adj_did_all = readRDS(paste0(res_dir, '5c_abs_glm_p2_adj_did_all.RDS'))
adj_did_all_peak = readRDS(paste0(res_dir, '5cp_abs_glm_p2_adj_did_all_peakwk.RDS'))

#Load data for illness-related absences
absence_rate_ill = mn.abs.w[mn.abs.w$abstype == "mn_ill", ]
absence_rate_ill = absence_rate_ill[, c("schoolyrp", "OUSD", "WCC")]
adj_did_ill = readRDS(paste0(res_dir, '5c_abs_glm_p2_adj_did_ill.RDS'))
adj_did_ill_peak = readRDS(paste0(res_dir, '5cp_abs_glm_p2_adj_did_ill_peakwk.RDS'))

#Define function to generate results table
results_table = function(absence_rate_table, did_season_table, did_peak_table){
  school_year = as.character(absence_rate_table$schoolyrp)
  num_student_days = append("-", format(adj_did_ill$n,big.mark=","))
  program_absence_rate = format(round(absence_rate_table$OUSD, 2), nsmall = 2)
  comparison_absence_rate = format(round(absence_rate_table$WCC, 2), nsmall = 2)
  flu_season_ci = pt.est.ci.table(did_season_table, 2, 100)
  flu_season_peak_ci = pt.est.ci.table(did_peak_table, 2, 100)
  return (cbind(school_year, num_student_days, program_absence_rate, comparison_absence_rate, flu_season_ci, flu_season_peak_ci))
}


results_column_names = c("school_year", "num_student_days", "program_absence_rate", "comparison_absence_rate", "flu_season_ci", "flu_season_peak_ci")

all_results = results_table(absence_rate_all, adj_did_all, adj_did_all_peak)
colnames(all_results) = results_column_names

ill_results = results_table(absence_rate_ill, adj_did_ill, adj_did_ill_peak)
colnames(ill_results) = results_column_names


#Convert and export tables to csv 
write.csv(all_results, paste0(tab_dir, "All-absences-results.csv"), row.names = FALSE)
write.csv(ill_results, paste0(tab_dir, "Illness-related-absences-results.csv"), row.names = FALSE)

