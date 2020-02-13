##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018
# bias analysis
##############################################
rm(list=ls())
# Load base scripts, define directories, load libraries
source(here::here("0-config.R"))

################################################################################ 
# Load and process data
################################################################################ 

absentee_flu = read_rds(path = absentee_flu_path)

# load estimates from unadjusted did models
p2_unadj_ill = readRDS(paste0(res_dir, "5a_abs_glm_p2_did_ill.RDS"))
p2_unadj_all = readRDS(paste0(res_dir, "5a_abs_glm_p2_did_all.RDS"))

absentee_flu = absentee_flu %>% 
  mutate(schoolyr_cat = case_when(
    schoolyr == "2011-12" ~ "Pre",
    schoolyr == "2012-13" ~ "Pre",
    schoolyr == "2013-14" ~ "Pre",
    schoolyr == "2014-15" ~ "2014-15",
    schoolyr == "2015-16" ~ "2015-16",
    schoolyr == "2016-17" ~ "2016-17",
    schoolyr == "2017-18" ~ "2017-18",
    schoolyr == "2018-19" ~ "2018-19"
  )) %>%
  mutate(schoolyr_cat = factor(schoolyr_cat, 
                               levels = c("Pre", "2014-15", "2015-16", "2016-17", "2017-18")))

absentee_flu = data.table(absentee_flu)

################################################################################ 
# Define distributions of sensitivity & specificity of each district 
################################################################################ 
# sensitivity: all cause absence
sens_dist_all_ousd_1718 = rbeta(n = 100000, shape1 = 14, shape2 = 1)
sens_dist_all_ousd_1116 = rbeta(n = 100000, shape1 = 6, shape2 = 1)
sens_dist_all_wcc = rbeta(n = 100000, shape1 = 6, shape2 = 1)

# sensitivity: illness absence
sens_dist_ill_ousd = rbeta(n = 100000, shape1 = 3, shape2 = 1.5)
sens_dist_ill_wcc = rbeta(n = 100000, shape1 = 3, shape2 = 1.5)

# ------------------------------------------------------------
# false positive rate
# -----------------------------------------------------------
# since the outcome units are in person-time, we use the false-positive
# rate, which is the number of false-positive diagnoses per unit person-time
# (i.e., the number of false-positives per student-day)
# if FPR is 0.01, then every 100 days we would expect one student 
# to have a record that said they were present but they were absent

# ------------------------------------------------------------
# all cause
# -----------------------------------------------------------
# in ousd there are about 19000 K-5 students and 175 school days
# if we assume that no more than 20% of students have a false positive
# per school year, that means the upper bound is: 
# (19000*.2) / (19000*175) = 0.0011

Fr_dist_all_ousd = rbeta(n = 100000, shape1 = 2, shape2 = 19)/300
Fr_dist_all_wcc = rbeta(n = 100000, shape1 = 2, shape2 = 5)/300

# ------------------------------------------------------------
# illness
# -----------------------------------------------------------
# in ousd there are about 19000 K-5 students and 175 school days
# if we assume that no more than 40% of students have a false positive
# per school year, that means the upper bound is: 
# (19000*.4) / (19000*175) = 0.0023

# in wcc there are approx. 16500 students, so:
# (16500*.2) / (16500*175) = 0.0023
Fr_dist_ill_ousd = rbeta(n = 100000, shape1 = 2, shape2 = 5)/300
Fr_dist_ill_wcc = rbeta(n = 100000, shape1 = 1.5, shape2 = 2)/400


bias_dists = data.frame(
  sens_dist_all_ousd_1116 = sens_dist_all_ousd_1116,
  sens_dist_all_ousd_1718 = sens_dist_all_ousd_1718,
  sens_dist_all_wcc = sens_dist_all_wcc,
  sens_dist_ill_ousd = sens_dist_ill_ousd,
  sens_dist_ill_wcc = sens_dist_ill_wcc,
  Fr_dist_all_ousd = Fr_dist_all_ousd,
  Fr_dist_all_wcc = Fr_dist_all_wcc,
  Fr_dist_ill_ousd = Fr_dist_ill_ousd,
  Fr_dist_ill_wcc = Fr_dist_ill_wcc
)

saveRDS(bias_dists, file = paste0(res_dir, "bias_distributions.RDS"))


################################################################################ 
# Functions to estimate true A and T
################################################################################ 
# Documentation: estimate_true_rate
# Usage: estimate_true_rate(A_star, T_star, Fr, Se)
# Description: Apply values sensitivity (Se) and false positive rate (Fr)
# to the observed values of case counts (A_star) and person time (T_star)
# to obtain the "true" rate

# Args/Options:
# A_star: observed number of outcomes classified as 1
# T_star: observed person-time at risk
# Fr: false-positive rate = number of false positives per unit person-time 
# (number of students who aren't absent due to illness who are classified as 
#  illness absences per student-day)
# Se: sensitivity (probability outcome is classified as 1 if it is truly 1)

# Returns: returns estimated true rate 
# Output: none

estimate_true_rate = function(A_star, T_star, Fr, Se){
  if(Fr * T_star >= A_star) print("Warning: Fr value results in more false positives than were observed")
  A = (A_star - (Fr * T_star)) / Se
  true_rate = A / T_star
  # print(paste0("True rate: ", true_rate))
  return(true_rate)
}

estimate_true_rate(
  A_star = 30691,
  T_star = 933194,
  Fr = 0.0144923156771902,
  Se = 0.88828763071065
)

# returns estimated true rate difference

estimate_true_rate_difference = function(A_star1, T_star1, Fr1, Se1, A_star0, T_star0, Fr0, Se0){
  rate1 = estimate_true_rate(A_star1, T_star1, Fr1, Se1)
  rate0 = estimate_true_rate(A_star0, T_star0, Fr0, Se0)
  
  rd = rate1 - rate0
  
  return(rd)
}

################################################################################ 
# Function to perform probabilistic bias correction 
# data: 
################################################################################ 

# Documentation: correct_bias
# Usage:         correct_bias(data, year, outcome, Fr1_dist, Se1_dist, Fr0_dist, Se0_dist)
# Description:   Draw from the assumed distributions of sensitivity and
#                false positive rate within a specific year for a specific outcome. 
#                Calculate corrected difference-in-difference correcting for misclassification
#                for a given year and outcome. 

# Args/Options:
# data:          data frame containing observed data
# year:          year of analysis, as a string
# outcome:       outcome variable of interest, as a string
# Fr1_dist:      assumed distribution of false positive rate in the intervention group
# Fr0_dist:      assumed distribution of false positive rate in the comparison group
# Se1_dist:      assumed distribution of sensitivity in the intervention group
# Se0_dist:      assumed distribution of sensitivity in the comparison group

# Returns:       difference-in-difference corrected for misclassification
#                for a given year and outcome, as a scalar
# Output:        none

correct_bias = function(data, year, outcome, Fr1_dist, Se1_dist, Fr0_dist, Se0_dist){
  cat(".")
  # randomly sample from each distribution 
  Se1 = sample(Se1_dist, size = 1)
  Se0 = sample(Se0_dist, size = 1)
  Fr1 = sample(Fr1_dist, size = 1)
  Fr0 = sample(Fr0_dist, size = 1)
  
  if(!is.data.table(data)) data = data.table(data)
  
  # obtain counts of those with disease who were exposed vs. unexposed
  A_star1 = data[get(outcome)==1 & dist=="OUSD" & schoolyr_cat == year, .N]
  A_star0 = data[get(outcome)==1 & dist=="WCC" & schoolyr_cat == year, .N]
  A_star1_pre = data[get(outcome)==1 & dist=="OUSD" & schoolyr_cat == "Pre", .N]
  A_star0_pre = data[get(outcome)==1 & dist=="WCC" & schoolyr_cat == "Pre", .N]

  # obtain person-time among exposed and unexposed
  T_star1 = data[dist=="OUSD" & schoolyr_cat == year, .N]
  T_star0 = data[dist=="WCC" & schoolyr_cat == year, .N]
  T_star1_pre = data[dist=="OUSD" & schoolyr_cat == "Pre", .N]
  T_star0_pre = data[dist=="WCC" & schoolyr_cat == "Pre", .N]
  
  # calculate true risk difference for OUSD during
  # intervention minus pre-intervention
  rd_true1 = estimate_true_rate_difference(
    A_star1 = A_star1,
    T_star1 = T_star1,
    A_star0 = A_star1_pre,
    T_star0 = T_star1_pre,
    Fr1 = Fr1,
    Se1 = Se1,
    Fr0 = Fr1,
    Se0 = Se1
  )
  
  # calculate true risk difference for WCC during
  # intervention minus pre-intervention
  rd_true0 = estimate_true_rate_difference(
    A_star1 = A_star0,
    T_star1 = T_star0,
    A_star0 = A_star0_pre,
    T_star0 = T_star0_pre,
    Fr1 = Fr0,
    Se1 = Se0,
    Fr0 = Fr0,
    Se0 = Se0
  )
  
  # calculate difference-in-difference
  did_true = rd_true1 - rd_true0
  
  return(did_true)
}


################################################################################ 
# Perform probabilistic bias correction
################################################################################ 
# What about adjusted estimates?? are they even that different from unadjusted? 
# if not maybe don't worry about it. 

year_list = list("2014-15", "2015-16", "2016-17", "2017-18")
reps = 1000
  
#----------------------------------------
# Illness-specific absences
#----------------------------------------
set.seed(123)
corrected_did_ill_list = lapply(year_list, function(x) 
  replicate(reps, correct_bias(data = absentee_flu,
                            year = x,
                            outcome = "absent_ill",
                            Fr1_dist = Fr_dist_ill_ousd, 
                            Se1_dist = sens_dist_ill_ousd, 
                            Fr0_dist = Fr_dist_ill_wcc, 
                            Se0_dist = sens_dist_ill_wcc))
)
names(corrected_did_ill_list) = year_list
corrected_did_ill_df = bind_rows(corrected_did_ill_list)
corrected_did_ill_plot = melt(corrected_did_ill_df) %>% rename(year = variable, estimate = value)

#----------------------------------------
# All-cause absences
#----------------------------------------
corrected_did_all_list_y1_3 = lapply(year_list[1:3], function(x) 
  replicate(reps, correct_bias(data = absentee_flu,
                              year = x,
                              outcome = "absent_all",
                              Fr1_dist = Fr_dist_all_ousd, 
                              Se1_dist = sens_dist_all_ousd_1116, 
                              Fr0_dist = Fr_dist_all_wcc, 
                              Se0_dist = sens_dist_all_wcc))
)
names(corrected_did_all_list_y1_3) = year_list[1:3]

corrected_did_all_list_y4 = replicate(reps, correct_bias(data = absentee_flu,
                             year = year_list[4],
                             outcome = "absent_all",
                             Fr1_dist = Fr_dist_all_ousd, 
                             Se1_dist = sens_dist_all_ousd_1718, 
                             Fr0_dist = Fr_dist_all_wcc, 
                             Se0_dist = sens_dist_all_wcc))
names(corrected_did_all_list_y4) = year_list[4]


corrected_did_all_df = bind_rows(corrected_did_all_list_y1_3)
corrected_did_all_df = corrected_did_all_df %>%
  mutate(`2017-18` = corrected_did_all_list_y4)
corrected_did_all_plot = melt(corrected_did_all_df) %>% rename(year = variable, estimate = value)


################################################################################ 
# Prep output for plotting
################################################################################ 
corrected_did_ill_plot = corrected_did_ill_plot %>%
  group_by(year) %>%
  mutate(median = median(estimate),
         lb = quantile(estimate, probs = 0.025),
         ub = quantile(estimate, probs = 0.975),
         obs_est = case_when(
           year == "2014-15" ~ p2_unadj_ill$pt.est[p2_unadj_ill$year==1],
           year == "2015-16" ~ p2_unadj_ill$pt.est[p2_unadj_ill$year==2],
           year == "2016-17" ~ p2_unadj_ill$pt.est[p2_unadj_ill$year==3],
           year == "2017-18" ~ p2_unadj_ill$pt.est[p2_unadj_ill$year==4]
         ))

corrected_did_all_plot = corrected_did_all_plot %>%
  group_by(year) %>%
  mutate(median = median(estimate),
         lb = quantile(estimate, probs = 0.025),
         ub = quantile(estimate, probs = 0.975),
         obs_est = case_when(
           year == "2014-15" ~ p2_unadj_all$pt.est[p2_unadj_all$year==1],
           year == "2015-16" ~ p2_unadj_all$pt.est[p2_unadj_all$year==2],
           year == "2016-17" ~ p2_unadj_all$pt.est[p2_unadj_all$year==3],
           year == "2017-18" ~ p2_unadj_all$pt.est[p2_unadj_all$year==4]
         ))

saveRDS(corrected_did_ill_plot, paste0(res_dir, "bias_analysis_ill.RDS"))
saveRDS(corrected_did_all_plot, paste0(res_dir, "bias_analysis_all.RDS"))
