##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018
# File for checking for positivity violations
##############################################
# Load base scripts, define directories, load libraries
source(here::here("0-config.R"))

################################################################################ 
# Load all data
################################################################################ 

absentee_flu     = read_rds(path = absentee_flu_path)
absentee_flu_adj = read_rds(path = absentee_flu_adj_path)

################################################################################ 
# Check for positivity violations
################################################################################ 

# Confirm number of schools per district
length(unique(absentee_flu$school[absentee_flu$dist=="OUSD"]))
length(unique(absentee_flu$school[absentee_flu$dist=="WCC"]))

# Table of peak wk by month
table(absentee_flu$peakwk_2.5,absentee_flu$month)

# Categorical vars
table(absentee_flu$matchid,absentee_flu$dist)
table(absentee_flu$schoolyr,absentee_flu$dist)
table(absentee_flu$month,absentee_flu$dist)
table(absentee_flu$grade,absentee_flu$dist)
table(absentee_flu$race,absentee_flu$dist)
table(absentee_flu$absent_ill,absentee_flu$dist)
table(absentee_flu$absent_all,absentee_flu$dist)
table(absentee_flu$fluseasCDC,absentee_flu$dist)

################################################################################ 
# Propensity Score calculations in absentee_flu_adj
################################################################################
view_pscore(data = absentee_flu_adj$y1.ill,
            y = "absent_ill",
            fig_filename = "pscore_y1_ill.pdf")
view_pscore(data = absentee_flu_adj$y2.ill,
            y = "absent_ill",
            fig_filename = "pscore_y2_ill.pdf")
view_pscore(data = absentee_flu_adj$y3.ill,
            y = "absent_ill",
            fig_filename = "pscore_y3_ill.pdf")

view_pscore(data = absentee_flu_adj$y1.all,
            y = "absent_all",
            fig_filename = "pscore_y1_all.pdf")
view_pscore(data = absentee_flu_adj$y2.all,
            y = "absent_all",
            fig_filename = "pscore_y2_all.pdf")
view_pscore(data = absentee_flu_adj$y3.all,
            y = "absent_all",
            fig_filename = "pscore_y3_all.pdf")

# continuous vars
mn.dist = absentee_flu %>%
  select(
    -c(
      yr,
      schoolyr,
      date,
      matchid,
      month,
      grade,
      race,
      absent_ill,
      absent_all,
      school,
      dist.n
    )
  ) %>%
  group_by(dist) %>%
  summarise_all(funs(mean(., na.rm = TRUE)))

cbind(colnames(mn.dist), t(mn.dist))
