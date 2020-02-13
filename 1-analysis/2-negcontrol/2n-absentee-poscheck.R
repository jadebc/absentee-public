##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018
# File for checking for positivity violations
# Non flu season negative control analysis
##############################################
# Load base scripts, define directories, load libraries
source(here::here("0-config.R"))

################################################################################ 
# Load all data
################################################################################ 

absentee_nonflu = read_rds(path = absentee_nonflu_path)

################################################################################ 
# Check for positivity violations 
################################################################################ 

# Confirm number of schools per district
length(unique(absentee_nonflu$school[absentee_nonflu$dist == "OUSD"]))
length(unique(absentee_nonflu$school[absentee_nonflu$dist == "WCC"]))

# Table of peak wk by month
table(absentee_nonflu$peakwk_2.5, absentee_nonflu$month)

# Categorical vars
table(absentee_nonflu$matchid, absentee_nonflu$dist)
table(absentee_nonflu$schoolyr, absentee_nonflu$dist)
table(absentee_nonflu$month, absentee_nonflu$dist)
table(absentee_nonflu$grade, absentee_nonflu$dist)
table(absentee_nonflu$race, absentee_nonflu$dist)
table(absentee_nonflu$absent_ill, absentee_nonflu$dist)
table(absentee_nonflu$absent_all, absentee_nonflu$dist)
table(absentee_nonflu$fluseasCDC, absentee_nonflu$dist)

# Continuous vars
mn.dist.non = absentee_nonflu %>%
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

cbind(colnames(mn.dist.non), t(mn.dist.non))