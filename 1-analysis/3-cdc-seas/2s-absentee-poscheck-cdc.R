##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018
# File for checking for positivity violations
# cdc flu season definition 
##############################################
# Load base scripts, define directories, load libraries
source(here::here("0-config.R"))

################################################################################ 
# Load all data
################################################################################ 

absentee_flu_CDC = read_rds(path = absentee_flu_CDC_path)

################################################################################ 
# Check for positivity violations
################################################################################ 

# Confirm number of schools per district
length(unique(absentee_flu_CDC$school[absentee_flu_CDC$dist == "OUSD"]))
length(unique(absentee_flu_CDC$school[absentee_flu_CDC$dist == "WCC"]))

# Table of peak wk by month
table(absentee_flu_CDC$peakwk_2.5, absentee_flu_CDC$month)

# Categorical vars
table(absentee_flu_CDC$matchid, absentee_flu_CDC$dist)
table(absentee_flu_CDC$schoolyr, absentee_flu_CDC$dist)
table(absentee_flu_CDC$month, absentee_flu_CDC$dist)
table(absentee_flu_CDC$grade, absentee_flu_CDC$dist)
table(absentee_flu_CDC$race, absentee_flu_CDC$dist)
table(absentee_flu_CDC$absent_ill, absentee_flu_CDC$dist)
table(absentee_flu_CDC$absent_all, absentee_flu_CDC$dist)

# Continuous vars
mn.dist = absentee_flu_CDC %>%
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
