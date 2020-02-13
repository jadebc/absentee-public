##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018
# File for checking for positivity violations
# subgroup analysis during peak week of influenza
##############################################
# Load base scripts, define directories, load libraries
source(here::here("0-config.R"))

################################################################################
# Load all data
################################################################################

absentee_flu = read_rds(path = absentee_flu_path)

################################################################################
# Check for positivity violations
################################################################################

# subset to peak week
absentee_peakwk = as.data.table(absentee_flu %>% filter(peakwk_2.5 == 1))

# confirm number of schools per district
length(unique(absentee_peakwk$school[absentee_peakwk$dist == "OUSD"]))
length(unique(absentee_peakwk$school[absentee_peakwk$dist == "WCC"]))

# table of peak wk by month
table(absentee_peakwk$peakwk_2.5, absentee_peakwk$month)

# categorical vars
table(absentee_peakwk$matchid, absentee_peakwk$dist)
table(absentee_peakwk$schoolyr, absentee_peakwk$dist)
table(absentee_peakwk$month, absentee_peakwk$dist)
table(absentee_peakwk$grade, absentee_peakwk$dist)
table(absentee_peakwk$race, absentee_peakwk$dist)
table(absentee_peakwk$absent_ill, absentee_peakwk$dist)
table(absentee_peakwk$absent_all, absentee_peakwk$dist)
table(absentee_peakwk$fluseasCDC, absentee_peakwk$dist)

# continuous vars
mn.dist = absentee_peakwk %>%
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
