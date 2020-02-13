##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018
# File calculating mean absences per student-day and crude differences in absences between districts
# Non flu season negative control analysis
##############################################
# Load base scripts, define directories, load libraries
source(here::here("0-config.R"))

################################################################################
# Load all data
################################################################################

absentee_nonflu = read_rds(path = absentee_flu_path)

################################################################################
# Clean data for analysis
################################################################################

# subset to relevant columns
absentee_nonflu$pre = ifelse(
  test = absentee_nonflu$schoolyr != "2014-15" &
    absentee_nonflu$schoolyr != "2015-16" &
    absentee_nonflu$schoolyr != "2016-17" &
    absentee_nonflu$schoolyr != "2017-18",
  yes = 1,
  no = 0
)
absentee_nonflu = absentee_nonflu[, c("dist",
                                      "matchid",
                                      "absent_ill",
                                      "absent_all",
                                      "schoolyr",
                                      "pre",
                                      "school")]

absentee_nonflu = absentee_nonflu[complete.cases(absentee_nonflu),]

# create variable for pre + schoolyr
absentee_nonflu = absentee_nonflu %>%
  mutate(schoolyrp = ifelse(pre == 1, "Pre", as.character(schoolyr))) %>%
  mutate(schoolyrp = factor(
    schoolyrp,
    levels = c("Pre", "2014-15",
               "2015-16", "2016-17", "2017-18")
  ))

################################################################################
# Plot means by year
################################################################################

# mean absences within categories
mn.abs = absentee_nonflu %>%
  group_by(dist, schoolyrp) %>%
  summarise(mn_all = mean(absent_all),
            mn_ill = mean(absent_ill))

mn.abs.l = mn.abs %>% gather(abstype, mean, mn_all:mn_ill) %>%
  mutate(mean = mean * 100)

pdf(
  file = paste0(res_dir, "means-yr-nonfluseas.pdf"),
  width = 7,
  height = 3
)

ggplot(mn.abs.l, aes(x = schoolyrp, y = mean, group = dist)) +
  geom_point(aes(col = dist)) + facet_wrap(~ abstype) +
  geom_line(aes(col = dist)) + ylab("Mean absences per 100 days")

dev.off()

################################################################################
# Plot differences by year
################################################################################

# spread data
mn.abs.w = mn.abs.l %>% spread(dist, mean) %>%
  mutate(diff = 100 * (OUSD - WCC))

pdf(
  file = paste0(res_dir, "mean-diff-yr-crude-nonfluseas.pdf"),
  width = 7,
  height = 3
)

ggplot(mn.abs.w, aes(x = schoolyrp, y = diff)) +
  geom_point() +  geom_line() + facet_wrap(~ abstype) +
  ylab("Mean difference in absences per 100 days") +
  geom_hline(yintercept = 0, linetype = "dashed")

dev.off()

################################################################################
# Save results
################################################################################

write_rds(
  x = list("mn.abs.l" = mn.abs.l, "mn.abs.w" = mn.abs.w),
  path = paste0(res_dir, "tables/", "1-unadj-table-raw-negc.RDS")
)
