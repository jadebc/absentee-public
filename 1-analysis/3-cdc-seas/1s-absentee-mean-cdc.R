##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018
# File calculating mean absences per student-day and crude differences in absences between districts
# CDC flu season definition 
##############################################
# load base scripts, define directories, load libraries
source(here::here("0-config.R"))

################################################################################
# Load all data
################################################################################

absentee_flu_CDC = read_rds(path = absentee_flu_CDC_path)

################################################################################
# Clean data for analysis
################################################################################

# subset to relevant columns
absentee_flu_CDC$pre = ifelse(
  test = absentee_flu_CDC$schoolyr != "2014-15" &
    absentee_flu_CDC$schoolyr != "2015-16" &
    absentee_flu_CDC$schoolyr != "2016-17" &
    absentee_flu_CDC$schoolyr != "2017-18",
  yes = 1,
  no = 0
)
absentee_flu_CDC = absentee_flu_CDC[, c("dist",
                                        "matchid",
                                        "absent_ill",
                                        "absent_all",
                                        "schoolyr",
                                        "pre",
                                        "school")]

absentee_flu_CDC = absentee_flu_CDC[complete.cases(absentee_flu_CDC), ]

# create variable for pre + schoolyr
absentee_flu_CDC = absentee_flu_CDC %>%
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
mn.abs = absentee_flu_CDC %>%
  group_by(dist, schoolyrp) %>%
  summarise(mn_all = mean(absent_all),
            mn_ill = mean(absent_ill))

mn.abs.l = mn.abs %>% gather(abstype, mean, mn_all:mn_ill) %>%
  mutate(mean = mean * 100)

pdf(
  file = paste0(fig_dir, "means-yr-flu.cdcseas.pdf"),
  width = 7,
  height = 3
)

ggplot(mn.abs.l, aes(x = schoolyrp, y = mean, group = dist)) +
  geom_point(aes(col = dist)) + facet_wrap( ~ abstype) +
  geom_line(aes(col = dist)) + ylab("Mean absences per 100 days")

dev.off()

################################################################################
# Plot differences by year
################################################################################
# spread data
mn.abs.w = mn.abs.l %>% spread(dist, mean) %>%
  mutate(diff = 100 * (OUSD - WCC))

pdf(
  file = paste0(fig_dir, "mean-diff-yr-crude-flu.cdcseas.pdf"),
  width = 7,
  height = 3
)

ggplot(mn.abs.w, aes(x = schoolyrp, y = diff)) +
  geom_point() +  geom_line() + facet_wrap( ~ abstype) +
  ylab("Mean difference in absences per 100 days") +
  geom_hline(yintercept = 0, linetype = "dashed")

dev.off()

################################################################################
# Save results
################################################################################

write_rds(
  x = list("mn.abs.l" = mn.abs.l, "mn.abs.w" = mn.abs.w),
  path = paste0(res_dir, "tables/", "1-unadj-table-raw-cdc_seas.RDS")
)
