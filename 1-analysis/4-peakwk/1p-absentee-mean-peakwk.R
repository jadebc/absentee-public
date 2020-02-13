##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018
# File calculating mean absences per student-day and crude differences in absences between districts
# Subgroup analysis during peak week of influenza
##############################################
# load base scripts, define directories, load libraries
source(here::here("0-config.R"))

################################################################################
# Load all data
################################################################################

absentee_flu = read_rds(path = absentee_flu_path)

################################################################################
# Clean data for analysis
################################################################################

# subset to peak week
absentee_peakwk = as.data.table(absentee_flu %>% filter(peakwk_2.5==1))

# subset to relevant columns
absentee_peakwk = absentee_peakwk[,pre:=ifelse(absentee_peakwk$schoolyr!="2014-15" & absentee_peakwk$schoolyr!="2015-16" &
                          absentee_peakwk$schoolyr!="2016-17" & absentee_peakwk$schoolyr!="2017-18",1,0)]

absentee_peakwk=absentee_peakwk[,.(dist,absent_ill,absent_all,schoolyr,pre,school)]
absentee_peakwk=absentee_peakwk[complete.cases(absentee_peakwk),]

# create variable for pre + schoolyr
absentee_peakwk = absentee_peakwk[,schoolyrp:=ifelse(pre==1,"Pre",as.character(schoolyr))]
absentee_peakwk = absentee_peakwk[,schoolyrp:=factor(schoolyrp,levels=c("Pre","2014-15","2015-16","2016-17","2017-18"))]

################################################################################
# Plot means by year
################################################################################

# mean absences within categories
mn.abs = absentee_peakwk %>%
  group_by(dist, schoolyrp) %>%
  summarise(mn_all = mean(absent_all),
            mn_ill = mean(absent_ill))

mn.abs.l = mn.abs %>% gather(abstype, mean, mn_all:mn_ill) %>%
  mutate(mean = mean * 100)

pdf(
  file = paste0(fig_dir, "means-yr-peakwk.pdf"),
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
  file = paste0(res_dir, "mean-diff-yr-crude-peakwk.pdf"),
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
  path = paste0(res_dir, "tables/", "1-unadj-table-peakwk.RDS")
)