##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2017

# Plot of flu season definition using CDPH data
##############################################
# load base scripts, define directories, load libraries
source(here::here("0-config.R"))

cdphpre17 = read_csv(file = raw_cdph_pre_2017_path)
cdph1718  = read_csv(file = raw_cdph_2017_2018_path)

cdph      = rbind(cdphpre17, cdph1718)

# clean dates
cdph$weekending=as.character(cdph$weekending)
slash=gregexpr("/",as.character(cdph$weekending))
slash1=matrix(NA,length(slash),1)
slash2=matrix(NA,length(slash),1)
for(i in 1:length(slash)){
  slash1[i,]=unlist(slash[i])[1]
  slash2[i,]=unlist(slash[i])[2]
}
cdph$slash1=slash1
cdph$slash2=slash2
cdph$mon=substr(cdph$weekending,1,cdph$slash1)
cdph$mon=as.numeric(gsub("/","",cdph$mon))
cdph$day=as.numeric(substr(cdph$weekending,cdph$slash1+1,cdph$slash2-1))
cdph$yr=as.numeric(substr(cdph$weekending,cdph$slash2+1,slash2+3))+2000
cdph$slash1=NULL
cdph$slash2=NULL

cdph$date=paste0(cdph$day,"-",cdph$mon,"-",cdph$yr)
cdph$date = as.Date(cdph$date, format = "%d-%m-%Y")

cdph$year = year(cdph$date)
cdph$month = month(cdph$date)

cdph_stf = cdph[cdph$yr>=2014 ,]

cdph_stf = cdph_stf %>% mutate(
  seas = case_when(
    yr==2014 & (mon>8 | mon<=12) ~ "2014-15",
    yr==2015 & (mon>=1 | mon<=5) ~ "2014-15",
    
    yr==2015 & (mon>8 | mon<=12) ~ "2015-16",
    yr==2016 & (mon>=1 | mon<=5) ~ "2015-16",
    
    yr==2016 & (mon>8 | mon<=12) ~ "2016-17",
    yr==2017 & (mon>=1 | mon<=5) ~ "2016-17",
    
    yr==2017 & (mon>8 | mon<=12) ~ "2017-18",
    yr==2018 & (mon>=1 | mon<=5) ~ "2017-18"
    
  )
)

drops = which(cdph_stf$month<8 & cdph_stf$year==2014)
cdph_stf = cdph_stf[-drops,]

pdf(paste0(fig_dir, "fig_cdph_season.pdf"), width=14, height=4)
ggplot(cdph_stf, aes(x= date, y=ILIper))+
  geom_line()+
  geom_hline(yintercept=2, linetype="dashed", col="blue")+
  scale_y_continuous(limits=c(0,7.5),breaks=seq(0,7,1), labels=seq(0,7,1))+
  scale_x_date(date_breaks="months"  , labels = date_format("%b"))+
  xlab("Month")+
  ylab("Percentage of Influenza-Like Illness Visits")+
  theme_bw() +
  geom_ribbon(aes(ymin = 0, ymax = ILIper),
              fill = "#FC5400")  +
  geom_area(aes(y = ifelse(ILIper >= 2,2,ILIper)), fill="#D9E7FA") +
  annotate("text", x = as.Date(c("2015-1-15","2016-1-15","2017-1-15","2018-1-15")),
  y=7.4, label=c("2014-15","2015-16","2016-17","2017-18"), size = 5)
dev.off()
