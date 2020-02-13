 ##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2018

# plot pre-program and program absentee trends
# only includes matched schools
##############################################
# load base scripts, define directories, load libraries
source(here::here("0-config.R")) 
source(here::here("0-data-prep/0-district-noschool.R")) 

  
# ---------------------------------------------
# Loading Data
# ---------------------------------------------

# Load absenteeism data
absentee_all = readRDS(absentee_all_clean_path) 
absentee_flu = readRDS(absentee_flu_path)   
cdph = readRDS(cdph_path)

x = absentee_all %>% filter(date >= as.Date("2017-10-05") & date <= as.Date("2017-10-15")) %>% 
                     group_by(dist.n, absent_all, absent_ill, date) %>% 
                     summarise(n = n())

# Load recorded dates from each district
dates = load_to_list(paste0(data_dir,"dist_dates.RData"))
w.dates = dates$w.dates
o.dates = dates$o.dates

# ---------------------------------------------
# Merging Data & Describing Change
# ---------------------------------------------

# CHANGE ME TO CHANGE WHAT IS BEING GRAPHED -- absentee_all vs absentee_flu
joined_absentee_data = absentee_all %>% 
  left_join(o.dates, by="date") %>% 
  left_join(w.dates, by="date")


joined_filtered_absentee_data = joined_absentee_data %>% 
  filter(date.o==1 & date.w==1) %>%
  select(-c(date.o, date.w, n.x, n.y))

initial_rows = joined_absentee_data %>% nrow
final_rows = joined_filtered_absentee_data %>% nrow

paste("Total Number of starting rows:", initial_rows) %>% print
paste("Total Number of ending rows:", final_rows) %>% print
paste("Number of rows dropped:", initial_rows - final_rows) %>% print
paste("Percentage of rows dropped:", (initial_rows - final_rows)/initial_rows ) %>% print

pryr::object_size(joined_filtered_absentee_data) %>% print

# Free up RAM.
gc()

# ---------------------------------------------
# Mutating Data to Prep for Plotting
# ---------------------------------------------

joined_filtered_downsampled_absentee_data = down_sample(data=joined_filtered_absentee_data,
                                                        sample_pct=.01,
                                                        seed=1)

# CHANGE ME IF YOU WANT TO USE THE DOWNSAMPLED DATA
# data = joined_filtered_downsampled_absentee_data
data = joined_filtered_absentee_data

data = data %>% as.data.table

# Group by school-year, district, and day, then calculate the mean
# all-absence and illness-specific absense rate for that day.
daily_agg = data[, 
                 lapply(.SD, mean), by = .(schoolyr, dist, date),
                 .SDcols = c("absent_ill", "absent_all")]

# Start the school year on August 1st.
school_day_of_year = function(date, start_date="2017-08-01") {
  date = as.Date(date)
  
  start_day = as.integer(format(as.Date(start_date), "%j"))
  
  day_of_year = as.integer(format(date, "%j")) - start_day
  
  # Move negative days (spring semester) forward to the next year.
  day_of_year = day_of_year %% 365
  
  return(day_of_year)
}

# Calculate day of year to help with facetted plots - Min = 7, max = 319
daily_agg$day = school_day_of_year(daily_agg$date) 
summary(daily_agg$day)

summary(daily_agg[, c("day", "absent_all", "absent_ill")])
summary(daily_agg[, .(day, absent_all, absent_ill)])
summary(daily_agg$absent_all)

# If there are no absences in that district for that day, it means school was off (174 district-day pairs)
table(daily_agg$absent_all == 0)

# Set absence rates on non-school days to NA, to avoid plotting a point for that district.
# TODO: Or remove that row entirely?
daily_agg[daily_agg$absent_all == 0, c("absent_ill", "absent_all")] = NA
head(daily_agg)

# Look at the top 20 days with the highest absence rates.
daily_agg_sorted = daily_agg[order(daily_agg$absent_all, decreasing = T), ]
daily_agg_sorted[1:20, c("date", "dist", "absent_ill", "absent_all")]

# Identify day of year for Jan 1st, Feb 1st, ..., Dec. 1st.
# Note that this will be off by 1 day if it's a leap-year, not a huge deal though.
(month_starts = as.Date(paste0("2017-", 1:12, "-01")))
(month_breaks = school_day_of_year(month_starts))
# Abbreviated month name to label the graph.
(month_labels = format(month_starts, "%b"))

#(flu_start = school_day_of_year("2017-10-01"))
#(flu_end = school_day_of_year("2017-05-01"))

# Gets the beginning day or ending day of flu season from CDPH data given a flu dataset, such as absentee_flu, the schoolyr, as a string and start boolean (True => Start, False => End)
get_CDPH_flu_start_or_end = function(flu, schoolyr, start=TRUE) {
  valid_schoolyrs = flu$schoolyr %>% levels
  assert_that((schoolyr %>% is.character | schoolyr %>% is.factor) &
                schoolyr %in% valid_schoolyrs &
                start %>% is.logical)
  
  # Subset flu season data to just this schoolyr
  schoolyr_arg = schoolyr
  flu_schoolyr = flu %>% filter(schoolyr == schoolyr_arg)
  
  # Find the earliest or latest date
  assert_that(flu_schoolyr$date %>% is.Date())
  
  if (start) {
    desired_date = (flu_schoolyr$date %>% unique %>% sort(decreasing=FALSE))[1] 
  } else {
    desired_date = (flu_schoolyr$date %>% unique %>% sort(decreasing=TRUE))[1]
  }
  
  # Convert the date into a numbered school day and and return it
  return(school_day_of_year(date=desired_date))
}

# Start of flu season for each year
flu_start = pmap(.f=get_CDPH_flu_start_or_end,
     .l=list(flu=list(absentee_flu %>% as.data.frame),
             #.l=list(flu=list(absentee_all %>% as.data.frame),
             schoolyr=as.list(c("2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18")),
             start=list(TRUE))) %>% unlist

# End of flu season for each year
flu_end = pmap(.f=get_CDPH_flu_start_or_end,
                 .l=list(flu=list(absentee_flu %>% as.data.frame),
                         #.l=list(flu=list(absentee_all %>% as.data.frame),
                         schoolyr=as.list(c("2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18")),
                         start=list(FALSE))) %>% unlist

# for 1 year, complete and fill all days
complete_days <- function(data, year, range) {
  data <- data %>% filter(schoolyr == year) %>% mutate(date = as.Date(date)) %>%
    filter(!(as.Date(date) %in% ousd_noschool) & 
             !(as.Date(date) %in% wcc_noschool)) %>%
    complete(dist, date = seq.Date(min(date), max(date), by="day")) %>%
    fill(schoolyr) %>%
    mutate(day = c(range, range)) %>% 
    arrange(day) %>% 
    arrange(dist)
  return(data)
}

# fill in all missing dates in the year for 2011-18
daily_agg_1112_days_completed = complete_days(daily_agg, "2011-12", 29:313)
daily_agg_1213_days_completed = complete_days(daily_agg, "2012-13", 27:311)
daily_agg_1314_days_completed = complete_days(daily_agg, "2013-14", 26:310)
daily_agg_1415_days_completed = complete_days(daily_agg, "2014-15", 25:309)
daily_agg_1516_days_completed = complete_days(daily_agg, "2015-16", 24:313)
daily_agg_1617_days_completed = complete_days(daily_agg, "2016-17", 22:312)
daily_agg_1718_days_completed = complete_days(daily_agg, "2017-18", 21:311)

daily_agg_prepped = rbind(daily_agg_1112_days_completed,
                          daily_agg_1213_days_completed,
                          daily_agg_1314_days_completed,
                          daily_agg_1415_days_completed,
                          daily_agg_1516_days_completed,
                          daily_agg_1617_days_completed,
                          daily_agg_1718_days_completed) 
# With min and max dates
peak_flu_school_yr = absentee_flu %>% 
                          filter(peakwk_2.5==1) %>% 
                          group_by(schoolyr) %>% 
                          summarise(mindate = min(date), maxdate = max(date)) %>% 
                          mutate(start = school_day_of_year(mindate), 
                                 end = school_day_of_year(maxdate))


# ---------------------------------------------
# Plot Data
# ---------------------------------------------
# TODO:
# * Rug - e.g. with holidays noted?
#
# All of these settings are optimized for the zoomed in, exported version
# of the chart, not the small version visible in RStudio.

plot_daily_absence = function(data, type = "point", result = "all", sep = FALSE) {
  
  # Full plot from 11-12 to 17-18
  if (sep == FALSE) {
    
    # Absentee-all
    if (result == "all") {
      
      bar_y_max = 0.20
      
      p <- ggplot(data = data,
                 aes(x = day,
                     y = absent_all,
                     color = dist)) + 
        
        scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4),
                           labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%", "35%", "40%"),
                           minor_breaks = NULL) +
        
        labs(title = NULL,
             x = NULL, 
             y = NULL) +
  
      # Gray box 1: early non-flu season (Aug - Sep.) - Add variably-sized grey bars for faceted plot based on start of flu seas
      geom_rect(data = data.frame(schoolyr = c("2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17"),
                                  day = rep(1, 6),
                                  absent_all = rep(0, 6),
                                  dist = rep("OUSD", 6)),
                aes(xmin = 0,
                    xmax = flu_start[0:6],
                    ymin = 0,
                    ymax = bar_y_max),
                alpha = 0.2,
                fill = "grey",
                color = "grey",
                linetype = "blank") +
        
        # '17-18: Gray box 1 early non-flu season (Aug - Sep.) - Add variably-sized grey bars for faceted plot based on start of flu seas
        geom_rect(data = data.frame(schoolyr = c("2017-18"),
                                    day = rep(1, 1),
                                    absent_all = rep(0, 1),
                                    dist = rep("OUSD", 1)),
                  aes(xmin = 0,
                      xmax = flu_start[7],
                      ymin = 0,
                      ymax = 0.30),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        
        
        # Gray box late non-flu season (May - Jul.) - Add variably-sized grey bars for faceted plot based on end of flu seas
        geom_rect(data = data.frame(schoolyr = c("2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17"),
                                    day = rep(1, 6),
                                    absent_all = rep(0, 6),
                                    dist = rep("OUSD", 6)),
                  aes(xmin = flu_end[0:6],
                      xmax = 319,
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # '17-18: Gray box late non-flu season (May - Jul.) - Add variably-sized grey bars for faceted plot based on end of flu seas
        geom_rect(data = data.frame(schoolyr = c("2017-18"),
                                    day = rep(1, 1),
                                    absent_all = rep(0, 1),
                                    dist = rep("OUSD", 1)),
                  aes(xmin = flu_end[7],
                      xmax = 319,
                      ymin = 0,
                      ymax = 0.30),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # Red box: peak flu season (Aug - Sep.) - Add red bars for faceted plot based on peak week of flu season
        geom_rect(data = data.frame(schoolyr = c("2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17"),
                                    day = rep(1, 6),
                                    absent_all = rep(0, 6),
                                    dist = rep("OUSD", 6)),
                  aes(xmin = peak_flu_school_yr$start[0:6],
                      xmax = (peak_flu_school_yr$start[0:6] + 7),
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "red",
                  color = "red",
                  linetype = "blank") +
        
        # '17-18: Red box: peak flu season (Aug - Sep.) - Add red bars for faceted plot based on peak week of flu season
        geom_rect(data = data.frame(schoolyr = c("2017-18"),
                                    day = rep(1, 1),
                                    absent_all = rep(0, 1),
                                    dist = rep("OUSD", 1)),
                  aes(xmin = peak_flu_school_yr$start[7],
                      xmax = peak_flu_school_yr$start[7] + 7,
                      ymin = 0,
                      ymax = 0.30),
                  alpha = 0.2,
                  fill = "red",
                  color = "red",
                  linetype = "blank") +
        
        # Text labels for each of the program years
        geom_label(data = data.frame(schoolyr = c("2014-15", "2015-16", "2016-17"),
                                     label = c("Year 1", "Year 2", "Year 3"),
                                     day = rep(1, 3),
                                     absent_all = rep(0, 3),
                                     dist = rep("OUSD", 3)),
                   aes(label = label),
                   show.legend = F,
                   hjust = "left",
                   color = "black",
                   x = 20,
                   y = bar_y_max * 0.7,
                   size = 7) +
        
        # '17-18: Text labels for each of the program years
        geom_label(data = data.frame(schoolyr = c("2017-18"),
                                     label = c("Year 4"),
                                     day = rep(1, 1),
                                     absent_all = rep(0, 1),
                                     dist = rep("OUSD", 1)),
                   aes(label = label),
                   show.legend = F,
                   hjust = "left",
                   color = "black",
                   x = 20,
                   y = 0.30 * 0.7,
                   size = 7) +
        
        # Plot a dotted line above each of the 3 program years.
        geom_segment(data = data %>% filter(schoolyr %in% c("2014-15", "2015-16", "2016-17")),
                     aes(x = 10,
                         xend = 320,
                         y = bar_y_max * 1.1,
                         yend = bar_y_max * 1.1),
                     linetype = "dashed",
                     color = "#aaaaaa",
                     show.legend = F) +
        
        # '17-18: Plot a dotted line above each of the 3 program years.
        geom_segment(data = data %>% filter(schoolyr %in% c("2017-18")),
                     aes(x = 10,
                         xend = 320,
                         y = 0.30 * 1.1,
                         yend = 0.30 * 1.1),
                     linetype = "dashed",
                     color = "#aaaaaa",
                     show.legend = F)
      
    # Illness-specific
    } else {
      
      bar_y_max = 0.08
      
      p <- ggplot(data = data,
                 aes(x = day,
                     y = absent_ill,
                     #group = dist,
                     color = dist)) +
      
        scale_y_continuous(breaks = c(0, 0.02, 0.04, 0.06, 0.08),
                         labels = c("0%", "2%", "4%", "6%", "8%"),
                         minor_breaks = NULL) +
        
        labs(title = NULL,
             x = NULL, 
             y = NULL) +
        
        # Gray box 1: early non-flu season (Aug - Sep.) - Add variably-sized grey bars for faceted plot based on start of flu seas
        geom_rect(data = data.frame(schoolyr = c("2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18"),
                                    day = rep(1, 7),
                                    absent_ill = rep(0, 7), 
                                    dist = rep("OUSD", 7)),
                  aes(xmin = 0,
                      xmax = flu_start,
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # Gray box late non-flu season (May - Jul.) - Add variably-sized grey bars for faceted plot based on end of flu seas
        geom_rect(data = data.frame(schoolyr = c("2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18"),
                                    day = rep(1, 7),
                                    absent_ill = rep(0, 7),
                                    dist = rep("OUSD", 7)),
                  aes(xmin = flu_end,
                      xmax = 319,
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # Text labels for each of the program years
        geom_label(data = data.frame(schoolyr = c("2014-15", "2015-16", "2016-17", "2017-18"),
                                     label = c("Year 1", "Year 2", "Year 3", "Year 4"),
                                     day = rep(1, 4),
                                     absent_ill = rep(0, 4), 
                                     dist = rep("OUSD", 4)),
                   aes(label = label),
                   show.legend = F,
                   hjust = "left",
                   color = "black",
                   x = 20,
                   y = bar_y_max * 0.7,
                   size = 7) +
      
      # Plot a dotted line above each of the 3 program years.
      geom_segment(data = data %>% filter(schoolyr %in% c("2014-15", "2015-16", "2016-17", "2017-18")),
                   aes(x = 10,
                       xend = 320,
                       y = bar_y_max * 1.1,
                       yend = bar_y_max * 1.1),
                   linetype = "dashed",
                   color = "#aaaaaa",
                   show.legend = F)
    }
    
    # Point version.
    if (type == "point") {
      p = p + geom_point(size = 0.9, na.rm = T) 
      
      # Line version.
    } else {
      p = p + geom_line(size = .6, show.legend = T) 
    }
    
    p = p +   
      
      theme_minimal() +
      
      scale_x_continuous(
        #"School day, where 1 = August 1st", #limits = c(0, 366)
        breaks = month_breaks,
        labels = month_labels,
        minor_breaks = NULL,
        # Prevent ggplot from extending the x-axis outside the data bounds.
        expand = c(0, 0)) +
      
      # Customize legend labels.
      scale_color_manual("",
                         values = c("#2185c5","#ff9715"),
                         breaks = c("OUSD", "WCC"),
                         labels = c("Intervention District (OUSD)", "Comparison District (WCCUSD)")) +
      
      # Show a separate plot with each school year in its own row.
      facet_grid(schoolyr ~ ., scale = "free_y") +
      
      theme(legend.position   = "bottom",         
            legend.title      = element_blank(),
            legend.margin     = margin(t=2, r=2, b=2, l=2),
            legend.text       = element_text(size = 9), 
            strip.text.y      = element_text(size = 14),
            axis.text.x       = element_text(size = 14),
            axis.text.y       = element_text(size = 12),
            plot.title        = element_text(size = 0),
            legend.background = element_rect(fill = "#fafafa",
                                             color = "#bbbbbb")) 
    
    return(invisible(p))
  
  # Two separate plots, one for pre-program, one for post-program
  } else {
    
    # Absentee-all
    if (result == "all") {
      
      bar_y_max = 0.20
      
      p1 <- ggplot(data = data %>% filter(schoolyr == "2011-12" | schoolyr == "2012-13" | schoolyr == "2013-14"),
                  aes(x = day,
                      y = absent_all,
                      color = dist)) + 
        
        scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4),
                           labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%", "35%", "40%"),
                           minor_breaks = NULL) +
        
        labs(title = NULL,
             x = NULL, 
             y = NULL) +
        
        # Gray box 1: early non-flu season (Aug - Sep.) - Add variably-sized grey bars for faceted plot based on start of flu seas
        geom_rect(data = data.frame(schoolyr = c("2011-12", "2012-13", "2013-14"),
                                    day = rep(1, 3),
                                    absent_all = rep(0, 3),
                                    dist = rep("OUSD", 3)),
                  aes(xmin = 0,
                      xmax = flu_start[0:3],
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # Gray box late non-flu season (May - Jul.) - Add variably-sized grey bars for faceted plot based on end of flu seas
        geom_rect(data = data.frame(schoolyr = c("2011-12", "2012-13", "2013-14"),
                                    day = rep(1, 3),
                                    absent_all = rep(0, 3),
                                    dist = rep("OUSD", 3)),
                  aes(xmin = flu_end[0:3],
                      xmax = 319,
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # Red box: peak flu season (Aug - Sep.) - Add red bars for faceted plot based on peak week of flu season
        geom_rect(data = data.frame(schoolyr = c("2011-12", "2012-13", "2013-14"),
                                    day = rep(1, 3),
                                    absent_all = rep(0, 3),
                                    dist = rep("OUSD", 3)),
                  aes(xmin = peak_flu_school_yr$start[0:3],
                      xmax = (peak_flu_school_yr$start[0:3] + 7),
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "red",
                  color = "red",
                  linetype = "blank") 
      
      p2 <- ggplot(data = data %>% filter(schoolyr == "2014-15" | schoolyr == "2015-16" | 
                                          schoolyr == "2016-17" | schoolyr == "2017-18"),
                   aes(x = day,
                       y = absent_all,
                       color = dist)) + 
        
        scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4),
                           labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%", "35%", "40%"),
                           minor_breaks = NULL) +
        
        labs(title = NULL,
             x = NULL, 
             y = NULL) +
        
        # Gray box 1: early non-flu season (Aug - Sep.) - Add variably-sized grey bars for faceted plot based on start of flu seas
        geom_rect(data = data.frame(schoolyr = c("2014-15", "2015-16", "2016-17"),
                                    day = rep(1, 3),
                                    absent_all = rep(0, 3),
                                    dist = rep("OUSD", 3)),
                  aes(xmin = 0,
                      xmax = flu_start[4:6],
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # 17-18 Gray box 1: early non-flu season (Aug - Sep.) - Add variably-sized grey bars for faceted plot based on start of flu seas
        geom_rect(data = data.frame(schoolyr = c("2017-18"),
                                    day = rep(1, 1),
                                    absent_all = rep(0, 1),
                                    dist = rep("OUSD", 1)),
                  aes(xmin = 0,
                      xmax = flu_start[7],
                      ymin = 0,
                      ymax = .3),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # Gray box late non-flu season (May - Jul.) - Add variably-sized grey bars for faceted plot based on end of flu seas
        geom_rect(data = data.frame(schoolyr = c("2014-15", "2015-16", "2016-17"),
                                    day = rep(1, 3),
                                    absent_all = rep(0, 3),
                                    dist = rep("OUSD", 3)),
                  aes(xmin = flu_end[4:6],
                      xmax = 319,
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # 17-18 Gray box late non-flu season (May - Jul.) - Add variably-sized grey bars for faceted plot based on end of flu seas
        geom_rect(data = data.frame(schoolyr = c("2017-18"),
                                    day = rep(1, 1),
                                    absent_all = rep(0, 1),
                                    dist = rep("OUSD", 1)),
                  aes(xmin = flu_end[7],
                      xmax = 319,
                      ymin = 0,
                      ymax = .3),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # Red box: peak flu season (Aug - Sep.) - Add red bars for faceted plot based on peak week of flu season
        geom_rect(data = data.frame(schoolyr = c("2014-15", "2015-16", "2016-17"),
                                    day = rep(1, 3),
                                    absent_all = rep(0, 3),
                                    dist = rep("OUSD", 3)),
                  aes(xmin = peak_flu_school_yr$start[4:6],
                      xmax = (peak_flu_school_yr$start[4:6] + 7),
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "red",
                  color = "red",
                  linetype = "blank") +
        
        # 17-18 Red box: peak flu season (Aug - Sep.) - Add red bars for faceted plot based on peak week of flu season
        geom_rect(data = data.frame(schoolyr = c("2017-18"),
                                    day = rep(1, 1),
                                    absent_all = rep(0, 1),
                                    dist = rep("OUSD", 1)),
                  aes(xmin = peak_flu_school_yr$start[7],
                      xmax = (peak_flu_school_yr$start[7] + 7),
                      ymin = 0,
                      ymax = .3),
                  alpha = 0.2,
                  fill = "red",
                  color = "red",
                  linetype = "blank")
      
      # Illness-specific
    } else {
      
      bar_y_max = 0.08
      
      p1 <- ggplot(data = data %>% filter(schoolyr == "2011-12" | schoolyr == "2012-13" | schoolyr == "2013-14"),
                   aes(x = day,
                       y = absent_ill,
                       color = dist)) + 
        
        scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4),
                           labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%", "35%", "40%"),
                           minor_breaks = NULL) +
        
        labs(title = NULL,
             x = NULL, 
             y = NULL) +
        
        # Gray box 1: early non-flu season (Aug - Sep.) - Add variably-sized grey bars for faceted plot based on start of flu seas
        geom_rect(data = data.frame(schoolyr = c("2011-12", "2012-13", "2013-14"),
                                    day = rep(1, 3),
                                    absent_all = rep(0, 3),
                                    dist = rep("OUSD", 3)),
                  aes(xmin = 0,
                      xmax = flu_start[0:3],
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # Gray box late non-flu season (May - Jul.) - Add variably-sized grey bars for faceted plot based on end of flu seas
        geom_rect(data = data.frame(schoolyr = c("2011-12", "2012-13", "2013-14"),
                                    day = rep(1, 3),
                                    absent_all = rep(0, 3),
                                    dist = rep("OUSD", 3)),
                  aes(xmin = flu_end[0:3],
                      xmax = 319,
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # Red box: peak flu season (Aug - Sep.) - Add red bars for faceted plot based on peak week of flu season
        geom_rect(data = data.frame(schoolyr = c("2011-12", "2012-13", "2013-14"),
                                    day = rep(1, 3),
                                    absent_all = rep(0, 3),
                                    dist = rep("OUSD", 3)),
                  aes(xmin = peak_flu_school_yr$start[0:3],
                      xmax = (peak_flu_school_yr$start[0:3] + 7),
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "red",
                  color = "red",
                  linetype = "blank") 
      
      p2 <- ggplot(data = data %>% filter(schoolyr == "2014-15" | schoolyr == "2015-16" | 
                                            schoolyr == "2016-17" | schoolyr == "2017-18"),
                   aes(x = day,
                       y = absent_ill,
                       color = dist)) + 
        
        scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4),
                           labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%", "35%", "40%"),
                           minor_breaks = NULL) +
        
        labs(title = NULL,
             x = NULL, 
             y = NULL) +
        
        # Gray box 1: early non-flu season (Aug - Sep.) - Add variably-sized grey bars for faceted plot based on start of flu seas
        geom_rect(data = data.frame(schoolyr = c("2014-15", "2015-16", "2016-17"),
                                    day = rep(1, 3),
                                    absent_all = rep(0, 3),
                                    dist = rep("OUSD", 3)),
                  aes(xmin = 0,
                      xmax = flu_start[4:6],
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # 17-18 Gray box 1: early non-flu season (Aug - Sep.) - Add variably-sized grey bars for faceted plot based on start of flu seas
        geom_rect(data = data.frame(schoolyr = c("2017-18"),
                                    day = rep(1, 1),
                                    absent_all = rep(0, 1),
                                    dist = rep("OUSD", 1)),
                  aes(xmin = 0,
                      xmax = flu_start[7],
                      ymin = 0,
                      ymax = .3),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # Gray box late non-flu season (May - Jul.) - Add variably-sized grey bars for faceted plot based on end of flu seas
        geom_rect(data = data.frame(schoolyr = c("2014-15", "2015-16", "2016-17"),
                                    day = rep(1, 3),
                                    absent_all = rep(0, 3),
                                    dist = rep("OUSD", 3)),
                  aes(xmin = flu_end[4:6],
                      xmax = 319,
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # 17-18 Gray box late non-flu season (May - Jul.) - Add variably-sized grey bars for faceted plot based on end of flu seas
        geom_rect(data = data.frame(schoolyr = c("2017-18"),
                                    day = rep(1, 1),
                                    absent_all = rep(0, 1),
                                    dist = rep("OUSD", 1)),
                  aes(xmin = flu_end[7],
                      xmax = 319,
                      ymin = 0,
                      ymax = .3),
                  alpha = 0.2,
                  fill = "grey",
                  color = "grey",
                  linetype = "blank") +
        
        # Red box: peak flu season (Aug - Sep.) - Add red bars for faceted plot based on peak week of flu season
        geom_rect(data = data.frame(schoolyr = c("2014-15", "2015-16", "2016-17"),
                                    day = rep(1, 3),
                                    absent_all = rep(0, 3),
                                    dist = rep("OUSD", 3)),
                  aes(xmin = peak_flu_school_yr$start[4:6],
                      xmax = (peak_flu_school_yr$start[4:6] + 7),
                      ymin = 0,
                      ymax = bar_y_max),
                  alpha = 0.2,
                  fill = "red",
                  color = "red",
                  linetype = "blank") +
        
        # 17-18 Red box: peak flu season (Aug - Sep.) - Add red bars for faceted plot based on peak week of flu season
        geom_rect(data = data.frame(schoolyr = c("2017-18"),
                                    day = rep(1, 1),
                                    absent_all = rep(0, 1),
                                    dist = rep("OUSD", 1)),
                  aes(xmin = peak_flu_school_yr$start[7],
                      xmax = (peak_flu_school_yr$start[7] + 7),
                      ymin = 0,
                      ymax = .3),
                  alpha = 0.2,
                  fill = "red",
                  color = "red",
                  linetype = "blank")
    }
    
    # Point version.
    if (type == "point") {
      p1 = p1 + geom_point(size = 0.9, na.rm = T) 
      p2 = p2 + geom_point(size = 0.9, na.rm = T) 
      
      # Line version.
    } else {
      p1 = p1 + geom_line(size = .6, show.legend = T)
      p2 = p2 + geom_line(size = .6, show.legend = T) 
    }
    
    p1 = p1 +   
      
      theme_minimal() +
      
      scale_x_continuous(
        #"School day, where 1 = August 1st", #limits = c(0, 366)
        breaks = month_breaks,
        labels = month_labels,
        minor_breaks = NULL,
        # Prevent ggplot from extending the x-axis outside the data bounds.
        expand = c(0, 0)) +
      
      # Customize legend labels.
      scale_color_manual("",
                         values = c("#2185c5","#ff9715"),
                         breaks = c("OUSD", "WCC"),
                         labels = c("Intervention District (OUSD)", "Comparison District (WCCUSD)")) +
      
      # Show a separate plot with each school year in its own row.
      facet_grid(schoolyr ~ ., scale = "free_y") +
      
      theme(legend.position   = "bottom",         
            legend.title      = element_blank(),
            legend.margin     = margin(t=2, r=2, b=2, l=2),
            legend.text       = element_text(size = 9), 
            strip.text.y      = element_text(size = 14),
            axis.text.x       = element_text(size = 14),
            axis.text.y       = element_text(size = 12),
            plot.title        = element_text(size = 0),
            legend.background = element_rect(fill = "#fafafa",
                                             color = "#bbbbbb")) 
    
    p2 = p2 +   
      
      theme_minimal() +
      
      scale_x_continuous(
        #"School day, where 1 = August 1st", #limits = c(0, 366)
        breaks = month_breaks,
        labels = month_labels,
        minor_breaks = NULL,
        # Prevent ggplot from extending the x-axis outside the data bounds.
        expand = c(0, 0)) +
      
      # Customize legend labels.
      scale_color_manual("",
                         values = c("#2185c5","#ff9715"),
                         breaks = c("OUSD", "WCC"),
                         labels = c("Intervention District (OUSD)", "Comparison District (WCCUSD)")) +
      
      # Show a separate plot with each school year in its own row.
      facet_grid(schoolyr ~ ., scale = "free_y") +
      
      theme(legend.position   = "bottom",         
            legend.title      = element_blank(),
            legend.margin     = margin(t=2, r=2, b=2, l=2),
            legend.text       = element_text(size = 9), 
            strip.text.y      = element_text(size = 14),
            axis.text.x       = element_text(size = 14),
            axis.text.y       = element_text(size = 12),
            plot.title        = element_text(size = 0),
            legend.background = element_rect(fill = "#fafafa",
                                             color = "#bbbbbb"))
    
    return(list(p1, p2))
  }
}

####### All absences ########

# Plot raw points all years
absence_all_point_plot = plot_daily_absence(data=daily_agg_prepped)
ggsave(paste0(fig_dir, "daily-absence-rates-all-points.png"), 
       plot = absence_all_point_plot, width = 16, height = 8)

# Plot raw points pre-program, post-program
absence_all_pre_post_program_point_plot = plot_daily_absence(data=daily_agg_prepped, sep = TRUE)
ggsave(paste0(fig_dir, "daily-absence-rates-all-points-pre-program.png"), 
       plot = absence_all_pre_post_program_point_plot[[1]], width = 16, height = 8)
ggsave(paste0(fig_dir, "daily-absence-rates-all-points-post-program.png"), 
       plot = absence_all_pre_post_program_point_plot[[2]], width = 16, height = 8)

# Line plot
absence_all_line_plot = plot_daily_absence(data=daily_agg_prepped, type = "line")
absence_all_line_plot
ggsave(paste0(fig_dir, "daily-absence-rates-all-line-peak.png"), 
       plot = absence_all_line_plot, width = 16, height = 8)

##### Illness-specific ######

# Plot raw points
absence_ill_point_plot = plot_daily_absence(data=daily_agg_prepped, result = "ill")
absence_ill_point_plot
ggsave(paste0(fig_dir, "daily-absence-rates-ill-points.png"), 
       plot = absence_ill_point_plot, width = 16, height = 8)

# Line plot
absence_ill_line_plot = plot_daily_absence(data=daily_agg_prepped, type = "line", result = "ill")
absence_ill_line_plot
ggsave(paste0(fig_dir, "daily-absence-rates-ill-line.png"), 
       plot = absence_ill_line_plot, width = 16, height = 8)

# Line plot trial
absence_ill_pre_post_program_line_plot = plot_daily_absence(data=daily_agg_prepped, type = "line", sep = TRUE)
ggsave(paste0(fig_dir, "daily-absence-rates-ill-line-pre-program.png"), 
       plot = absence_ill_pre_post_program_line_plot[[1]], width = 16, height = 8)
ggsave(paste0(fig_dir, "daily-absence-rates-ill-line-post-program.png"), 
       plot = absence_ill_pre_post_program_line_plot[[2]], width = 16, height = 8)



