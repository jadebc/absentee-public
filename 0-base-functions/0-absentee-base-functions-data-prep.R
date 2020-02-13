##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2017

# Base data prep functions for absentee analysis
# comment
##############################################
library(data.table)

# [TEMPLATE FOR NEW FUNCTIONS]
# Documentation: 
# Usage: 
# Description: 
# Args/Options: 
# Returns: 
# Output: ...
# [TEMPLATE FOR NEW FUNCTIONS]

##############################################
##############################################

##############################################
# Documentation: convert_data_to_subsetted_list
# Usage: convert_data_to_subsetted_list(data, subset_column, subsets, drop_levels)
# Description: Returns a list of subsets of the input tibble. 
# Args/Options:
#   data: a tibble to subset
#   subset_column: a string column name where the values to subset on reside
#   subsets: a list of values which will be used to individually subset with 
#   drop_levels: a boolean indicating whether levels should be dropped after subsets are made
# Returns: a named list of subsets of data, mapping a subset from subsets to the actual tibble representing the subset from data
# Output: ...

convert_data_to_subsetted_list = function(data, subset_column, subsets, drop_levels = FALSE) {
  assert_that(is.data.frame(data))
  assert_that(is_character(subset_column))
  assert_that(is_list(subsets))
  assert_that(subset_column %in% colnames(data))
  
  subsetted_list = list()
  for (subset in subsets) {
    filter_criteria = interp(~subset_column == subset,
                             .values = list(subset_column = as.name(subset_column), subset = subset))
    
    if (drop_levels) 
      subsetted_list[[subset]] = data %>% filter_(filter_criteria) %>% droplevels()
    else 
      subsetted_list[[subset]] = data %>% filter_(filter_criteria)
  }
  
  return(subsetted_list)
}

# Minimal Unit Test
test_data = tibble(a=c(1, 1, 2, 2, 3), b=6:10)
test_return_subsetted_list = convert_data_to_subsetted_list(data = test_data, subset_column = "a", subsets = list(1, 2, 3))

##############################################
# Documentation: absentee_adjusted_prep
# Usage: absentee_adjusted_prep(data)
# Description: Create datasets for adjusted analyses using weighted data
# Args/Options:
#   yearly_data a named list of yearly data to prep for tmle, of arbitrary length, ordered by year (oldest=>newest) 
#   covariates is a vector of strings for variable names included in the covariate adjustment set
#   parallel whether to paralellize calls to tmle.prep or not
#   start_year the year to which the name of the prepped datasets  are saved to (i.e. start_year=1 will save datasets starting with y1.ill and y1.all,
#   did whether the datasets being prepared are going to be used for difference-in-difference analysis
#   wted whether the datasets being prepared are going to be used for a weighted analysis
# Returns: a named list of prepared datasets
# Output: ...

absentee_adjusted_prep = function(yearly_data, covariates, parallel=FALSE, start_year=1, did=FALSE, wted=FALSE, peakwk=FALSE) {
  initialize_parallelization()
  
  for (year_data in yearly_data) {
    print(pryr::object_size(year_data))
  }
  
  #----------------------------------------------------
  # Create datasets for adjusted flu analyses
  #----------------------------------------------------
  # prep data for tmle
  # This requires about 4.5 GB of RAM per core.
  # These take about:
  
  prefix     = "y"
  year       = start_year
  suffix.ill = ".ill"
  suffix.all = ".all"
  
  prepped_datasets = list()
  
  for (year_data in yearly_data) {
    
    # - 14 minutes each when run sequentially.
    # - 4 minutes each with 5 cores.
    # - 2 minutes each with 20 cores.
    # Is the W screening even necessary, since we are specifying W manually below?
    # TODO: assert that year_data$did and year_data$pre
    
    cat("\nYear:", year, "Outcome: absent_ill\n")
    
    # Prep absentee_ill data for tmle, accounting for did and weighting
    tic()
    dynamic_year_ill_name = paste0(prefix, year, suffix.ill)
    if(did){
      
      dynamic_year_ill = tmle.prep(year_data, "absent_ill", covariates = covariates, parallel=parallel, wted=wted, peakwk=peakwk, did=TRUE) %>%
        mutate(did=year_data$did, pre=year_data$pre) %>%
        dplyr::select(absent_ill, tr, did, pre, everything())
      
    }else{
      if(peakwk){
        dynamic_year_ill = tmle.prep(year_data, "absent_ill", covariates = covariates, parallel=parallel, wted=wted, peakwk=peakwk)
        
      }else{
        dynamic_year_ill = tmle.prep(year_data, "absent_ill", covariates = covariates, parallel=parallel, wted=wted, peakwk=FALSE)
        
      }
      
    }
    
    prepped_datasets[[dynamic_year_ill_name]] = dynamic_year_ill
    toc()
    
    # Prep absentee_all data for tmle, accounting for did and weighting
    tic()
    dynamic_year_all_name = paste0(prefix, year, suffix.all)
    
    cat("\nYear:", year, "Outcome: absent_all\n")
    
    if(did){
      
      dynamic_year_all = tmle.prep(year_data, y = "absent_all", covariates = covariates, parallel=parallel ,wted=wted, peakwk=peakwk, did=TRUE) %>%
        mutate(did=year_data$did, pre=year_data$pre) %>%
        dplyr::select(absent_all, tr, did, pre, everything())
      
    }else{
      
      dynamic_year_all = tmle.prep(year_data, "absent_all", covariates = covariates, parallel=parallel, wted=wted, peakwk=peakwk) 
      
    }
    
    prepped_datasets[[dynamic_year_all_name]] = dynamic_year_all
    toc()
    
    # Review memory usage
    dynamic_year_ill %>% pryr::object_size() %>% print() # 1.5 GB
    dynamic_year_all %>% pryr::object_size() %>% print() # 1.5 GB
    
    # Move to the next year
    year = year + 1
    
    # Note: removing year_data after every iteration of the loop isn't feasible, unfortunately
    gc()
  }
  
  assert_that(length(prepped_datasets) == 2*length(yearly_data))
  
  return(prepped_datasets)
}

# Documentation: prep_for_plot
# Usage: 
# Description: Preps an arbitrary number of dataframes for a single call to plot.rd by combining and prepping them as needed
# Args/Options:
# data:               a list of lists of prepared fit dataframes
# newCols:            a list of named lists; the list at index i will have its name-value pairs added as columns and values to the datasets in the corresponding list at index i in data
# filterCondition:    a string (that would evaluate to a boolean if it were code, such as "TRUE", "FALSE", or "someColName > 3"), used to filter the rows within a list of prepared fit dataframes -- see its usage if you're still confused. Use "TRUE" if you want to include all rows.  Defaults to "est==RD"
# stratify_by:        a string (name of column to stratify on)
# Returns:  
# Output: 

prep_for_plot = function(data, newCols, filterCondition="est==RD", stratify_by = NULL) {
  assert_that(data %>% is.list &
                newCols %>% is.list &
                data %>% length == newCols %>% length)
  
  # The eventual return object
  combined_DF = data.frame()
  index = 1
  
  # Combine all prepared fits in data that are grouped together by lst 
  for (lst in data) {
    newCols_specific = newCols[[index]]
    
    assert_that(lst %>% is.list &
                  lst %>% length >= 1 &
                  lst[[1]] %>% is.data.frame)
    
    assert_that(newCols_specific %>% is.list &
                  newCols_specific %>% length >= 1 & 
                  newCols_specific %>% names %>% length == newCols_specific %>% length)
    
    lst_DF = bind_rows(lst) %>%
      filter_(filterCondition) %>%
      dplyr::select(rd=pt.est, lb=lb, ub=ub, Outcome, year, stratify_by)
    
    
    # Add columns-value pairs from newCols[[index]] to the combined datasets (lst_DF) from data[[index]] 
    for (name in newCols_specific %>% names) {
      lst_DF = lst_DF %>% mutate(!!name := newCols_specific[[name]])
    }
    
    combined_DF = rbind(combined_DF, lst_DF)
    index = index + 1
  }
  
  combined_DF = combined_DF %>% mutate(rd = as.numeric(rd),
                                       lb = as.numeric(lb),
                                       ub = as.numeric(ub),
                                       Outcome = as.factor(Outcome))
  
  return(combined_DF)
}


##############################################
# Documentation: make.ind
# function to create indicators for covariates that are factors

# input: dataframe with covariates including factors and
# other variable classes
# output: dataframe with covariates; all factors converted
# to indicators, other variable classes left as is
#----------------------------------------------------
make.ind=function(data){
  
  # which rows are factors
  # get names of columns that are factors
  factor.cols=matrix(0,length(colnames(data)),1)
  for(i in 1:length(colnames(data))){
    if(class(data[[i]])=="factor"){
      factor.cols[i,]=1 
    }
  }
  coldf=cbind(data.frame(names=colnames(data),factor=factor.cols))
  factor.names=as.character(coldf$names[coldf$factor==1])
  
  cat("Converting factors to indicators:", paste0(factor.names, collapse = ", "), "\n")
  
  if(length(factor.names)>0){
    x=colnames(data)[colnames(data) %in% factor.names]
    f.df=data[,..x]
    
    # make indicators and remove intercept column that model.matrix() adds.
    W=model.matrix(~., data=f.df)[,-1]
    
    return(list(W=W,factor.names=factor.names))
  }
  
}

##############################################
# Documentation: Wprescreen
# function to pre-screen adjustment
# covariates -- restrict to those
# with a LR test P<0.2

# inputs:
# Y = vector of outcome data
# Ws = data frame containing covariates
# family = "binomial" for binary outcomes and
# "gaussian" for continuous outcomes
# parallel = indicator for parallel processing (F=default)
# verbose = T if you want to print output to the log, F otherwise
# weight_var = string name of the numeric variable for each observation's weight

# output: list of covariates that are associated with the outcome (p-value<0.2)
# --------------------------------------
# TODO: use future instead of foreach.
Wprescreen <- function(Y, Ws, family, parallel = F, verbose = T,
                       obs_weight = NULL) {
  require(lmtest)
  # Y   : outcome variable of interest
  # Ws  : data frame of candidate covariates to screen
  dat <- data.frame(Ws, Y)
  dat <- dat[complete.cases(dat),] 
  nW <- ncol(Ws)
  LRp <- rep(NA,nW)
  
  # Detect any parallel backend that is registered.
  # Thanks to Jeremy Coyle's origami package for this approach.
  `%do_op%` = foreach::`%do%`
  # Use parallelism if there is a backend registered, unless parallel == F.
  if (foreach::getDoParRegistered() && parallel) {
    `%do_op%` = foreach::`%dopar%`
    if (verbose) cat("Parallel backend detected: using foreach parallelization.\n")
  } else {
    if (verbose) cat("No parallel backend detected. Operating sequentially.\n")
  }
  
  # Optionally run in parallel using foreach package.
  # Note that this will use much more ram when running in parallel.
  # for(i in 1:nW) {
  # May want to set mc.preschedule=F per
  # http://stackoverflow.com/questions/23231183/mclapply-long-vectors-not-supported-yet
  LRp = foreach::foreach(i = 1:nW, .combine = "c") %do_op% {
    dat$W <- dat[[i]]
    fit1 <- glm(Y~W, data = dat, family = family, weights = obs_weight)
    fit0 <- glm(Y~1, data = dat, family = family, weights = obs_weight)
    # LRp[i] <- lrtest(fit1,fit0)[2,5]
    LRp_i <- lmtest::lrtest(fit1,fit0)[2,5]
    # Return result to later be combined into a vector.
    LRp_i
  }
  # Convert foreach result from a list to a vector if needed.
  LRp = simplify2array(LRp)
  p20 <- ifelse(LRp<0.2,1,0)
  cat("\nLikelihood Ratio Test P-values:\n")
  print(cbind(names(Ws),paste("P =",sprintf("%1.3f",LRp))))
  cat("\n\nCovariates selected (P<0.20):\n")
  print(cbind(names(Ws)[p20==1],paste("P =",sprintf("%1.3f",LRp[p20==1]))))
  return(names(Ws)[p20==1])
}

#----------------------------------------------------
# Documentation: tmle.prep
# Prepare data for TMLE by subsetting to complete cases 
# (no missing data), subsetting to relevant variables, 
# creating indicators for covariates that are factors,
# selecting covariates associated with the outcome 

# inputs: 
# data = dataframe containing treatment variable, outcome
# variable, and covariates
# y = string for outcome name 
#   covariates is a vector of strings for variable names included in the covariate adjustment set
# parallel = indicator for parallel processing (F=default)

# output: data frame with treatment variable, outcome
# variable, and covariates ready for TMLE
#----------------------------------------------------
tmle.prep = function(data,y, covariates, peakwk=FALSE, parallel = F, wted=FALSE, did=FALSE){
  # Time how long this function takes to complete.
  
  data = as.data.table(data)
  
  if("race" %in% covariates) data = data[,race:= droplevels(race)]
  if("month" %in% covariates){
    data = data[,month:=as.factor(month)]
    data = data[,month:= droplevels(month)]
  }
  
  time_start = proc.time()
  
  # subset to complete cases
  data <- subset(data, complete.cases(data))
  
  if("race" %in% covariates & "grade" %in% covariates)   Ws=data[,.(race,grade)]
  Wsel = colnames(Ws)
  
  # create data frame with selected Ws ; include month 
  if("month" %in% covariates){
    df.Wsel=subset(data, select = c(Wsel,"month"))
  }else{
    df.Wsel=subset(data, select = c(Wsel))
  }

  # create indicators for categorical variables
  ind.out=make.ind(df.Wsel)
  
  # get names of continuous variables that were selected in prescreening
  cont.sel=Wsel[!Wsel %in% ind.out$factor.names]
  
  # create full data frame for analysis
  if(wted==FALSE){
    if(peakwk==FALSE){

      df.out=as.data.table(cbind(data[[y]],data$tr,data$schooln,ind.out$W))

      colnames(df.out)[1:3]=c(y,"tr","schooln")
    }else{

      df.out=as.data.table(cbind(data[[y]],data$tr,data$schooln, data[["peakwk"]],ind.out$W))
      colnames(df.out)[1:4]=c(y,"tr","schooln","peakwk")
    }
    
  }else{
    df.out=as.data.table(cbind(data[[y]],data$tr,data$schooln,data$weight,ind.out$W))
    colnames(df.out)[1:4]=c(y,"tr","schooln","weight")
  }
  
  # remove special characters from column names
  df.out = remove_special_characters(df.out)
  
  # Report execution time to assist with planning.
  time_end = proc.time()
  cat("\ntmle.prep execution time:\n")
  print(time_end - time_start)
  
  return(df.out)
}

#----------------------------------------------------
# Documentation: glm.did.prep
# Prepare data for GLM DID by subsetting to complete cases 
# (no missing data), subsetting to relevant variables, 
# creating indicators for covariates that are factors,
# selecting covariates associated with the outcome, 
# and creating DID indicator

# inputs: 
# data = dataframe containing treatment variable, outcome
# variable, and covariates
# y = string for outcome name 
# parallel = indicator for parallel processing (F=default)
# weight_var = string name of the numeric variable for each observation's weight

# output: data frame with treatment variable, outcome
# variable, and covariates ready for TMLE
#----------------------------------------------------
glm.did.prep=function(data,y,yr, parallel = F, weight_var = NULL) {
  # Time how long this function takes to complete.
  time_start = proc.time()
  
  # subset to complete cases
  data=data[complete.cases(data),]
  
  data$tr=ifelse(data[["dist"]]=="OUSD",1,0)
  data$did=ifelse(data$schoolyr==yr & data$dist=="OUSD",1,0)
  data$pre=ifelse(data$schoolyr!="2014-15" & data$schoolyr!="2015-16",1,0)
  
  # LR test to select covariates
  school.W=c("enrolled","mn.class.size","per.not_hsg","per.hsg", "per.some_col",
             "per.col_grad","per.grad_sch","per.englearn","per.freelunch","API13","API12",
             "mean.cst.ela","per.adv.ela","per.basic.ela","mean.cst.m","per.adv.m","per.basic.m")
  
  obs_weight = NULL
  if (!is.null(weight_var)) {
    obs_weight = data[[weight_var]]
  }
  
  Wsel = Wprescreen(Y = data[[y]], Ws = data[,c("race","grade",school.W)],
                    family = "binomial",
                    parallel = parallel,
                    obs_weight = obs_weight)
  # create data frame with Ws ; include month 
  df.Wsel=subset(data, select = c(Wsel,"month"))
  # create indicators for categorical variables
  df.ind=make.ind(df.Wsel)
  # create full data frame for analysis
  Wcont=c("enrolled", "mn.class.size", "per.not_hsg", "per.hsg", "per.some_col",
          "per.col_grad", "per.grad_sch", "per.englearn","per.freelunch", "API13",
          "API12", "mean.cst.ela", "per.adv.ela", "per.basic.ela", "mean.cst.m",
          "per.adv.m", "per.basic.m")
  df.out=cbind(data[[y]],data$matchid,data$tr,data$pre,data$did,df.ind,data[,Wcont])
  
  # Also add in weight variable if specified.
  if (!is.null(weight_var)) {
    df.out = cbind(df.out, data[[weight_var]])
    
    # Fix the column name for the weight variable.
    colnames(df.out)[ncol(df.out)] = weight_var
  }
  
  colnames(df.out)[1:5]=c(y,"matchid","tr","pre","did")
  
  # Report execution time to assist with planning.
  time_end = proc.time()
  cat("\nglm.did.prep execution time:\n")
  print(time_end - time_start)
  
  return(df.out)
}

# #----------------------------------------------------
# # Documentation: tmle.did.prep
# # Prepare data for TMLE - adjust for past absences
# #  by subsetting to complete cases 
# # (no missing data), subsetting to relevant variables, 
# # creating indicators for covariates that are factors,
# # selecting covariates associated with the outcome 
# 
# # inputs: 
# # data = dataframe containing treatment variable, outcome
# # variable, and covariates
# # y = string for outcome name 
# # yr = string for school year of analysis
# # parallel = indicator for parallel processing (F=default)
# 
# # output: data frame with treatment variable, outcome
# # variable, and covariates ready for TMLE including
# # mean of past absences by school year and school
# #----------------------------------------------------
# tmle.did.prep = function(data, y, yr, parallel = F) {
#   
#   cat("tmle.did.prep() for y =", y, "year =", yr, "\n")
#   
#   # drop school-level covariates
#   school.w = c( "enrolled", "mn.class.size", "per.not_hsg", 
#                 "per.hsg", "per.some_col", "per.col_grad", 
#                 "per.grad_sch", "per.englearn", "per.freelunch",
#                 "API13", "API12", "mean.cst.ela", "per.adv.ela",
#                 "per.basic.ela", "mean.cst.m" , "per.adv.m", "per.basic.m" )
#   
#   data=data[, (school.w):=NULL]  
# 
#   # get the mean of pre-program absences per week 
#   prog=data[schoolyr==yr,]
#   pre=data[data$schoolyr!="2014-15" & data$schoolyr!="2015-16",]
#   
#   pre$schoolyr=droplevels(pre$schoolyr)
# 
#   pre.agg=aggregate(pre[[y]], list(pre$dist,pre$schoolyr,pre$school),mean)
#   colnames(pre.agg)=c("dist","schoolyr","school","meany")
#   pre.agg.w <- reshape(pre.agg, timevar = "schoolyr", idvar = c("dist","school"),
#                direction = "wide")
#   
#   # Merger fields.
#   by_fields = c("dist","school")
#   
#   # Confirm that merger fields exist in prog.
#   if (mean(by_fields %in% colnames(prog)) != 1) {
#     bad_fields = by_fields[!by_fields %in% colnames(prog)]
#     cat("Warning: prog is missing these merge fields: ", bad_fields, "\n") 
#     cat("prog colnames:", paste(colnames(prog), collapse = ", "), "\n")
#   }
#   
#   # Confirm that merger fields exist in pre.agg.w.
#   if (mean(by_fields %in% colnames(pre.agg.w)) != 1) {
#     bad_fields = by_fields[!by_fields %in% colnames(pre.agg.w)]
#     cat("Warning: pre.agg.w is missing these merge fields: ", bad_fields, "\n") 
#     cat("pre.agg.w colnames:", paste(colnames(pre.agg.w), collapse = ", "), "\n")
#   }
#   
#   # merge in pre-program means as a covariate
#   prog=merge(prog,pre.agg.w,by = by_fields)
#   
#   prog$tr=ifelse(prog[["dist"]]=="OUSD",1,0)
#   
#   # LR test to select covariates
# 
#   Wsel=Wprescreen(Y=prog[[y]], Ws=prog[,c("race","grade")],family="binomial", parallel = parallel)
#   # create data frame with Ws ; include month 
#   df.Wsel=subset(prog, select = c(Wsel,"month"))
#   # create indicators for categorical variables
#   df.ind=make.ind(df.Wsel)
#   # create full data frame for analysis
#   Wcont=c("enrolled", "mn.class.size", "per.not_hsg", "per.hsg", "per.some_col",
#           "per.col_grad", "per.grad_sch", "per.englearn","per.freelunch", "API13",
#           "API12", "mean.cst.ela", "per.adv.ela", "per.basic.ela", "mean.cst.m",
#           "per.adv.m", "per.basic.m")
#   df.out=cbind(prog[[y]],prog$matchid,prog$tr,
#                prog[["meany.2011-12"]],prog[["meany.2012-13"]],prog[["meany.2013-14"]],
#                df.ind,prog[,Wcont])
#   colnames(df.out)[1:6]=c(y,"matchid","tr","y201112","y201213","y201314")
#   
#  preN=nrow(df.out)
#  # subset to complete cases
#  df.out=df.out[complete.cases(df.out),]
#  postN=nrow(df.out)
#  print(paste(preN-postN, " rows dropped because of no previous year absence data"))
#  # typically these missing weeks are due to spring break / holidays
#   
#  return(df.out)
# }

#----------------------------------------------------
# Documentation: sl.prep
# prepare data for Superlearner
# by subsetting to complete cases 
# (no missing data), subsetting to relevant variables, 
# creating indicators for covariates that are factors,
# selecting covariates associated with the outcome 

# inputs: 
# data = dataframe containing treatment variable, outcome
# variable, and covariates
# y = string for outcome name 
# parallel = indicator for parallel processing (F=default)
#----------------------------------------------------
sl.prep = function(data, y, parallel = F) {
  # subset to complete cases
  data <- subset(data, complete.cases(data))
  
  # LR test to select covariates - school level covariates only
  school.W=data[,.(enrolled,mn.class.size,per.not_hsg,per.hsg, per.some_col,
                   per.col_grad,per.grad_sch,per.englearn,per.freelunch,API13,API12,
                   mean.cst.ela,per.adv.ela,per.basic.ela,mean.cst.m,per.adv.m,per.basic.m)]
  
  Wsel=Wprescreen(Y=data[[y]], Ws=school.W,family="gaussian", parallel = parallel)
  
  # create data frame with Ws  
  A = as.data.frame(subset(data, select = Wsel))
  
  # remove special characters from column names
  A = remove_special_characters(A)
  
  return(list(Y=data[[y]],A=A))
}

#----------------------------------------------------
# Documentation: sl.prep.p3
# prepare data for Superlearner - parameter 3
# by subsetting to complete cases 
# (no missing data), subsetting to relevant variables, 
# creating indicators for covariates that are factors
# for parameter 3, including all covariates 

# inputs: 
# data = dataframe containing treatment variable, outcome
# variable, and covariates
# y = string for outcome name 
# parallel = indicator for parallel processing (F=default)
#----------------------------------------------------
sl.prep.p3 = function(data, y, parallel = F) {
  
  # subset to complete cases
  data <- subset(data, complete.cases(data))
  
  # create data frame with Ws  
  A = data %>% ungroup() 
  A = A %>% dplyr::select(-c(schoolyr,school,absent_all,absent_ill))
  
  # remove special characters from column names
  A = remove_special_characters(A)
  
  return(list(Y=data[[y]],A=as.data.frame(A)))
}
