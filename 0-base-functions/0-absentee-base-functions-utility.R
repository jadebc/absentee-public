##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2017

# Base utility functions for absentee analysis
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
# NOTE: I just realized that dplyr::bind_rows does the exact same thing as this
# Documentation: convert_nested_list_to_DF
# Usage: convert_nested_list_to_DF(nested_list)
# Description: Converts a list of dataframes into a single dataframe
# Args/Options: 
# Returns: 
# Output: ...

convert_nested_list_to_DF = function(nested_list) {
  return_DF = data.frame()
  
  for (nested_DF in nested_list) {
    return_DF = return_DF %>% rbind(nested_DF)
  }
  
  return(return_DF)
}

##############################################
# Documentation: convert_nested_listoflists_to_DF
# Usage: convert_nested_listoflists_to_DF(nested_list)
# Description: Converts a list of lists into a single dataframe
# Args/Options: 
# Returns: a dataframe with the column names from the first 
# element in the list of lists
# Output: ...

convert_nested_listoflists_to_DF = function(nested_list) {
  # return_DF = data.frame()
  
  rrlist =list()
  rdlist =list()
  
  for(i in 1:length(nested_list)){
    rrlist[[i]]= as.vector(nested_list[[i]]$rr)
    rdlist[[i]]= as.vector(nested_list[[i]]$rd)
  }
  
  rr.df = suppressWarnings(bind_rows(rrlist))
  rd.df = suppressWarnings(bind_rows(rdlist))
  
  return_DF = rbind(rr.df, rd.df)
  
  return(return_DF)
}

##############################################
# Documentation: load_to_list
# Usage: load_to_list(data, sample_pct, seed)
# Description: Loads the contents of an RData to an environment and returns the environment as a list
# Args/Options: RData_path (a path to the RData_file, either relative to the working directory
# or as an absolute path)
# Returns: a named list corresponding to the named objects from the loaded RData file 
# Output: ...

load_to_list <- function(RData_path) {
  if (!file.exists(RData_path)) {
    stop(paste("Error, file does not exist:", RData_path))
  }
  env = new.env()
  load(RData_path, env)
  return(env %>% as.list()) 
}

##############################################
# Documentation: down_sample
# Usage: down_sample(data, sample_pct, seed)
# Description: down samples data according to a random seed
# Args/Options: data, sample_pct, seed
# Returns: A sample of the data
# Output: ...

down_sample = function(data, sample_pct=.01, seed=1) {
  
  assert_that(is.data.frame(data))
  assert_that(is.double(sample_pct) & 0 < sample_pct & sample_pct < 1)
  assert_that(is.double(seed))
  
  set.seed(seed)
  return(data[sample(nrow(data), floor(nrow(data) * sample_pct)),])
}

##############################################
# Documentation: compress_dataframe
# Usage: compress_dataframe(data, vars)
# Description: Create weighted version of dataframe that restricts to a single observation
# for each unique combination of covariates, but add a weight column that
# notes how many observations were collapsed into the de-duplicated dataframe.
# Args/Options: data, vars
# Returns: a de-duplicated, weighted tibble representing the same data 
# Output: Prints the compression rate (compression size vs uncompressed size) acheived

compress_dataframe = function(data, vars = colnames(data)) {
  
  compressed_data = data %>%
    group_by_at(vars) %>%               # Group by all columns.
    mutate(weight = n()) %>%            # Create a new weight columns.
    filter(row_number() == 1)           # Select first observation within group.
  
  print(cat("Compression of",
            deparse(substitute(data)),
            round((1 - nrow(compressed_data) / nrow(data)) * 100, 1), 
            "%\n"
  )
  )
  
  return(compressed_data)
}

##############################################
# Documentation: initialize_parallelization
# Usage: initialize_parallelization()
# Description: Checks an environment variable that can override the number of max_cores, which is 2 by default
# Args/Options: ...
# Returns: ...
# Output: Prints what the value of max_cores is

initialize_parallelization = function() {
  
  parallel_cores = Sys.getenv("R_PARALLEL_CORES")
  
  max_cores <<- ifelse(test=parallel_cores != "",
                       yes=as.numeric(parallel_cores),
                       no=2
  )
  
  cat("Max cores:", max_cores, "\n")
  
  invisible(NULL)
}

#----------------------------------------------------
# Documentation: remove_special_characters
# function to remove special characters from strings
# input: character vector
# output: character vector without periods or underscores
#----------------------------------------------------
remove_special_characters=function(obj, removeunderscore=FALSE){
  colobj=colnames(obj)
  rows_per=grep(".",colobj)
  rows_space=grep(" ",colobj)
  rows_apos=grep("'",colobj)
  
  newcol=colnames(obj)
  for(i in rows_per){
    newcol[i]=gsub("[.]","",colobj[i])
  }
  
  for(i in rows_space){
    newcol[i]=gsub(" ","",newcol[i])
  }
  
  for(i in rows_apos){
    newcol[i]=gsub("[']","",newcol[i])
  }
  
  if(removeunderscore==TRUE){
    rows_und=grep("_",colobj)
    for(i in rows_und){
      newcol[i]=gsub("_","",newcol[i])
    }
  }
  
  
  colnames(obj)=newcol
  return(obj)
}

#----------------------------------------------------
# Documentation: format.tmle
# Format TMLE results
# inputs:
# out = saved TMLE object
# family = "binomial" for binary outcomes, "gaussian"
# for continuous outcomes
# output: matrix of formatted TMLE results
#----------------------------------------------------
format.tmle=function(data, out, family, year=NULL){
  n = nrow(data)
  if(family=="binomial"){
    rr.res=matrix(NA,1,3)
    rr.res[1,1]=out$estimates$RR$psi
    rr.res[1,2]=out$estimates$RR$CI[1]
    rr.res[1,3]=out$estimates$RR$CI[2]
    
    if(!is.null(year)){
      rr.res=as.data.frame(cbind(n,rr.res,year,"RR"))
      colnames(rr.res)=c("n","pt.est","lb","ub","year","est") 
    }else{
      rr.res=as.data.frame(cbind(n,rr.res,"RR"))
      colnames(rr.res)=c("n","pt.est","lb","ub","est") 
    }
  }
  rd.res=matrix(NA,1,3)
  rd.res[1,1]=out$estimates$ATE$psi
  rd.res[1,2]=out$estimates$ATE$CI[1]
  rd.res[1,3]=out$estimates$ATE$CI[2]
  
  if(!is.null(year)){
    rd.res=as.data.frame(cbind(n,rd.res,year,"RD"))
    colnames(rd.res)=c("n","pt.est","lb","ub","year","est")  
  }
  
  if(family=="binomial"){
    return(list(rr=rr.res,rd=rd.res))
  }
  return(rd.res)
  
}

#----------------------------------------------------
# Documentation: format.ltmle
# Format LTMLE results, for weighted analysis.
# inputs:
# data = data frame fed into ltmle
# weight = vector of weights used in ltmle
# out = saved LTMLE object
# family = "binomial" for binary outcomes, "gaussian"
# for continuous outcomes
# output: matrix of formatted TMLE results
#----------------------------------------------------
format.ltmle = function(data, weight, out, family, year, wted=TRUE) {
  
  if(wted==TRUE){
    n = sum(weight)
  }else{
    n = nrow(data)
  }
  estimates = summary(out)$effect.measures
  
  if (family == "binomial") {
    # Relative risk estimates.
    rr.res = matrix(NA, 1, 3)
    rr.res[1, 1] = estimates$RR$estimate
    rr.res[1, 2] = estimates$RR$CI[1]
    rr.res[1, 3] = estimates$RR$CI[2]
    
    if(!is.null(year)){
      rr.res = as.data.frame(cbind(n,rr.res,year,"RR"))
      colnames(rr.res) = c("n","pt.est", "lb", "ub","year","est")
    }else{
      rr.res = as.data.frame(cbind(n,rr.res,"RR"))
      colnames(rr.res) = c("n","pt.est", "lb", "ub","est")
    }
  }
  
  # Risk difference estimates.
  rd.res = matrix(NA, 1, 3)
  rd.res[1, 1] = estimates$ATE$estimate
  rd.res[1, 2] = estimates$ATE$CI[1]
  rd.res[1, 3] = estimates$ATE$CI[2]
  
  if(!is.null(year)){
    rd.res = as.data.frame(cbind(n,rd.res,year,"RD"))
    colnames(rd.res) = c("n","pt.est", "lb", "ub","year","est")
  }else{
    rd.res = as.data.frame(cbind(n,rd.res,"RD"))
    colnames(rd.res) = c("n","pt.est", "lb", "ub","est")
  }
  
  
  if (family == "binomial") {
    return(list(rr = rr.res, rd = rd.res))
  }
  return(rd.res)
  
}



#----------------------------------------------------
# Documentation: format.glm
# Format GLM  results
# inputs: 
# rfit = fit from GLM model
# output: matrix with formatted results
#----------------------------------------------------
format.glm = function(fit, rfit, did=FALSE, family="gaussian"){
  
  row_index = ifelse(test=did,
                     yes=which(names(rfit[,1])=="did"),
                     no=2)
  
  pt.est = rfit[row_index,1]
  se     = rfit[row_index,2]
  
  lb     = pt.est-(qnorm(0.975)*se)
  ub     = pt.est+(qnorm(0.975)*se)
  n      = nrow(fit$data)
  
  out = cbind(n,pt.est,se,lb,ub)
  
  if (family=="binomial") out = exp(out)
  
  return(out)
}

#----------------------------------------------------
# Documentation: pt.est.ci.f
# format point estimate and ci
# obj -> object with three elements: ptest, lb, ub
# decimals -> how many decimal points to include in outputs
# scale -> multipication factor for outputs

pt.est.ci.f=function(obj,decimals,scale){
  a=sprintf(paste("%0.0",decimals,"f",sep=""),obj[1]*scale)
  b=sprintf(paste("%0.0",decimals,"f",sep=""),obj[2]*scale)
  c=sprintf(paste("%0.0",decimals,"f",sep=""),obj[3]*scale)
  return(paste(a," (",b,", ",c,")",sep=""))
}

# -------------------------------
# Documentation - pt.est.ci.table
#
# Usage: Format CIs from an inputted table/list of point estimates, lower bounds, and upper bounds
# Description: Applies pt.est.ci.f to individual rows of a table to allow for quick formatting of many CIs
# Args/Options: 
#       - table: a table containing the columns "pt.est", "lb", "ub"
#       - decimals: number of decimal places to include
#       - scale:
# Returns: Formatted CIs
# Output: A vector of strings in the form "pt.est (lb, ub)"
# -------------------------------

pt.est.ci.table = function(table, decimals, scale){
  cis = c("-")
  for (row_num in seq(1: nrow(table))) {
    cis = rbind(cis, pt.est.ci.f(table[row_num, c("pt.est", "lb", "ub")], decimals, scale))
  }
  return (cis)
}


#----------------------------------------------------
# Documentation: sum_nomiss
# Take the sum of non missing observations
# starting from column ind1 and ending with column ind2
# in data.frame data
#----------------------------------------------------
sum_nomiss=function(ind1, ind2, data){
  x=data[,c(ind1:ind2)]
  x[is.na(x)]=0
  return(rowSums(x))
}

#----------------------------------------------------
# Documentation: sum_nomiss
# ggplot plain black white theme
#----------------------------------------------------
theme_complete_bw <- function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.line =         element_blank(),
      axis.text.x =       element_text(size = base_size * 0.8 , lineheight = 0.9, colour = "black", vjust = 1, margin = margin(0.1,0.1,0.1,0.1,"cm")),
      axis.text.y =       element_text(size = base_size * 0.8, lineheight = 0.9, colour = "black", hjust = 1, margin = margin(0.1,0.1,0.1,0.1,"cm")),
      axis.ticks =        element_line(colour = "black"),
      axis.title.x =      element_text(size = base_size, vjust = 0.5),
      axis.title.y =      element_text(size = base_size, angle = 90, vjust = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      
      legend.background = element_rect(colour=NA), 
      legend.key =        element_rect(fill = NA, colour = "black", size = 0.25),
      legend.key.size =   unit(1.2, "lines"),
      legend.text =       element_text(size = base_size * 0.8),
      legend.title =      element_text(size = base_size * 0.8, face = "bold", hjust = 0),
      legend.position =   "right",
      
      panel.background = element_rect(fill = "white", colour = NA), 
      panel.border =     element_rect(fill = NA, colour = "grey50"), 
      panel.grid.major = element_line(colour = "grey80", size = 0.5, linetype="dashed"), 
      panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
      panel.spacing =     unit(0.5, "lines"),
      
      strip.background = element_rect(fill = NA, colour = NA), 
      strip.text.x =     element_text(colour = "black", size = base_size * 0.8, face="bold", 
                                      margin = ggplot2::margin(0.3,0.1,0.1,0.1,"cm")),
      strip.text.y =     element_text(colour = "black", size = base_size * 0.8, angle = -90),
      
      plot.background =  element_rect(colour = NA, fill = "white"),
      #plot.title =       element_text(size = base_size * 1.2),
      plot.margin =      unit(c(1, 1, 0.5, 0.5), "lines"))
}
