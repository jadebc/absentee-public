##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2017

# Base data analysis functions for absentee analysis
# comment
##############################################

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
# Documentation: abs_glm
# Usage: abs_glm(yearly_data, outcome, treatment, adj, did start_year, family)
# Description: fits glm models for a specific outcome (i.e. all-cause of illness-specific absenteeism) over an arbitrary number of years
# Args/Options:   yearly_data (a list of yearly data to fit with glm, of arbitrary length, ordered by year (oldest=>newest)),
# outcome:        the outcome column name, as a string
# treatment:      the treatment column name, as a string
# adj:            whether yearly_data is for an adjusted analysis or not
# start_year:     the year to which the name of the prepped datasets are saved to (i.e. start_year=1 will return a list with names starting with y1.ill.res and y1.all.res)
# family:         the glm family to use, defaulting to "gaussian"
# Returns: a dataframe of the formatted glm fits OR a list of dataframes containing the formatted glm fits and unformatted glm fits (seperately) if adj=TRUE
# Output: ...

#TODO: add in assertions
abs_glm = function(yearly_data, outcome, treatment, covariates=NULL, adj=FALSE, did=FALSE, start_year=1, family="gaussian") {
  
  year = start_year
  prepared_fits = list()
  
  for (year_data in yearly_data) {
    
    # if adjusted model, subset to covariates in covariate list
    if(adj) covariate_names = unlist(lapply(as.list(covariates), function(x) 
      colnames(year_data)[grep(x,colnames(year_data))]))
    
    if(adj) assert_that(!is.null(covariate_names))
    
    # subset data for adjusted vs unadjusted analysis vs did analysis
    if(adj & !did) year_model_data = year_data %>% dplyr::select(c(outcome, treatment, covariate_names))
    if(adj & did) year_model_data = year_data %>% dplyr::select(c(outcome, treatment, covariate_names, pre, did))
    if(!adj & !did) year_model_data = year_data %>% dplyr::select(c(outcome, treatment))
    if(!adj & did) year_model_data = year_data %>% dplyr::select(c(outcome, treatment, pre, did))
    
    dynamic_glm_name = paste0("glm", ".", "y", year) # i.e. "glm.y1" or "glm.y22"
    dynamic_res_name = paste0("y", year,".res") # i.e. "y1.res" or "y22.res"
    
    # Assigning the formula to be used for the analysis, based on the type of analysis being done
    glm_formula = case_when(
      !adj & !did ~ paste0(outcome, "~", treatment),         
      adj  & !did ~ paste0(outcome, "~", "tr+."),
      !adj & did  ~ paste0(outcome, "~", "tr+did+pre"), 
      adj  & did  ~ paste0(outcome, "~", "tr+did+pre+.")    
    ) %>% as.formula()  
    
    print(paste0("glm_formula: ", glm_formula))
    
    glm.outcome = glm(formula=glm_formula,
                      data=year_model_data, family=family)
    print(glm.outcome)
    
    vcovCL = sandwichSE(year_model_data, 
                        fm=glm.outcome,
                        cluster=year_data$schooln)
    
    rfit = coeftest(glm.outcome, 
                    vcovCL)
    
    res = format.glm(fit=glm.outcome,
                     rfit=rfit,
                     did=did,                
                     family=family)
    print(res)
    
    prepared_fits[[dynamic_res_name]] = cbind(res, year)
    
    year = year + 1
  }
  
  assert_that(length(prepared_fits) == length(yearly_data))

  prepared_fits_DF = convert_nested_list_to_DF(nested_list=prepared_fits)

  # Concatenating the outcome
  prepared_fits_DF = prepared_fits_DF %>% cbind(Outcome=outcome, Treatment=treatment)

  return(prepared_fits_DF)
  
  
}

##############################################
# Documentation: abs_ltmle
# Usage: abs_ltmle(yearly_wted_data, outcome, treatment, id, weight, SL.library, adj=FALSE, did=FALSE, start_year)
# Description: fits tmle models for a specific outcome (i.e. all-cause of illness-specific absenteeism) over an arbitrary number of years
# Args/Options:   yearly_wted_data (a list of yearly weighted data to fit with ltmle, of arbitrary length, ordered by year (oldest=>newest)),
# outcome:        the outcome column name, as a string
# treatment:      the treatment column name, as a string
# id:             the id column name for clustered SEs, as a string
# weight:         the weight column name, as a string
# SL.library:     a list of learners for SuperLearner to run in ltmle
# seed:           numeric seed set by user for ltmle
# adj:            whether yearly_data is for an adjusted analysis or not
# wgt_normalize:  whether obs weights should be normalized to sum to 1.
# start_year:     the year to which the name of the prepped datasets are saved to (i.e. start_year=1 will return a list with names starting with y1.ill.res and y1.all.res)
# Returns: a dataframe of the formatted ltmle fits OR a list of dataframes containing the formatted ltmle fits and unformatted ltmle fits (seperately) if adj=TRUE
# Output: ...

#TODO: add in assertions
abs_ltmle = function(yearly_wted_data, outcome, treatment, id, weight, SL.library, seed, adj=FALSE,
                     wgt_normalize = FALSE,
                     start_year=1) {
  
  year = start_year
  prepared_fits = list()
  prepared_raw_fits = list() # For adjusted - saving raw, unformatted glm fits
  
  # Enable ltmle to return the Q and g SuperLearner objects.
  # Unclear if this is truly working or not.
  ltmle_lib = list(Q = unlist(SL.library),
                   g = unlist(SL.library))
  ltmle_lib = unlist(SL.library)
  attr(ltmle_lib, "return.fit") = TRUE
  ltmle_lib
  
  for (year_data in yearly_wted_data) {
    
    weight_vec = year_data[[weight]]
    
    if (wgt_normalize) {
      # Normalize observation weights to sum to 1.
      weight_vec = weight_vec / sum(weight_vec)
    }
    
    # Adjusted model ----------------------------
    if (adj) {
      dynamic_tmle_name = paste0("tmle", ".", "y", year) 
      
      year_data_w = colnames(year_data)[!colnames(year_data) %in% c(outcome, treatment, weight, id)]
      
      # create data frame in order of W, A, Y
      model_data = year_data[,c(year_data_w, treatment, outcome)] 
      
      # Explicitly create our Qform formula to avoid an ltmle warning.
      Qform = c(Y = paste("Q.kplus1 ~", treatment, "+", paste(year_data_w, collapse = " + ")))
      # Rename formula to use the outcome name as specified in Ynodes
      names(Qform) = outcome
      
      set.seed(seed, "L'Ecuyer-CMRG")
      ltmle.fit =
        ltmle(Ynodes = outcome, Anodes = treatment,
              Lnodes = year_data_w, 
              Qform = Qform,
              gform = paste(treatment, "~", paste(year_data_w, collapse = " + ")),
              gbounds = c(0.025, 0.975),
              data = model_data,
              abar = list(1, 0), estimate.time = F,
              SL.library = SL.library, 
              id = year_data[[id]],
              observation.weights = weight_vec)
      
      print(summary(ltmle.fit))
      
      # Save model fit 
      prepared_raw_fits[[dynamic_tmle_name]] = ltmle.fit
      
      # Unadjusted model ----------------------------
    } else {
      dynamic_tmle_name = paste0("tmle", ".", "y", year) 
      
      set.seed(seed, "L'Ecuyer-CMRG")
      ltmle.fit =
        ltmle(Ynodes = outcome, Anodes = treatment,
              Lnodes = c(), 
              data = year_data[, c(treatment, outcome)],
              gform = paste0(treatment, " ~ 1"),
              gbounds = c(0.025, 0.975),
              abar = list(1, 0), estimate.time = FALSE,
              SL.library = SL.library, id = year_data[,id],
              observation.weights = year_data[[weight]])
      
      print(paste0("Year: " , year))
      print(summary(ltmle.fit))
      
      # Save model fit 
      prepared_raw_fits[[dynamic_tmle_name]] = ltmle.fit
    }
    
    dynamic_res_name = paste0("y", year,".res") # i.e. y1.res or y22.res
    res = format.ltmle(data = year_data, weight = weight_vec, 
                       out = ltmle.fit, family = "binomial", year=year,
                       wted = TRUE)
    
    prepared_fits[[dynamic_res_name]] = res
    
    year = year + 1
    
  }
  
  assert_that(length(prepared_fits) == length(yearly_wted_data))
  if (adj) assert_that(length(prepared_raw_fits) == length(yearly_wted_data))
  
  prepared_fits_DF = convert_nested_listoflists_to_DF(nested_list=prepared_fits)
  
  # Concatenating the outcome
  prepared_fits_DF = prepared_fits_DF %>% cbind(Outcome=outcome, Treatment=treatment)
  
  return(list(prepared_fits_DF=prepared_fits_DF, 
              prepared_raw_fits=prepared_raw_fits))
  
}


##############################################
# Documentation: abs_ltmle
# Usage: abs_ltmle(yearly_wted_data, outcome, treatment, id, weight, SL.library, adj=FALSE, did=FALSE, start_year)
# Description: fits tmle models for a specific outcome (i.e. all-cause of illness-specific absenteeism) over an arbitrary number of years
# Args/Options:   yearly_wted_data (a list of yearly weighted data to fit with ltmle, of arbitrary length, ordered by year (oldest=>newest)),
# outcome:        the outcome column name, as a string
# treatment:      the treatment column name, as a string
# id:             the id column name for clustered SEs, as a string
# weight:         the weight column name, as a string
# SL.library:     a list of learners for SuperLearner to run in ltmle
# seed:           numeric seed set by user for ltmle
# adj:            whether yearly_data is for an adjusted analysis or not
# start_year:     the year to which the name of the prepped datasets are saved to (i.e. start_year=1 will return a list with names starting with y1.ill.res and y1.all.res)
# Returns: a dataframe of the formatted ltmle fits OR a list of dataframes containing the formatted ltmle fits and unformatted ltmle fits (seperately) if adj=TRUE
# Output: ...

#TODO: add in assertions
abs_ltmle_unwted = function(yearly_data, outcome, treatment, id=NULL, SL.library,
                            seed, glm_only=FALSE, adj=FALSE, start_year=1) {
  
  year = start_year
  prepared_fits = list()
  prepared_raw_fits = list() # For adjusted - saving raw, unformatted glm fits
  
  # Enable ltmle to return the Q and g SuperLearner objects.
  # Unclear if this is truly working or not.
  ltmle_lib = list(Q = unlist(SL.library),
                   g = unlist(SL.library))
  ltmle_lib = unlist(SL.library)
  attr(ltmle_lib, "return.fit") = TRUE
  ltmle_lib
  
  for (year_data in yearly_data) {
    
    # Adjusted model ----------------------------
    if (adj) {
      dynamic_tmle_name = paste0("tmle", ".", "y", year) 
      
      year_data_w = colnames(year_data)[!colnames(year_data) %in% c(outcome, treatment, id)]
      
      # create data frame in order of W, A, Y
      model_data = year_data[,c(year_data_w, treatment, outcome)] 
      
      # Create regression formulas for tmle to use all covariates in a glm()
      # rather than a SuperLearner library.
      if (glm_only) {
        SL.library = NULL
      }
      
      # Explicitly create our Qform formula to avoid an ltmle warning.
      Qform = c(Y = paste("Q.kplus1 ~", treatment, "+", paste(year_data_w, collapse = " + ")))
      gform = paste(treatment, "~", paste(year_data_w, collapse = " + "))
      
      # Rename formula to use the outcome name as specified in Ynodes
      names(Qform) = outcome
      
      if(!is.null(id)){
        set.seed(seed, "L'Ecuyer-CMRG")
        ltmle.fit =
          ltmle(Ynodes = outcome, Anodes = treatment,
                Lnodes = year_data_w, 
                Qform = Qform,
                gform = gform,
                gbounds = c(0.025, 0.975),
                data = model_data,
                abar = list(1, 0), estimate.time = F,
                SL.library = SL.library, 
                id = year_data[[id]])
      }else{
        set.seed(seed, "L'Ecuyer-CMRG")
        ltmle.fit =
          ltmle(Ynodes = outcome, Anodes = treatment,
                Lnodes = year_data_w, 
                Qform = Qform,
                gform = gform,
                gbounds = c(0.025, 0.975),
                data = model_data,
                abar = list(1, 0), estimate.time = F,
                SL.library = SL.library)
      }
      
      print(summary(ltmle.fit))
      
      # Save model fit 
      prepared_raw_fits[[dynamic_tmle_name]] = ltmle.fit
      
      # Unadjusted model ----------------------------
    } else {
      dynamic_tmle_name = paste0("tmle", ".", "y", year) 
      
      set.seed(seed, "L'Ecuyer-CMRG")
      ltmle.fit =
        ltmle(Ynodes = outcome, Anodes = treatment,
              Lnodes = c(), 
              data = year_data[, c(treatment, outcome)],
              gform = paste0(treatment, " ~ 1"),
              gbounds = c(0.025, 0.975),
              abar = list(1, 0), estimate.time = FALSE,
              SL.library = SL.library, id = year_data[,id])
      
      print(paste0("Year: " , year))
      print(summary(ltmle.fit))
      
      # Save model fit 
      prepared_raw_fits[[dynamic_tmle_name]] = ltmle.fit
    }
    
    dynamic_res_name = paste0("y", year,".res") # i.e. y1.res or y22.res
    res = format.ltmle(data = year_data, 
                       out = ltmle.fit, family = "binomial", year=year,
                       wted = FALSE)
    
    prepared_fits[[dynamic_res_name]] = res
    
    year = year + 1
    
  }
  
  assert_that(length(prepared_fits) == length(yearly_data))
  if (adj) assert_that(length(prepared_raw_fits) == length(yearly_data))
  
  prepared_fits_DF = convert_nested_listoflists_to_DF(nested_list=prepared_fits)
  
  # Concatenating the outcome
  prepared_fits_DF = prepared_fits_DF %>% cbind(Outcome=outcome, Treatment=treatment)
  
  return(list(prepared_fits_DF=prepared_fits_DF, 
              prepared_raw_fits=prepared_raw_fits))
  
}

##############################################
# Documentation: abs_tmle
# Usage: abs_tmle(yearly_wted_data, outcome, treatment, id, weight, SL.library, adj=FALSE, did=FALSE, start_year)
# Description: fits tmle models for a specific outcome (i.e. all-cause of illness-specific absenteeism) over an arbitrary number of years
# Args/Options:   
# yearly_wted_data: a list of yearly weighted data to fit with ltmle, of arbitrary length, ordered by year (oldest=>newest)),
# outcome:          the outcome column name, as a string
# treatment:        the treatment column name, as a string
# id:               the id column name for clustered SEs, as a string
# weight:           the weight column name, as a string
# SL.library:       a list of learners for SuperLearner to run in ltmle
# seed:             numeric seed set by user for ltmle
# adj:              whether yearly_data is for an adjusted analysis or not
#' glm_only:        if TRUE, skip SuperLearner and directly run logistic regression (glm)
#'                  to model outcome and propensity score. Not yet implemented for unadjusted analysis
#'                  because it should give exactly equivalent results to the SuperLearner.
# start_year:       the year to which the name of the prepped datasets are saved to (i.e. start_year=1 will return a list with names starting with y1.ill.res and y1.all.res)
# Returns: a dataframe of the formatted ltmle fits OR a list of dataframes containing the formatted ltmle fits and unformatted ltmle fits (seperately) if adj=TRUE
# Output: ...

#TODO: add in assertions
abs_tmle = function(yearly_data, outcome, treatment, id=NULL, SL.library, seed, adj=FALSE,
                    glm_only = FALSE, start_year=1) {
  
  year = start_year
  prepared_fits = list()
  prepared_raw_fits = list() # For adjusted - saving raw, unformatted glm fits
  
  for (year_data in yearly_data) {
    
    Qform_glm = NULL
    gform_glm = NULL
    
    # Adjusted model ----------------------------
    if (adj) {
      dynamic_tmle_name = paste0("tmle", ".", "y", year) 
      if (!exists("run_tmle")) run_tmle = tmle::tmle
      
      # create covariate data frame
      year_data_w=year_data %>% select(-c(outcome, treatment, schooln))
      
      # Create regression formulas for tmle to use all covariates in a glm()
      # rather than a SuperLearner library.
      if (glm_only) {
        Qform_glm = as.formula(paste("Y ~ A +", paste(names(year_data_w), collapse = " + ")))
        gform_glm = as.formula(paste("A ~", paste(names(year_data_w), collapse = " + ")))
        SL.library = NULL
      }
      
      if(!is.null(id)){
        set.seed(seed, "L'Ecuyer-CMRG")
        tmle.fit=run_tmle(Y=year_data[[outcome]], 
                          A=year_data[[treatment]],
                          W=year_data_w, 
                          family="binomial", 
                          verbose=TRUE, 
                          g.SL.library=SL.library,
                          Q.SL.library=SL.library,
                          Qform = Qform_glm,
                          gform = gform_glm,
                          id=year_data[[id]])
      }else{
        set.seed(seed, "L'Ecuyer-CMRG")
        tmle.fit=run_tmle(Y=year_data[[outcome]], 
                          A=year_data[[treatment]],
                          W=year_data_w, 
                          family="binomial", 
                          verbose=TRUE, 
                          g.SL.library=SL.library,
                          Q.SL.library=SL.library,
                          Qform = Qform_glm,
                          gform = gform_glm)
      }
      
      
      cat("Year:", year, "\n")
      print(summary(tmle.fit))
      
      # Save model fit 
      prepared_raw_fits[[dynamic_tmle_name]] = tmle.fit
      
      # Unadjusted model ----------------------------
    } else {
      dynamic_tmle_name = paste0("tmle", ".", "y", year) 
      
      # create covariate data frame
      year_data_w = year_data %>% mutate(W=1) %>% select(W)
      
      if(!is.null(id)){
        set.seed(seed, "L'Ecuyer-CMRG")
        tmle.fit = tmle(Y = year_data[[outcome]], 
                        A = year_data[[treatment]], 
                        W = year_data_w,
                        family = "binomial", 
                        verbose = TRUE, 
                        g.SL.library = SL.library, 
                        Q.SL.library = SL.library, 
                        id = year_data[[id]])
      }else{
        set.seed(seed, "L'Ecuyer-CMRG")
        tmle.fit = tmle(Y = year_data[[outcome]], 
                        A = year_data[[treatment]], 
                        W = year_data_w,
                        family = "binomial", 
                        verbose = TRUE, 
                        g.SL.library = SL.library, 
                        Q.SL.library = SL.library)
      }
      
      
      print(paste0("Year: " , year))
      print(summary(tmle.fit))
      
      # Save model fit 
      prepared_raw_fits[[dynamic_tmle_name]] = tmle.fit
    }
    
    dynamic_res_name = paste0("y", year,".res") # i.e. y1.res or y22.res
    res = format.tmle(data = year_data, out = tmle.fit, family = "binomial", year = year)
    
    prepared_fits[[dynamic_res_name]] = res
    
    year = year + 1
    
  }
  
  assert_that(length(prepared_fits) == length(yearly_data))
  if (adj) assert_that(length(prepared_raw_fits) == length(yearly_data))
  
  prepared_fits_DF = convert_nested_listoflists_to_DF(nested_list=prepared_fits)
  
  # Concatenating the outcome
  prepared_fits_DF = prepared_fits_DF %>% cbind(Outcome=outcome, Treatment=treatment)
  
  return(list(prepared_fits_DF=prepared_fits_DF, 
              prepared_raw_fits=prepared_raw_fits))
  
}

##############################################
# Documentation: abs_tmle3
# Usage: abs_tmle(yearly_wted_data, outcome, treatment, id, weight, SL.library, adj=FALSE, did=FALSE, start_year)
# Description: fits tmle models for a specific outcome (i.e. all-cause of illness-specific absenteeism) over an arbitrary number of years
# Uses new tmle3 package. To install: devtools::install_github("tlverse/tmle3")
# Args/Options:   
# yearly_wted_data: a list of yearly weighted data to fit with ltmle, of arbitrary length, ordered by year (oldest=>newest)),
# outcome:          the outcome column name, as a string
# treatment:        the treatment column name, as a string
# id:               the id column name for clustered SEs, as a string
# weight:           the weight column name, as a string
# SL.library:       a list of learners for SuperLearner to run in ltmle
# seed:             numeric seed set by user for ltmle
# adj:              whether yearly_data is for an adjusted analysis or not
# start_year:       the year to which the name of the prepped datasets are saved to (i.e. start_year=1 will return a list with names starting with y1.ill.res and y1.all.res)
# Returns: a dataframe of the formatted ltmle fits OR a list of dataframes containing the formatted ltmle fits and unformatted ltmle fits (seperately) if adj=TRUE
# Output: ...

#TODO: add in assertions
abs_tmle3 = function(yearly_data, outcome, treatment, id, SL.library, seed, adj=FALSE,
                     start_year=1) {
  
  year = start_year
  prepared_fits = list()
  prepared_raw_fits = list() # For adjusted - saving raw, unformatted glm fits
  
  for (year_data in yearly_data) {
    
    # Adjusted model ----------------------------
    if (adj) {
      dynamic_tmle_name = paste0("tmle", ".", "y", year) 
      
      # create covariate data frame
      # year_data_w = year_data %>% select(-c(outcome, treatment, schooln))
      
      set.seed(seed, "L'Ecuyer-CMRG")
      
      cluster_name = "schooln"
      
      
      # tmle3 code adapted from:
      # https://tmle3.tlverse.org/articles/framework.html
      nodes <- list(W = setdiff(colnames(year_data),
                                c(outcome, treatment, cluster_name)),
                    A = treatment,
                    Y = outcome)
      
      #       lrnr_glm_fast <- sl3::make_learner(sl3::Lrnr_glm_fast)
      lrnr_glm <- sl3::make_learner(sl3::Lrnr_glm)
      lrnr_mean <- sl3::make_learner(sl3::Lrnr_mean)
      #learner_list <- list(Y = lrnr_mean, A = lrnr_glm_fast)
      learner_list <- list(Y = lrnr_glm, A = lrnr_glm)
      
      # make a new copy to deal with data.table weirdness
      dt_data <- data.table(year_data)
      
      tmle_fit <- tmle3(tmle_TSM_all(), dt_data, nodes, learner_list)
      
      #tmle.fit=run_tmle(Y=year_data[[outcome]], 
      #                  A=year_data[[treatment]],
      #                  W=year_data_w, 
      #                  family="binomial", 
      #                  verbose=TRUE, 
      #                  g.SL.library=SL.library,
      #                  Q.SL.library=SL.library,
      #                  Qform = Qform_glm,
      #                  gform = gform_glm,
      #                  id=year_data[[id]])
      
      cat("Year:", year, "\n")
      print(tmle_fit)
      
      # Save model fit 
      prepared_raw_fits[[dynamic_tmle_name]] = tmle_fit
      
      # Unadjusted model ----------------------------
    } else {
      
      
      stop("unadjusted not yet implemented for tmle3")
      
      
      dynamic_tmle_name = paste0("tmle", ".", "y", year) 
      
      # create covariate data frame
      year_data_w = year_data %>% mutate(W=1) %>% select(W)
      
      set.seed(seed, "L'Ecuyer-CMRG")
      
      print(paste0("Year: " , year))
      print(summary(tmle.fit))
      
      # Save model fit 
      prepared_raw_fits[[dynamic_tmle_name]] = tmle.fit
    }
    
    
    # **************************************
    # TODO: implement format.tmle for tmle3 
    # **************************************
    
    dynamic_res_name = paste0("y", year,".res") # i.e. y1.res or y22.res
    #res = format.tmle(data = year_data, out = tmle.fit, family = "binomial", year = year)
    res = list()
    
    prepared_fits[[dynamic_res_name]] = res
    
    year = year + 1
    
  }
  
  assert_that(length(prepared_fits) == length(yearly_data))
  if (adj) assert_that(length(prepared_raw_fits) == length(yearly_data))
  
  prepared_fits_DF = convert_nested_listoflists_to_DF(nested_list=prepared_fits)
  
  # Concatenating the outcome
  # **************************************
  # TODO: fix for tmle3 ******************
  # **************************************
  prepared_fits_DF = prepared_fits_DF %>% cbind(Outcome=outcome, Treatment=treatment)
  
  return(list(prepared_fits_DF=prepared_fits_DF, 
              prepared_raw_fits=prepared_raw_fits))
  
}


#----------------------------------------------------
# Documentation: slpred
# Get predicted value from SuperLearner fit for each
# value of school participation 
# inputs: 
# slfit = saved object from SuperLearner fit
# data = data used in SuperLearner

# output: predicted probability of the outcome at 
# each value of school participation in Shoo the Flu
#----------------------------------------------------
slpred = function(slfit, data) {
  newdat = data$A
  As <- unique(data$A$participation)
  pY <- rep(NA, length(As))
  for (i in 1:length(As)) {
    newdat$participation <- As[i]
    pYs <- predict(slfit, newdata = newdat)$pred
    pY[i] <- mean(pYs)
  }
  return(pY)
}

# CK version for comparison.
# This version does not set all observations to the participation level, but
# instead just predicts each observation at its observed participation level.
# It is too variant as-is, and would benefit from kernel smoothing (e.g. loess)
# of the resulting predictions.
slpred2 = function(slfit, data) {
  
  preds <- predict(slfit, newdata = data$A)$pred
  
  pY = sapply(unique(data$A$participation), function(participation_val) {
    # Calculate the mean predicted value for that level of participation.
    mean(preds[data$A$participation == participation_val])  
  })
  
  return(pY)
}


#----------------------------------------------------
# Documentation: sandwichSE
# Get robust standard errors from GLM

# inputs:
# dat = data used to fit GLM model
# fm = fit from GLM model
# cluster = vector of cluster ids

# outputs: variance covariance matrix adjusted for 
# clustering 
#----------------------------------------------------
sandwichSE=function(dat, fm, cluster, weights = 1){
  if (!require(sandwich, quietly = TRUE)) {
    stop("sandwich library could not be loaded")
  }
  if (!require(lmtest, quietly = TRUE)) {
    stop("lmtest library could not be loaded")
  }
  # Number of clusters.
  M <- length(unique(cluster))
  # Number of observations.
  N <- max(length(cluster), sum(weights))
  K <- fm$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  # TODO: this seems to not yet incorporate observation weights.
  uj <- apply(estfun(fm), 2, function(x) tapply(x, cluster, 
                                                sum))
  vcovCL <- dfc * sandwich(fm, meat = crossprod(uj)/N)
  return(vcovCL)
}


#----------------------------------------------------
# Documentation: tmle.did
# True DID function for TMLE
# inputs: tmle output for pre and post program analyses
# output: DID estimate with influence curve based SE and CI
#----------------------------------------------------
tmle.did = function(tmle_post, tmle_pre) {
  est = tmle_post$beta[["A"]] - summary(tmle_pre)$effect.measures$ATE$estimate
  ic = tmle_post$IC - tmle_pre$IC
  n = nrow(tmle_post$IC)
  se = sd(ic) / sqrt(n)
  ci = est + c(-1, 1) * qnorm(0.975) * se
  
  result = list(est = est,
                se = se,
                ci_lower = ci[1],
                ci_upper = ci[2],
                n = n)
}


#----------------------------------------------------
# Documentation: view_pscore
# Usage: view_pscore(data, y, drops=NULL)
# Description: Removes the outcome variable (y), school number,
# and any other variables indicated in drops from data, then estimates
# the propensity score using tr as the dependent variable and
# all other variables as independent variables. 
# Args/Options: 
# data = dataset
# y = string for outcome column name
# drops = optional vector of strings of column names to drops
# Output: histogram of propensity score and summary of 
# glm fit 
#----------------------------------------------------

view_pscore = function(data, y, fig_filename, drops=NULL){
  # if(is.null(drops)){
  #   data = data %>% select( - c(y, schooln))
  #   
  # }else{
  #   data = data %>% select( - c(y, schooln, drops))
  # }
  drop.cols = c(y, "schooln")
  if(!is.null(drops)) drop.cols =  c(drop.cols, drops)
  
  keep.cols = c(colnames(data)[!colnames(data) %in% drop.cols])
  data = data[, ..keep.cols]
  
  glm.fit = glm(tr ~ .,data = data, family="binomial")
  pred = data.frame(pred=predict(glm.fit, type="response"))
  
  g = ggplot(pred, aes(x=pred))+geom_histogram()
  if(!is.null(fig_filename))  ggsave(g, filename = paste0(fig_dir, fig_filename))
  
  print(summary(glm.fit))
}


#----------------------------------------------------
# Documentation: run_stepwise
# Usage: run_stepwise = function(data, y, drops=NULL)
# Description: Removes the outcome variable (y), school number,
# and any other variables indicated in drops from data, then estimates
# the propensity score using tr as the dependent variable and
# all other variables as independent variables. 
# Args/Options: 
# data = dataset
# y = string for outcome column name
# drops = optional vector of strings of column names to drops
# Output: summary of stepwise regression
# Returns: list of covariates selected by stepwise regression
#----------------------------------------------------

run_stepwise = function(data, y, drops=NULL){
  
  data = as.data.table(data)
  drop.cols =  c("schooln", "schoolyr","dist.n", "matchid", 
                 "matchidn", "flusesn", "fluseasCDC","fluseasCDPH",
                 "absent_ill","absent_all","school",
                 "week","peakwk","yr","date","dist","month")
  
  if(!is.null(drops)) drop.cols =  c(drop.cols, drops)
  
  keep.cols = c(colnames(data)[!colnames(data) %in% drop.cols],y)
  data = data[, ..keep.cols]
  
  print(paste("Number of coefficients in initial model:", ncol(data)))
  
  glm_formula = paste0(y, "~", ".") %>% as.formula()
  full.model <- glm(glm_formula, data = data)
  
  stepfit = stepAIC(full.model, direction = "both", 
                    trace = FALSE)
  print(summary(stepfit))
  
  selected_cols = names(stepfit$coefficients)[2:length(names(stepfit$coefficients))]
  
  print(paste("Number of coefficients after stepwiseAIC:", length(stepfit$coefficients)))
  
  return(selected_cols)
  
}

#----------------------------------------------------
# Documentation: stratify_data
# Usage: Stratifies a table
# Description: Filters a datatable to values that are set to 1 in a specified one-hot-encoded indicator column
# Args/Options: 
#     - data: a datatable, containing that column specified by strat_value
#     - strat_value: a string, name of the one-hot-encoded indicator column
# Returns: a table with rows that only have the value 1 in the strat_value column
# Output: a table
#----------------------------------------------------

stratify_data = function(data, strat_value){
  strat_tables = data
  
  for (year in names(data)){
    year_data = strat_tables[[year]]
    if (any(year_data[strat_value] == 1)){
      strat_tables[[year]] = year_data[year_data[strat_value] == 1,]
    } else {
      strat_tables[[year]] = NULL
    }
  }
  
  return(strat_tables)
} 

#----------------------------------------------------
# Documentation: stratified_fit_glm
# Usage: Fits a glm model on stratified data
# Description: Stratifies data using stratify_data, fits a glm model on each stratified table, and adds the output to a list of lists
# Args/Options: 
#     - glm_data: adatatable, the full data to be stratified
#     - absence_type: a string, either "ill" or "all"
#     - strat_by: a string, the variable that the table is being stratified by
# Returns: outputs for glm models that are fit on each stratified group
# Output: a list of list of glm outputs
#----------------------------------------------------
stratified_fit_glm = function(glm_data, covariates, absence_type, strat_by) {
  outcome_type = paste("absent_", absence_type, sep = "")
  
  keep = grep(strat_by, colnames(glm_data[[1]]))
  stratify_values = colnames(glm_data[[1]][,keep])
  
  strat_glm_list = list()
  list_index = 1
  
  for (strat_val in stratify_values){
    glm_strat_data = stratify_data(glm_data, strat_val)
    
    if (length(glm_strat_data) != 0) {
      glm_res = abs_glm(yearly_data=glm_strat_data,
                        outcome=outcome_type,
                        treatment="tr",
                        covariates = covariates,
                        adj=TRUE,
                        did=TRUE,
                        start_year=1,
                        family="gaussian")
      
      glm_res = glm_res[complete.cases(glm_res),]
      
      glm_res[strat_by] = substr(strat_val, nchar(strat_by) + 1, nchar(strat_val))
      
      strat_glm_list[[list_index]] = glm_res
      list_index = list_index + 1
    }
  }
  
  return(list(strat_glm_list))
}


##############################################
##############################################

# Documentation: check_sparsity
# Usage: check_sparsity(data, outcome, treatment, covariates)
# Description: check a data frame for positivity violations within each 
#                 treatment, outcome, covariate combination
#
# Args/Options:   
# data:           dataset to check for sparsity
# outcome:        the outcome column name, as a string
# treatment:      the treatment column name, as a string
# covariates:     covariate column name(s), as a string

# Returns: vector of covariates (as string) that pass all checks
# Output: prints rows of the data frame with conditional prevalence < 5%

check_sparsity = function(data, covariates, tolerance = 0.05){
  
  data = as.data.frame(data)
  
  assert_that(all(covariates %in% colnames(data)), 
              msg = "Some covariates are not in the data.")
  
  mean_cov = lapply(as.list(covariates), function(x) 
    mean(as.numeric(data[,x]), na.rm=TRUE))
  names(mean_cov) = covariates
  
  if(!all(mean_cov > tolerance)){
    print("Variables that have prevalence < 5% and were removed from covariates list:")
    print(names(which(mean_cov <= tolerance) ))
    covariates = covariates[-which(covariates %in% names(which(mean_cov <= tolerance)))]
    return(covariates)
  }
  
  if(all(mean_cov > tolerance)) return(covariates)
  
}


##############################################
##############################################

# Documentation: fit_tmle3
# Usage: fit_tmle3(data, outcome, outcome_type, treatment, treatment_type, vacc_strata, covariates, adjusted, tolerance)
# Description: fit tmle, estimate ATE
#
# Args/Options:   
# data:           data frame containing outcome, treatment, covariates
# outcome:        the outcome column name, as a string
# outcome_type:   indicates outcome type; "binomial" or "continuous"
# treatment:      the treatment column name, as a string
# treatment_type: indicates treatment type; "binomial" or "continuous"
# vacc_strata:    character indicating vaccination strata; "Pooled", "Vaccinated", or "Unvaccinated" 
# covariates:     character string containing covariate column names (Default: NULL)
# adjusted:       Boolean option for an adjusted model (Default: FALSE)
# tolerance:      allowable minimum prevalence of each covariate to be included in models
#
# Returns: tmle output
# Output: tmle output

fit_tmle3 = function(data, outcome, outcome_type, treatment, treatment_type, covariates, adjusted, clusterid, tolerance = 0.05){
  
  # sparsity check
  covariates_checked = check_sparsity(data = data, 
                                      covariates = covariates,
                                      tolerance = tolerance)
  
  if(adjusted){
    # define nodes
    node_list <- list(
      W = covariates_checked,
      A = treatment,
      Y = outcome,
      id = clusterid
    )
  }
  
  if(!adjusted){
    
    data = data %>% mutate(W=1)
    
    # define nodes
    node_list <- list(
      W = "W",
      A = treatment,
      Y = outcome,
      id = clusterid
    )
  }
  
  # process missing data
  processed <- process_missing(data, node_list)
  data_processed <- processed$data
  node_list <- processed$node_list
  
  # define TMLE specification
  ate_spec <- tmle_ATE(
    treatment_level = 1,
    control_level = 0
  )
  
  # choose base learners
  lrnr_glm <- make_learner(Lrnr_glm)
  lrnr_mean <- make_learner(Lrnr_mean)
  lrnr_glmnet <- make_learner(Lrnr_glmnet)
  lrnr_xgboost <- make_learner(Lrnr_xgboost, nrounds = 50, learning_rate = 0.01)
  
  lrnr_gam <- Lrnr_pkg_SuperLearner$new("SL.gam")
  lrnr_bayesglm <- Lrnr_pkg_SuperLearner$new("SL.bayesglm")
  lrnr_stepAIC <- Lrnr_pkg_SuperLearner$new("SL.stepAIC")
  
  # define metalearners appropriate to data types
  
  # continuous metalearner
  ls_metalearner <- make_learner(Lrnr_nnls)
  
  # binomial metalearner
  bi_metalearner <- make_learner(Lrnr_nnls)
  
  if(outcome_type == "binomial"){
    sl_Y <- Lrnr_sl$new(
      learners = list(lrnr_glm, lrnr_mean, lrnr_glmnet, lrnr_xgboost,
                      lrnr_gam, lrnr_bayesglm, lrnr_stepAIC),
      metalearner = bi_metalearner
    )
  }
  
  if(outcome_type == "continuous"){
    sl_Y <- Lrnr_sl$new(
      learners = list(lrnr_glm, lrnr_mean, lrnr_glmnet, lrnr_xgboost,
                      lrnr_gam, lrnr_bayesglm, lrnr_stepAIC),
      metalearner = ls_metalearner
    )
  }
  
  if(treatment_type == "binomial"){
    sl_A <- Lrnr_sl$new(
      learners = list(lrnr_glm, lrnr_mean, lrnr_glmnet, lrnr_xgboost,
                      lrnr_gam, lrnr_bayesglm, lrnr_stepAIC),
      metalearner = bi_metalearner
    )
  }
  
  if(treatment_type == "continuous"){
    sl_A <- Lrnr_sl$new(
      learners = list(lrnr_glm, lrnr_mean, lrnr_glmnet, lrnr_xgboost,
                      lrnr_gam, lrnr_bayesglm, lrnr_stepAIC),
      metalearner = ls_metalearner
    )
  }
  
  learner_list <- list(A = sl_A, Y = sl_Y)
  
  # fit TMLE
  tmle_fit <- tmle3(ate_spec, data_processed, node_list, learner_list)
  
  return(tmle_fit$summary)
}