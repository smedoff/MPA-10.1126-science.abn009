
#=============
# Title: Running Difference and Difference regressions - Continuous Distance
# Author: Sarah Medoff 
#=============

# The purpose of this script is run the diff in diff regressions 
# for the continuous distance specification.  To ensure our results do not suffer
# from selection of the arbitrary region-radii specifications, we run our analysis 
# using a continuous treatment variable, distance to MPA border. 
# The code will set the following parameters: data set used (data_set)
# region specifications (rr_specifications), outcome variable of interest,
# expansion date (expansion_date), if this should be applied to a 
# 'real' analysis or a 'placebo' test (analysis_type), and fixed effects 
# specifications we are interested in (fe_type). 
# The code will loop through each region radii specification, identify 
# the sets in each region of interest, produced a time series plot that compares 
# 'near' and 'far' CPUE, calculate the diff-in-diff regression, produce a regression 
# coefficient plot, produce the final latex regression table



rm(list = ls())

#---------------------------------------
# Setting Parameters 

  # Data set used 
  data_set <- "Observer Data"

  # Region-radii specifications
  rr_specifications <- rr_specification <- "cnt_dist"

  # Define the fishing boundary
  fishing_boundary_nm <- 600

  # Outcome variable
  outcome_var <- "standardized_CPUE"  # relative_CPUE, ln_CPUE, CPUE

  # Expansion date 
  expansion_date <- as.Date("2016-08-26")

  # Real Analysis, Placebo Test, Sensitivity Analysis? 
  analysis_type <- "real"

  # Fixed Effects Specifications 
  fe_type <- c("noFE", "timeFE", "kitchenFE")

#---------------------
# Creating universal file name 

  source(file.path("hlpr_func", "hlprfx_creating_file_name.R"))
  file_name <- creating_file_name.f(data_set.arg = data_set,
                                    rr_specification.arg = rr_specification,
                                    outcome_var.arg = outcome_var,
                                    analysis.arg = analysis_type)
    

#---------------------
# Running the code 
  
  # Bounding the fishing grounds at fishing_boundary_nm 
  # Filter for >= "2010-01-01"
  # Filter for Deep set only 
  source(file.path("hlpr_func", 
                   "Regressions",
                   "06a_identifying_cnt_dist_sets_DIST.R"))
  
  # Running regressions
  # Scale DIST_NM by 1000
  source(file.path("hlpr_func", 
                   "Regressions",
                   "06c_regressions.R"))
    
  # Creating regression figures and data frames
  source(file.path("hlpr_func", 
                   "Regressions",
                   "06d_regression_figure_and_df.R"))
  
  # Creating regression latex tables
  source(file.path("hlpr_func", 
                   "Regressions",
                   "06e_tables.R"))

  





