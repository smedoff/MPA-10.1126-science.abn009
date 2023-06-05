
#=============
# Title: Running Difference and Difference regressions - Region Radii
# Author: Sarah Medoff 
#=============

# The purpose of this script is run the diff in diff regressions 
# for each region-radii specification: 100NM, 200NM, 300NM
# The code will set the following parameters: data set used (data_set)
# region specifications (rr_specifications), outcome variable of interest,
# expansion date (expansion_date), if this should be applied to a 
# 'real' analysis or a 'placbo' test (analysis_type), and fixed effects 
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
  
  # Region specifications
  rr_specifications <- paste(c(100, 200, 300), "NM", sep = "")
  
  # Outcome variable
  outcome_var <- "standardized_CPUE"  # relative_CPUE, ln_CPUE, CPUE
  
  # Expansion date 
  expansion_date <- as.Date("2016-08-26")
  
  # Real Analysis, Placebo Test, Sensitivity Analysis? 
  analysis_type <- "real"
  
  # Fixed Effects Specifications 
  fe_type <- c("noFE", "timeFE", "kitchenFE")

  

#---------------------------------------
# Loop through region radii specifications

 for(j in 1:length(rr_specifications)){
   
   rr_specification <- rr_specifications[j]
   
   # Creating universal file name 
   source(file.path("hlpr_func", "hlprfx_creating_file_name.R"))
   file_name <- creating_file_name.f(data_set.arg = data_set,
                                     rr_specification.arg = rr_specification,
                                     outcome_var.arg = outcome_var,
                                     analysis.arg = analysis_type)

  #---------------------
  # Running the code 
      
   # Identifying sets in each region (near and far)  
   # Filter for >= "2010-01-01"
   # Filter for Deep set only 
   source(file.path("hlpr_func", 
                    "Regressions",
                    "06a_identifying_region_sets_RR.R"))
   print(paste0("Done with 06a: ", file_name))
   
    # Create time trend plots of the difference between relative/standardized
    # CPUE for the 'near' and 'far' regions
    # Create a data frame of these differences
   source(file.path("hlpr_func", 
                    "Regressions",
                    "06b_time_trend_plots_donut_compare_RR.R"))
   print(paste0("Done with 06b: ", file_name))
    
  # Running regressions
  source(file.path("hlpr_func", 
                   "Regressions",
                   "06c_regressions.R"))
   print(paste0("Done with 06c: ", file_name))
    
  # Creating regression figures and data frames
  source(file.path("hlpr_func", 
                   "Regressions",
                   "06d_regression_figure_and_df.R"))
   print(paste0("Done with 06d: ", file_name))
  
  # Creating regression latex tables
  source(file.path("hlpr_func", 
                   "Regressions",
                   "06e_tables.R"))
   print(paste0("Done with 06e: ", file_name))

  } # end of donut specification loop
      

    




