
#=============
# Title: Running Difference and Difference regressions - Time Placebo
# Author: Sarah Medoff 
#=============

# The purpose of this script is run the diff in diff regressions 
# for the time placebo robustness check.  Timing of the monument expansion could 
# coincide with some unobserved factors that naturally provided 
# optimal environmental conditions for fishing productivity.  This would cause 
# our estiamtes to be biased.  To test for this source of endogeniety, we look at 
# Phytoplanton size in 2012 (4 years prior to the MPA expansion) and find a separate year in 
# which Phytoplanton size was roughly the same, this was 2006.  We then assign a 
# placebo expansion date to be 2010 and run our time placebo robustness check.  


rm(list = ls())

# ===============================
# Running Placebo - Region Radii 
# ===============================
# Setting Parameters 

  # Real Analysis, Placebo Test, Sensitivity Analysis? 
  analysis_type <- "placeboTIME"
  
  # Placebo expansion date 
  expansion_date <- as.Date("2010-08-26")
  
  # Fixed Effects Specifications 
  fe_type <- c("timeFE")

  #-----
  # Data set used 
  data_set <- "Observer Data" 
  
  # Region specifications
  rr_specifications <- paste(c(100, 200, 300), "NM", sep = "")
  
  # Outcome variable
  outcome_var <- "standardized_CPUE"  # relative_CPUE, ln_CPUE, CPUE
  



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
  
  # Create time trend plots of the difference between relative/standardized
  # CPUE for the 'near' and 'far' regions
  # Create a data frame of these differences
  source(file.path("hlpr_func", 
                   "Regressions",
                   "06b_time_trend_plots_donut_compare_RR.R"))
  
  # Running regressions
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
  
} # end of donut specification loop


  
  

# ===============================
# Running Placebo - Continous Distance 
# ===============================

  # Define the fishing boundary
  fishing_boundary_nm <- 600
  
  # Region-radii specifications
  rr_specifications <- rr_specification <- "cnt_dist"
  
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
  
  
  
  
  
  
  
