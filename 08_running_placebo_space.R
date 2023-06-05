
#=============
# Title: Running Difference and Difference regressions - Space Placebo
# Author: Sarah Medoff 
#=============

# The purpose of this script is run the diff in diff regressions 
# for the space placebo robustness check.  To test our results are indeed estimating 
# the spillover and not some other factor that would influence our 'near' region 
# differently from our 'far' region, we shift the MPA border north, south, east, and west.


rm(list = ls())

# ===============================
# Running Placebo - Region Radii 
# ===============================
# Setting Parameters 
  
  # Real Analysis, Placebo Test, Sensitivity Analysis? 
  analysis_type <- "placeboSPACE"
  
  # Monument Shifting Parameters
  space_shifters <- c("N", "S", "E", "W")
  space_shift_deg <- 10   #This should match the shift in 01.R
  
  # Region specifications
  rr_specifications <- paste(100, "NM", sep = "")
  
  # Fixed Effects Specifications 
  fe_type <- c("timeFE")
  

  #-------
  # Data set used 
  data_set <- "Observer Data" 
  
  # Outcome variable
  outcome_var <- "standardized_CPUE"  # relative_CPUE, ln_CPUE, CPUE
  
  # Placebo expansion date 
  expansion_date <- as.Date("2016-08-26")
  

#---------------------------------------
# Running the code 
  
# Loop through the space shifters
for(s in 1:length(space_shifters)){
  
  space_shift <- space_shifters[s]
  
  # Loop through region radii specifications
  for(j in 1:length(rr_specifications)){
    
    rr_specification <- rr_specifications[j]
    
    # Creating universal file name 
    source(file.path("hlpr_func", "hlprfx_creating_file_name.R"))
    file_name <- creating_file_name.f(data_set.arg = data_set,
                                      rr_specification.arg = rr_specification,
                                      outcome_var.arg = outcome_var,
                                      analysis.arg = analysis_type)
    
    file_name <- paste0(file_name, "_", space_shift)
    
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
  
  
}

  
  