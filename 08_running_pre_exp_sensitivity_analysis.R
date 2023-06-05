
#=============
# Title: Running Difference and Difference regressions - Pre-expansion Sensitivity Check
# Author: Sarah Medoff 
#=============

# A critique of this analysis is the arbitrarily chosen "Pre-Expansion" start year.  In the 
# main specification we define our pre-expansion period as the observations that take place 
# between 2010-2016.  To check how robust our findings are to this pre-defined start date, 
# we incrementally extend our "pre-expansion" to start in 2009, 2008, 2007, and 2006.  
# This script will loop through the pre-expansion robustness specifications and then loop through the 
# region-radii specifications.  
# We only need a data frame that summarizes the regression outputs.  We do not need the 
# the time trend figures/data frame, the regression figures/data frames, or the latex tables 


rm(list = ls())

# ===============================
# Sensitivity Analysis - Region Radii 
# ===============================
# Setting Parameters 

  # Real Analysis, Placebo Test, Sensitivity Analysis? 
  analysis_type <- "sensitivityPRE"
  
  # Create a vector of "Pre-Expansion" start years
  pre_expansion_start_yr.v <- 2006:2010
  
  # Fixed Effects Specifications 
  fe_type <- "timeFE"
  
  # Region specifications
  rr_specifications <- paste(c(100, 200, 300), "NM", sep = "")
  
  

  #----------
  # Data set used 
  data_set <- "Observer Data"
  
  # Outcome variable
  outcome_var <- "standardized_CPUE"  # relative_CPUE, ln_CPUE, CPUE
  
  # Expansion date 
  expansion_date <- as.Date("2016-08-26")
  



#---------------------------------------
# Run code
    
  # Loop through pre expansion start years
  for(y in 1:length(pre_expansion_start_yr.v)){
    
    pre_expansion_start_yr <- pre_expansion_start_yr.v[y]
    
    # Loop through region radii specifications
    for(j in 1:length(rr_specifications)){
      
      rr_specification <- rr_specifications[j]
      
      # Creating universal file name 
      source(file.path("hlpr_func", "hlprfx_creating_file_name.R"))
      file_name <- creating_file_name.f(data_set.arg = data_set,
                                        rr_specification.arg = rr_specification,
                                        outcome_var.arg = outcome_var,
                                        analysis.arg = analysis_type)
      
      file_name <- paste0(file_name, "_", pre_expansion_start_yr)
      
      #---------------------
      # Running the code 
      
      # Identifying sets in each region (near and far)  
      # Filter for >= "2010-01-01"
      # Filter for Deep set only 
      source(file.path("hlpr_func", 
                       "Regressions",
                       "06a_identifying_region_sets_RR.R"))

      
      # Running regressions
      source(file.path("hlpr_func", 
                       "Regressions",
                       "06c_regressions.R"))

      
    } # end of donut specification loop
    
  }

  
  
  
# ===============================
# Sensitivity Analysis - Continous Distance 
# ===============================
  
  # Define the fishing boundary
  fishing_boundary_nm <- 600
  
  # Region-radii specifications
  rr_specifications <- rr_specification <- "cnt_dist"

  # Loop through pre expansion start years
  for(y in 1:length(pre_expansion_start_yr.v)){  
    
    pre_expansion_start_yr <- pre_expansion_start_yr.v[y]
    
    #---------------------
    # Creating universal file name 
    source(file.path("hlpr_func", "hlprfx_creating_file_name.R"))
    file_name <- creating_file_name.f(data_set.arg = data_set,
                                      rr_specification.arg = rr_specification,
                                      outcome_var.arg = outcome_var,
                                      analysis.arg = analysis_type)
    
    file_name <- paste0(file_name, "_", pre_expansion_start_yr)
    
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

    
  }
  

# ===============================
# Creating data frame  
# ===============================
  rm(list=setdiff(ls(), c()))
  
  source(file.path("hlpr_func", "hlprfx_creating_sensitivityPRE_data_frame.R"))
  pre_expansion_start_yr <- 2006:2010
  
  full_start_year.l <- lapply(1:length(pre_expansion_start_yr), FUN = function(y){
    
    one_year <- pre_expansion_start_yr[y]
    
    one_start_year.df <- creating_pre_expansion_sensitivity_data_frames.f(one_year)
    
    print(paste0("Done with year ", one_year))
    
    return(one_start_year.df)
    })
  
  full_start_year.df <- do.call("rbind", full_start_year.l)
  
  # Creating a final_file_name for the final saved data 
  data_file <- ifelse(data_set == "Observer Data", "OB", "LB")
  outcome_file <- ifelse(outcome_var == "standardized_CPUE", "std", "raw")
  final_file_name <- paste0(data_file, "_",
                            outcome_file, "_",
                            analysis_type)

  saveRDS(full_start_year.df, file.path("Results",
                                        "Figures and Data",
                                        paste0(final_file_name, "_reg_df.RDS")))
  
  
  
  
  