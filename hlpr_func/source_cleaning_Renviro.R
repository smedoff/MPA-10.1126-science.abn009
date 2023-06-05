

# Before running each script, we want to clean the R enviornment while still saving some 
# Important parameters.  

# ------------------------------------------------------------
# Preserving the parameters - Region Radii
  enviro_vars.v <- c("data_set", "expansion_date", "fe_type",
                     "file_name", "outcome_var", "analysis_type", 
                     "rr_specifications", "rr_specification")
  
#------
if(rr_specification == "cnt_dist"){
  enviro_vars.v <- c(enviro_vars.v,
                     "fishing_boundary_nm")
}

#------
  if(analysis_type == "placeboSPACE"){
    enviro_vars.v <- c(enviro_vars.v,
                       "space_shift", "space_shifters", "space_shift_deg")
  }

#------
  if(analysis_type == "sensitivityPRE"){
    enviro_vars.v <- c(enviro_vars.v,
                       "pre_expansion_start_yr", "pre_expansion_start_yr.v")
  }
  
  

