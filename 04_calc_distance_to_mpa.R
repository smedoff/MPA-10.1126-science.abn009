
#=============
# Title: Calculating Distance to MPA
# Author: Sarah Medoff 
#=============

# The purpose of this script is to calculate the distance to 
# the MPA border. The distance variable (DIST_NM) will be computed 
# in nautical  miles and scaled by 1,000



#---------
# Observer Data 
#---------

rm(list=ls())

# Parameteres 
  
  # Observer vs. Logbook 
  data_set <- "Observer Data"
  
  # Sourcing Code 
  source(file.path("hlpr_func", "Data Processing",
                   "04_SOURCE_calc_distance_to_mpa.R"))


#---------
# Logbook Data 
#---------

rm(list=ls())

# Parameteres 
  
  # Observer vs. Logbook 
  data_set <- "Logbook Data"
  
  # Sourcing Code 
  source(file.path("hlpr_func", "Data Processing",
                   "04_SOURCE_calc_distance_to_mpa.R"))
  
  