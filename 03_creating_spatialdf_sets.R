
#=============
# Title: Creating Spatial Data 
# Author: Sarah Medoff 
#=============

# The purpose of this script is to create a spatial points data set 
# for set locations for the observer and logbook data.  The spatial points
# will be used later in the program to identify set distance to MPA 
# and to identify sets within specified regions with respect to the 
# monument border.  


#---------
# Observer Data 
#---------

  rm(list=ls())

  # Parameteres 

  # Observer vs. Logbook 
  data_set <- "Observer Data"
  
  # Sourcing Code 
  source(file.path("hlpr_func", "Data Processing",
                   "03_SOURCE_creating_spatialdf_sets.R"))
  
  
#---------
# Logbook Data 
#---------
  
  rm(list=ls())
  
  # Parameteres 
  
  # Observer vs. Logbook 
  data_set <- "Logbook Data"
  
  # Sourcing Code 
  source(file.path("hlpr_func", "Data Processing",
                   "03_SOURCE_creating_spatialdf_sets.R"))
  