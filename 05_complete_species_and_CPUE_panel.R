
#=============
# Title: Complete Species Panel
# Author: Sarah Medoff 
#=============

# The purpose of this script is to create a full species panel where each 
# set will have four observations: one for bigeye tuna, one for yellowfin 
# tuna, one for all, and one for other.  Sets that do not catch any fish 
# from one particular group will record a zero 


#---------
# Observer Data 
#---------

rm(list=ls())

# Parameteres 
  
  # Observer vs. Logbook 
  data_set <- "Observer Data"
  
  # Sourcing Code 
  source(file.path("hlpr_func", "Data Processing",
                   "05_SOURCE_complete_species_and_CPUE_panel.R"))

#---------
# Logbook Data 
#---------

rm(list=ls())

# Parameteres 
  
  # Observer vs. Logbook 
  data_set <- "Logbook Data"
  
  # Sourcing Code 
  source(file.path("hlpr_func", "Data Processing",
                   "05_SOURCE_complete_species_and_CPUE_panel.R"))

