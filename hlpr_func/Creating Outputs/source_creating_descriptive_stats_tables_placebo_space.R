
# This script will compile summary statistics for the space placebo shifts

library(tidyverse)
library(dplyr)
library(foreign)
library(xtable)
library(stargazer)

# ------------------------------------------------------------
# Cleaning R environment
  enviro_vars.v <- c("analysis_type", "data_set", 
                     "outcome_var")
  rm(list=setdiff(ls(), enviro_vars.v))
  
  source(file.path("hlpr_func",
                   "hlprfx_create_descriptive_stat_table_placebo_space.R"))


#---------------------------------------
# Setting Parameters 

space_shifters <- c("N", "S", "E", "W")
                      

# --------------------------
# Creating the table 
  
  # Calc stat for each space shift
  SpacePlacebo_stats.l <- lapply(1:length(space_shifters), FUN = function(i){
    
    space_shift <- space_shifters[i]
    
    # Grabbing universal file name 
    source(file.path("hlpr_func", "hlprfx_creating_file_name.R"))
    file_name <- creating_file_name.f(data_set.arg = data_set,
                                      rr_specification.arg = "100NM",
                                      outcome_var.arg = outcome_var,
                                      analysis.arg = analysis_type)
    
    file_name <- paste0(file_name, "_", space_shift)
    
    # This data set was made in the hlpr_func/Regression/6a
    # Data is filtered for 10-year sample and D set
    donut_sample.df <- readRDS(file.path("Data",
                                         data_set,
                                         "Specification Specific",
                                         paste0("06a_", file_name, ".RDS"))) 
    
    
    create_desc_stat_table.f(df = donut_sample.df,
                             space_shift.arg = space_shift)
    
  })
  
  SpacePlacebo_stats.df <- purrr::reduce(SpacePlacebo_stats.l, left_join) %>%
    mutate_each(funs(prettyNum(., big.mark=",")))
  
  # Just save straight to the science folder
  print(xtable(SpacePlacebo_stats.df, type = "latex"), include.rownames=FALSE, 
        file = file.path("Results",
                         "Tables",
                         "Descriptive_Tables",
                         "Descriptive_Stats_Placebo_Space.txt"))
  
  
  
  
