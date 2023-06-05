
#=============
# Title: Creating Final Output
# Author: Sarah Medoff 
#=============

# The purpose of this script is to create final figures and tables.  These include 
# - Pre-expansion Mean Table
#  - Regression Figure and Data Frame  
# - Descriptive Stats Tables
# - Descriptive Stats Tables (Space Placebo)
# - Bycatch Percentage Table
# - Covariate Time Trends


rm(list = ls())

# NOTE** parameters that need to be adjusted are within the table-specific
# code chunks.  The parameters at the top of the script do not need to be 
# adjusted

#---------------------------------------
# Setting Parameters 
  
  # Region specifications
  rr_specifications <- paste(c(100, 200, 300), "NM", sep = "")
  
  # Outcome variable
  outcome_var <- "standardized_CPUE"  # relative_CPUE, ln_CPUE, CPUE
  
  # PMNM expansion
  expansion_date <- as.Date("2016-08-26")

  
#---------------------------------------
# Creating Pre-expansion Mean Table
#----------------------
  
  # Data set used 
  data_set <- "Observer Data"
  
  # Real Analysis, Placebo Test, Sensitivity Analysis? 
  analysis_type <- "real"
  
  source(file.path("hlpr_func", 
                   "Creating Outputs", 
                   "source_creating_pre_expansion_mean_table.R"))
  
  #-------
  # Data set used 
  data_set <- "Logbook Data"
  
  # Real Analysis, Placebo Test, Sensitivity Analysis? 
  analysis_type <- "real"
  
  source(file.path("hlpr_func", 
                   "Creating Outputs", 
                   "source_creating_pre_expansion_mean_table.R"))
  
  
  
#---------------------------------------
# Creating Regression Figure and Data Frame
#----------------------
  
  # Data set used 
  data_set <- "Observer Data"
  
  
  # Real Analysis, Placebo Test, Sensitivity Analysis? 
  analysis_type <- "real"
  
  source(file.path("hlpr_func", 
                   "Creating Outputs", 
                   "source_creating_regression_figure.R"))
  
  
  
  
#---------------------------------------
# Creating Descriptive Stats Tables 
  # for different fishing areas 
#----------------------
  
  # Data set used 
  data_set <- "Observer Data"
  
  
  # Real Analysis, Placebo Test, Sensitivity Analysis? 
  analysis_type <- "real"
  
  source(file.path("hlpr_func", 
                   "Creating Outputs", 
                   "source_creating_descriptive_stats_tables.R"))
  
  
  
  
#---------------------------------------
# Creating Descriptive Stats Tables 
  # for placebo space 
#----------------------
  
  # Data set used 
  data_set <- "Observer Data"
  
  
  # Real Analysis, Placebo Test, Sensitivity Analysis? 
  analysis_type <- "placeboSPACE"
  
  source(file.path("hlpr_func", 
                   "Creating Outputs", 
                   "source_creating_descriptive_stats_tables_placebo_space.R"))

  
  
  
#---------------------------------------
# Creating Bycatch Percentage Table
#----------------------  
  
  # Data set used 
  data_set <- "Observer Data"
  
  
  source(file.path("hlpr_func", 
                   "Creating Outputs", 
                   "source_creating_bycatch_yearly_percentages.R"))
  
  
  
  
#---------------------------------------
# Creating Covariate Time Trends 
#----------------------  
  
  # Data set used 
  data_set <- "Observer Data"
  
  
  source(file.path("hlpr_func", 
                   "Creating Outputs", 
                   "source_creating_covariate_ts_graphs.R"))
  
  
  
#---------------------------------------
# Quantifying offsets and calculating fishing activity within MPA 
#----------------------  
  
  # Data set used 
  data_set <- "Logbook Data"
  
  source(file.path("hlpr_func", 
                   "Creating Outputs", 
                   "source_quantifying_offsets_and_inside_mpa_activity.R"))
  
  # Data set used 
  data_set <- "Observer Data"
  
  source(file.path("hlpr_func", 
                   "Creating Outputs", 
                   "source_quantifying_offsets_and_inside_mpa_activity.R"))
  
  
  
  
  
  