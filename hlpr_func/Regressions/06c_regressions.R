
# This script will compute the diff-in-diff regression for each fixed effect 
# specification given by the fe_type parameter.  The MPA expansion date is 
# defined as the treatment period and 'near' sets are the treatment group. 
# The output of this code is a list of regression results by species and the 
# pre-expansion statistics, that is, the per-expansion mean and standard deviation
# for each region radii, species group, and fixed effects specification.  


# prepare the workspace --------------------------------------------------------
options(scipen=999)
library(tidyverse)
library(dplyr)
library(lmtest)
library(AER)
library(plm)
library(gridExtra)
library(grid)
library(broom)

# ------------------------------------------------------------
# Cleaning R environment
  source(file.path("hlpr_func", "source_cleaning_Renviro.R"))
  rm(list=setdiff(ls(), enviro_vars.v))

# --------------------------- 
# Bringing in the data 
# --------------------------- 
  
  donut_sample.df <- readRDS(file.path("Data", 
                                       data_set,
                                       "Specification Specific",
                                       paste0("06a_", file_name, ".RDS")))
  
  
  species_of_interest <- data.frame(ENGLISH_NAME = unique(donut_sample.df$ENGLISH_NAME))
  
  
  source(file.path("hlpr_func",
                   "hlprfx_running_reg.R"))
  
  
  source(file.path("hlpr_func",
                   "hlprfx_compute_standardization.R"))
  
# --------------------------- 
# Cleaning the data for the regressions
# --------------------------  
# Remove any NA entries for CPUE.  This results from missing hook data 
  donut_sample.df <- donut_sample.df %>% 
    filter(!is.na(CPUE),
           !(NUM_HOOKS == 0))
  
# --------------------------- 
  # Creating the panel 
# ---------------------------  
  
  # Creating a species panel to do species specific regressions
    # Subset fleet type and creating dummies 
  if(rr_specification == "cnt_dist"){
    
    SPECIES_PANEL.df <- donut_sample.df %>% 
      mutate(SET_MONTH = substr(SETBEGIN_DATE, 6, 7), 
             SET_YR = substr(SETBEGIN_DATE, 1, 4),
             SET_MONTHYR = substr(SETBEGIN_DATE, 1, 7), 
             EXP_DUMMY = ifelse(SETBEGIN_DATE < expansion_date, 0, 1),
             DIST_TREATMENT = DIST_NM_scaled*-1,
             DiD = EXP_DUMMY * DIST_TREATMENT) %>%
      rename(DATE = SETBEGIN_DATE) 
    
  }else{
    # if rr_specification %in% c(100NM, 200NM, 300NM)
    
    SPECIES_PANEL.df <- donut_sample.df %>%
      mutate(EXP_DUMMY = ifelse(SETBEGIN_DATE < expansion_date, 0, 1),
             DIST_TREATMENT = ifelse(donut_id == "small", 1, 0),
             DiD = EXP_DUMMY * DIST_TREATMENT) %>% 
      rename(DATE = SETBEGIN_DATE)
    
  }

  # --------------
  # Validation Check
  # Make sure theres 1 unique set observation.  If this returned a df wich had nrow > 0 than 
  # one of the selected variables is not unique  
  check_unique_sets <- SPECIES_PANEL.df %>% 
    group_by(SET_ID, ENGLISH_NAME) %>% 
    nrow() == n_distinct(SPECIES_PANEL.df$SET_ID)*n_distinct(SPECIES_PANEL.df$ENGLISH_NAME)
  
  if(check_unique_sets == TRUE){
    paste0("There is one set_id for each species group: GOOD TO GO!!!")
    
    rm(check_unique_sets)
  }else{
    stop(paste0("STOP: One species group is missing from 1 or more set_ids"))
  }
  
  

  
#=================
# Species by Species regressions
#=================
# The function preforms the standardization CPUE calculations 
  
  species_reg.l <- lapply(1:length(fe_type), FUN = function(f){
    print(f)
    one_fe_type <- fe_type[f]
    
    running_did.f(df = SPECIES_PANEL.df, 
                  species.v = species_of_interest, 
                  specification = one_fe_type,
                  outcome_var = outcome_var)
    
  })
  
  names(species_reg.l) <- fe_type

  # Save Regression Results
  save(species_reg.l, 
       file= file.path("Data",
                       data_set,
                       "Specification Specific",
                       paste0("06c_", file_name, ".RDS")))
    
  
