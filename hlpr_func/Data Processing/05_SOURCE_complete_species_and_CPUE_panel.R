
# This script will take each set id and calculate the number caught for 
# bigeye, yellowfin, all species, and other species.  Variables that do 
# not change across set ids are preserved.  We then calculate CPUE as 
# catch per 1000 hooks. 

# prepare the workspace --------------------------------------------------------
options(scipen=999)
library(tidyverse)
library(dplyr)
library(ggplot2)


#-------------------------------------------------------------------------------
# Pull in the data 
 
  distance_sample.df <- readRDS(file.path(".",
                                          "Data", 
                                          data_set,
                                          paste0("04_PMNM_distanceSample.RDS"))) 

# Validation check:  Verify that there is only one species observation for 
# each set_id 
  species_per_id <- distance_sample.df %>% 
    group_by(SET_ID, ENGLISH_NAME) %>% 
    summarize(n = n()) %>% 
    filter(!(n == 1))
  
  if(!(nrow(species_per_id) == 0)){
    stop(print("STOP!! your distance_sample.df is not a species panel.  
               group_by(ENGLISH_NAME) before proceeding"))
  }else{
    rm(species_per_id)
  }


# ----------
# Creating a full panel by set_id x species

  # Get unique set ids for function
  set_ids.v <- unique(distance_sample.df$SET_ID)
  

  # Create a speceis x set_id panel
  set_panel_unfilled.df <- expand.grid(SET_ID = unique(distance_sample.df$SET_ID),
                                       ENGLISH_NAME = c("BIGEYE_TUNA", "YELLOWFIN_TUNA", "ALL", "OTHER"))

  
  #---
  # Obtain data frame for static variables for each set_id
  
  # Remove species-specific variables from column names 
  if(data_set == "Observer Data"){
    static_variables.v <- setdiff(names(distance_sample.df), 
                                  c("ENGLISH_NAME", "SPECIES_CODE", "NUM_CAUGHT", 
                                    "SPECIES_COMMON_NAME", "APPROX_LEN"))}
  
  if(data_set == "Logbook Data"){
    static_variables.v <- setdiff(names(distance_sample.df), 
                                  c("ENGLISH_NAME", "NUM_CAUGHT"))}
  
  
  #------
  static.df <- distance_sample.df %>% 
    ungroup() %>% 
    dplyr::select(SET_ID, all_of(static_variables.v)) %>% 
    unique()
  
  # Validation check: Make sure nrow(static.df) == # of unique set_ids
  # if nrow(static.df)>n_distinct(set_ids) this means some static variables 
  # repeated for one set_id.  Edit this before preceeding
  if(!(nrow(static.df) == length(set_ids.v))){
    stop(print("STOP!! Static variables are change within one set_id."))
  }
  
  #----
  # Left join with set_panel_unfilled.df
  set_panel_static.df <- set_panel_unfilled.df %>% 
    left_join(static.df)
  

# ----------
# Creating a catch panel for each species group  
  
  # Creating a group_by data frame for each species group 
  
  # Bigeye and yellowfin
  if(data_set == "Observer Data"){
    bet_yf.df <- distance_sample.df %>% 
      filter(ENGLISH_NAME %in% c("BIGEYE_TUNA", "YELLOWFIN_TUNA")) %>% 
      group_by(SET_ID, ENGLISH_NAME) %>% 
      summarize(NUM_CAUGHT = sum(NUM_CAUGHT, na.rm = TRUE),
                APPROX_LEN = mean(APPROX_LEN, na.rm = TRUE))
  }
  
  if(data_set == "Logbook Data"){
    bet_yf.df <- distance_sample.df %>% 
      filter(ENGLISH_NAME %in% c("BIGEYE_TUNA", "YELLOWFIN_TUNA")) %>% 
      group_by(SET_ID, ENGLISH_NAME) %>% 
      summarize(NUM_CAUGHT = sum(NUM_CAUGHT, na.rm = TRUE))
  }

  #--------
  # Other
  if(data_set == "Observer Data"){
    other.df <- distance_sample.df %>% 
      filter(!(ENGLISH_NAME %in% c("BIGEYE_TUNA", "YELLOWFIN_TUNA"))) %>% 
      group_by(SET_ID) %>% 
      summarize(NUM_CAUGHT = sum(NUM_CAUGHT, na.rm = TRUE),
                APPROX_LEN = mean(APPROX_LEN, na.rm = TRUE)) %>% 
      mutate(ENGLISH_NAME = "OTHER")
  }
  
  if(data_set == "Logbook Data"){
    other.df <- distance_sample.df %>% 
      filter(!(ENGLISH_NAME %in% c("BIGEYE_TUNA", "YELLOWFIN_TUNA"))) %>% 
      group_by(SET_ID) %>% 
      summarize(NUM_CAUGHT = sum(NUM_CAUGHT, na.rm = TRUE)) %>% 
      mutate(ENGLISH_NAME = "OTHER")
  }
  
  #--------
  # All
  if(data_set == "Observer Data"){
    all.df <- distance_sample.df %>% 
      group_by(SET_ID) %>% 
      summarize(NUM_CAUGHT = sum(NUM_CAUGHT, na.rm = TRUE),
                APPROX_LEN = mean(APPROX_LEN, na.rm = TRUE)) %>% 
      mutate(ENGLISH_NAME = "ALL")
  }
  
  if(data_set == "Logbook Data"){
    all.df <- distance_sample.df %>% 
      group_by(SET_ID) %>% 
      summarize(NUM_CAUGHT = sum(NUM_CAUGHT, na.rm = TRUE)) %>% 
      mutate(ENGLISH_NAME = "ALL")
  }
  
  
  
  
  #----
  # Rbind for a complete panel 
  species_specific.df <- rbind(bet_yf.df, 
                               other.df, 
                               all.df)
  
  #------
  # Validation check
  # Make sure there are the same number of set_ids in the species_specific.df
  if(!(n_distinct(species_specific.df$SET_ID) == length(set_ids.v))){
    stop(print("STOP!! Some set_ids are not accounted for"))
  }
  
  # Save for Validation check later
  
  zero_catch <- length(set_ids.v)*4 - nrow(species_specific.df)
  


#-------------------------
# Creating final species panel 
  
  # Join it to the set_panel 
  set_panel.df <- set_panel_static.df %>% 
    left_join(species_specific.df)
  
  #------
  # Validation check
  zero_catch2 <- set_panel.df %>% 
    filter(is.na(NUM_CAUGHT)) %>% 
    nrow()
  
  if(!(zero_catch == zero_catch2)){
    print("STOP!! problem with missing data in set_panel.df")
  }else{
    rm(zero_catch, zero_catch2)
  }
  
  
  # Make sure set_panel.df has a full grid of set_ids x species
  if(!(nrow(set_panel.df) == nrow(set_panel_unfilled.df))){
    stop(print("STOP!! The species/set panel is not complete.  Some sets are missing"))
  }
  
  
  
  #------
  # Fill 0's with NA for num_caught
  set_panel.df <- set_panel.df %>% 
    mutate(NUM_CAUGHT = as.numeric(replace_na(NUM_CAUGHT, 0)),
           NUM_HOOKS = as.numeric(NUM_HOOKS),
           CPUE = (NUM_CAUGHT/NUM_HOOKS)*1000)
  

#-------------------------
# Save final df 
  saveRDS(set_panel.df, file.path(".",
                                  "Data",
                                  data_set,
                                  paste0("05_FullSetPanel_2000_2020.RDS")))
  
