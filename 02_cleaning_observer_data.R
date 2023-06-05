
#=============
# Title: Cleaning Observer Data 
# Author: Sarah Medoff 
#=============

# The purpose of this script is to clean and merge the observer 
# data sets.  The main cleaning efforts were allocated towards defining
# static variables consistently across fishing sets.  For example, 
# ensuring these variables are the same across set id: 
# set type (deep vs. shallow), begin set coordinates, begin set dates, 
# soak time, gear variables

library(tidyverse)
library(dplyr)
rm(list = ls())

source(file.path("hlpr_func",
                 "hlprfx_creating_date_objects.R"))

source(file.path("hlpr_func", 
                 "hlprfx_converting_decdegree.R"))

#-------------------
# Bringing in the data 

  ENVIRO <- readRDS(file.path("Data",
                              "Observer Data", 
                              "Source",
                              "newobs_LDS_SET_ENVIRON_V.rds"))
  
  CATCH <- readRDS(file.path("Data",
                             "Observer Data", 
                             "Source",
                             "newobs_LDS_CATCH_V.rds"))
  
  GEAR <- readRDS(file.path("Data",
                            "Observer Data", 
                            "Source",
                            "newobs_LOP_GEAR_CFG_V.RDS"))
  
  TRIPS <- readRDS(file.path("Data",
                             "Observer Data", 
                             "Source",
                             "newobs_LDS_TRIPS_V.RDS"))

#-------------------
# Sub setting catch data 
  
  # CATCH data is at the individual fish level, SPECIES_CATCH.df is at the 
  # species level
  SPECIES_CATCH.df <- CATCH %>%
    mutate(SET_ID = paste0(TRIP_NUM, SET_NUM)) %>% 
    mutate(SOAK = difftime(HAUL_BEGIN_DTM, SET_END_DTM, units = "hours")) %>%
    dplyr::select(PERMIT_NUM, TRIP_NUM, SET_ID, 
                  ENGLISH_NAME, APPROX_LEN, SOAK) %>% 
    group_by(PERMIT_NUM, TRIP_NUM, SET_ID, SOAK, ENGLISH_NAME) %>% 
    summarize(NUM_CAUGHT = n(),
              APPROX_LEN = mean(APPROX_LEN, na.rm = TRUE))
  
  #----------
  # Validation Check - make sure there is only one species observation for each
  # set_id 
  species_x_setid <- SPECIES_CATCH.df %>% 
    group_by(SET_ID, ENGLISH_NAME) %>% 
    summarize(n = n()) %>% 
    filter(n > 1)
  
  if(!(nrow(species_x_setid) == 0)){
    stop(print("STOP!! some static variables differ within one set_id"))
  }else{
    rm(species_x_setid)
  }
    


#-------------------
# Creating Date Objects
  
  ENVIRO$SETBEGIN_DATE <- creating_dates.f(df = ENVIRO, type = "SETBEGIN")
  ENVIRO$SETEND_DATE <- creating_dates.f(df = ENVIRO, type = "SETEND")
  ENVIRO$HAULBEGIN_DATE <- creating_dates.f(df = ENVIRO, type = "HAULBEGIN")
  ENVIRO$HAULEND_DATE <- creating_dates.f(df = ENVIRO, type = "HAULEND")
  
  ENVIRO_sub <- ENVIRO %>%
    mutate(SET_ID = paste0(TRIP_NUM, SET_NUM)) %>%
    dplyr::select(SET_ID, #SOAK = SOAK_TIME, 
           SETBEGIN_DATE, SETEND_DATE, HAULBEGIN_DATE, HAULEND_DATE,
           SET_BEGIN_LAT, SET_BEGIN_LON, SET_END_LAT, SET_END_LON_DEG,
           HAUL_BEGIN_LAT, HAUL_BEGIN_LON, HAUL_END_LAT, HAUL_END_LON) 
  
  
  #----------
  # Validation Check - make sure each observation represents a unique set_id
  enviro_set_id_unique <- ENVIRO_sub %>% 
    group_by(SET_ID) %>% 
    summarize(n = n()) %>% 
    filter(n > 1)
  
  if(!(nrow(enviro_set_id_unique) == 0)){
    stop(print("STOP!! some set_ids have mult enviro variables. 
               Merge before proceeding"))
  }else{
    rm(enviro_set_id_unique)
  }
  

  #------------------
  # Merge species_catch and enviro variables
  # Filter out observations with missing location
  # - This df will be at the species x set_id level
  species_catch_FULL.df <- SPECIES_CATCH.df %>%
    left_join(ENVIRO_sub, by = "SET_ID") %>% 
    filter(!is.na(SET_BEGIN_LON), !is.na(SET_BEGIN_LAT)) %>%
    mutate(ENGLISH_NAME = gsub("\\s*\\([^\\)]+\\)","",as.character(ENGLISH_NAME))) %>% #remove all paranthesis
    mutate(ENGLISH_NAME = gsub("\\-", "_", gsub("\\ ", "_", ENGLISH_NAME)))
  
  #----------
  # Validation Check - make sure obervations weren't duplicated on the left_join
  if(!(nrow(species_catch_FULL.df) <= nrow(SPECIES_CATCH.df))){
    stop(print("STOP!! left_merge of SPECIES_CATCH.df and ENVIRO.df resulted
               in duplicated observations.  Correct before proceeding"))
  }else{
    rm(enviro_set_id_unique)
  }
  

#------------------
# Adding trip level characteristics 
  
  species_catch_FULL.df <- species_catch_FULL.df %>% 
    left_join(TRIPS %>% 
                dplyr::select(TRIP_NUM, VESSEL_NAME, 
                              TRIP_TYPE, DECLARED_TRIP_TYPE), 
              by = "TRIP_NUM")
  
  
#------------------
  # Adding total number of hooks 
  
  HOOKS <- GEAR %>%
    mutate(SET_ID = paste0(T_TRIP_NUM, S_SET_NUM)) %>%
    mutate(LIGHT_COUNT = replace_na(NUM_LITE_DEVICES, 0)) %>% 
    dplyr::select(SET_ID,
                  LIGHT_COUNT,
                  NUM_HOOKS = NUM_HKS_SET, 
                  HOOK_TYPE = HK_TYPE_CODE_VAL, 
                  HOOKS_PER_FLT = HKS_PER_FLT, 
                  TARGET_SPECIES = TARGET_SPECIES_NAME,
                  BAIT_TYPE = BAIT_CODE_VAL,
                  LIGHT_TYPE = LITE_DEVICE_TYPE_CODE_VAL,
                  LIGHT_COLOR = LITE_DEVICE_COLOR_CODE_VAL,
                  FLOAT_LINE_DIAMETER = FLTLN_DIAM,
                  FLOAT_LINE_LENGTH = FLTLN_LEN)
  
  #----------
  # Validation Check - make sure each observation represents a unique set_id
  if(!(n_distinct(HOOKS$SET_ID) == nrow(HOOKS))){
    stop(print("STOP!! some set_ids have mult hooks variables. 
               Merge variables before proceeding"))
  }
  
  
  
  species_catch_FULL.df <- species_catch_FULL.df %>% 
    left_join(HOOKS) 

#----------------
# Remove all sets made before the 2004 swordfish closure
  species_catch_FULL.df <- species_catch_FULL.df %>% 
    filter(SETBEGIN_DATE >= as.Date("2004-01-01"))

#----------------
# Save Data
  
  saveRDS(species_catch_FULL.df, file.path(".",
                                          "Data",
                                          "Observer Data",
                                          "02_species_catch_FULL_df.RDS"))
  


#----------------
# Count the number of set_ids in our final sample for our main specifications. 
  # species_catch_FULL was already filtered for missing lat/lon
  
  #test_sample <- species_catch_FULL.df %>% 
  #  group_by(SET_ID) %>% 
  #  summarize(LON = unique(SET_BEGIN_LON), 
  #            LAT = unique(SET_BEGIN_LAT), 
  #            DATE = unique(SETBEGIN_DATE), 
  #            TRIP_TYPE = unique(DECLARED_TRIP_TYPE)) %>% 
  #  filter(DATE %in% seq.Date(as.Date("2010-01-01"), as.Date("2019-12-31"), by = "days"), 
  #         TRIP_TYPE == "D")
  
  
