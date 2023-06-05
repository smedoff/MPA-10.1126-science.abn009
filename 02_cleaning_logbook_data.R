
#=============
# Title: Cleaning Logbook Data 
# Author: Sarah Medoff 
#=============

# The purpose of this script is to clean and merge the logbook 
# data sets.  The main cleaning efforts were allocated towards defining
# static variables consistently across fishing sets.  For example, 
# ensuring these variables are the same across set id: 
# set type (deep vs. shallow), begin set coordinates, begin set dates, 
# soak time, gear variables


rm(list=ls())

library(tidyverse)
library(dplyr)

  HDR <- readRDS(file.path("Data",
                           "Logbook Data",
                           "Source",
                           "LLDS_HDR_20210315HAC.rds"))
  
  DETAIL <- readRDS(file.path("Data",
                              "Logbook Data",
                              "Source",
                              "LLDS_DETAIL_20210315HAC.rds"))
  
  rm(list=setdiff(ls(), c("HDR", "DETAIL")))

#-----------
  source(file.path("hlpr_func",
                   "hlprfx_cleaning_HDR.R"))

  HDR_processed <- cleaning_HDR.f(HDR_DATASET = HDR)
  
  
  # SOAK_FLG == TRUE if SOAK < 0 or >3 (negative or greater than 3 days)
  HDR_soak_flg <- HDR_processed %>% 
    filter(!(SOAK_FLG == TRUE))
  # Lost 63 observations 
  
  # Convert SOAK from days to hours to be consistent with observer data 
  HDR_soak <- HDR_soak_flg %>% 
    rename(SOAK_DAYS = SOAK) %>% 
    mutate(SOAK = as.numeric(SOAK_DAYS) * 24)
  
  # Filter for dates early - there are a lot of data input errors that occur 
  # before 2000 (e.g. one SET ID having two different SETBEGIN_DATES)
  # 2004 ended the swordfish fishery closures 
  # We do not use any data from before 2010 in our logbook analysis 
  HDR_cleaned <- HDR_soak %>% 
    filter(SETBEGIN_DATE >= as.Date("2010-01-01"))
  
  
#-----------
  source(file.path("hlpr_func",
                   "hlprfx_cleaning_DETAILS.R"))
  
  DETAIL_cleaned <- cleaning_DETAIL.f(DETAIL_DATASET = DETAIL)


#----------- 
# Combine data sets 
  
  HDR_sub <- HDR_cleaned %>% 
    dplyr::select(SET_ID,
                  SETBEGIN_DATE,
                  SET_BEGIN_LAT,
                  SET_BEGIN_LON,
                  SOAK,
                  BAIT,
                  MAINLINE,
                  NUM_HOOKS = HOOKSSET,
                  LIGHTSTICKS,
                  FLEET)
  
  # Verification Check - Makre sure every observation in the HDR_sub
  # data set represents a unique SET_ID otherwise the left_join will 
  # duplicate rows
  if(!(n_distinct(HDR_sub$SET_ID) == nrow(HDR_sub))){
    stop(print("STOP!! SET_IDs are repeated in the HDR data set.  Meaning, 
               for one set_id, there are more than one variable value 
               (e.g., one set_id having two SETBEGIN_DATES."))
  }
  
  
 # Turn details data set from individual fish level to 
 # species level
  DETAILS_sub <- DETAIL_cleaned %>% 
    filter(!(is.na(ENGLISH_NAME))) %>% 
    group_by(SET_ID, 
             ENGLISH_NAME,
             PERMITNUM, 
             VESSEL_NAME = VESSELNAME,
             TRIP_NUM = INTEG_TRIP_KEY,
             DECLARED_TRIP_TYPE = SETDEPTH) %>% 
    summarize(NUMKEPT= sum(NUMKEPT, na.rm = TRUE),
              NUMRELEASED = sum(NUMRELEASED, na.rm = TRUE)) %>% 
    mutate(NUM_CAUGHT = NUMKEPT + NUMRELEASED) %>% 
    dplyr::select(-c(NUMKEPT, NUMRELEASED)) 
  
  species_catch.df <- left_join(DETAILS_sub, HDR_sub) 
  
  # Code Verification - make sure species_catch.df <= the
  # number of observations as DETAILS_sub. DETAILS_sub should have more
  # observations because we filter for sets greater than 2010 for the logbook 
  # specification
  if(nrow(species_catch.df) > nrow(DETAILS_sub)){
    stop(print("STOP!! left joining of HDR and DETAILS duplicated observations"))
  }
  
  
  #  Filter out missing data 
  species_catch.df <- species_catch.df %>% 
    filter(!(is.na(SET_BEGIN_LAT)),
           !(is.na(SET_BEGIN_LAT)),
           !(is.na(ENGLISH_NAME)))
  # 1,989,025 observations removed leaving 1,703,757 total
  
  # Add underscores to ENLGISH_NAME to be consistent with observer 
  # data entries 
  species_catch.df <- species_catch.df %>% 
    mutate(ENGLISH_NAME = gsub(" ", "_", ENGLISH_NAME, fixed = TRUE)) %>% 
    rename(PERMIT_NUM = PERMITNUM,
           BAIT_TYPE = BAIT)


  #---------------
  saveRDS(species_catch.df, file.path("Data",
                                      "Logbook Data",
                                      "02_species_catch_FULL_df.RDS"))
  


    