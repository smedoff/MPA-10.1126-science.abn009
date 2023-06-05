
# This script will create a yearly bycatch table that reports the % of catch that is considered 
# bycatch. 

library(tidyverse)
library(dplyr)
library(foreign)
library(xtable)
library(stargazer)

# Species the council is interested in 
# Albacore
# Bigeye tuna
# Bluefin tuna
# Skipjack tuna
# Yellowfin tuna
# Swordfish
# Blue marlin
# Spearfish (hebi)
# Striped marlin
# Mahimahi
# Ono (wahoo)
# Opah (moonfish)
# Oilfish
# Pomfrets (monchong)

# ------------------------------------------------------------
# Cleaning R environment
enviro_vars.v <- c("data_set")
rm(list=setdiff(ls(), enviro_vars.v))

# ------------------------------------------------------------
# Read in source files

# Use the raw (un species-panel'd data set)
  org_species_catch_FULL.df <- readRDS(file.path("Data",
                                             data_set,
                                             "04_PMNM_distanceSample.RDS")) %>% 
    mutate(YEAR = as.numeric(substr(SETBEGIN_DATE, 1, 4))) %>%
    filter(YEAR >= 2010 & YEAR < 2020,
           DIST_NM <= 600,
           DECLARED_TRIP_TYPE == "D")
    
  species.v <- unique(org_species_catch_FULL.df$ENGLISH_NAME)  


#----------------
# Assign groups of species to a unifed name 
  
  pomfrets.v <- grep("POMFRET", 
                     species.v, value = TRUE)
  
  marlin.v <- grep("MARLIN",
                   species.v, value = TRUE)
  
  tunas.v <- c("BIGEYE_TUNA", "YELLOWFIN_TUNA",
               "SKIPJACK_TUNA", "PACIFIC_BLUEFIN_TUNA", "ALBACORE")
  
  other.v <- c("SWORDFISH", "SHORTBILL_SPEARFISH", "MAHIMAHI", 
               "OILFISH", "OPAH", "WAHOO")
  
  
  #----
  # Calc bycatch percent  
  BYCATCH.df <- org_species_catch_FULL.df %>% 
    filter(!(ENGLISH_NAME %in% c(pomfrets.v, marlin.v, tunas.v, other.v))) %>% 
    group_by(YEAR) %>% 
    summarize(BYCATCH = sum(NUM_CAUGHT, na.rm = TRUE)) %>% 
    left_join(org_species_catch_FULL.df %>% 
                group_by(YEAR) %>% 
                summarize(TOTAL_CATCH = sum(NUM_CAUGHT, na.rm = TRUE))) %>% 
    mutate(BYCATCH_PERC = round(BYCATCH/TOTAL_CATCH * 100, 2)) %>% 
    mutate(Percentage = paste0(BYCATCH_PERC, "%")) %>% 
    dplyr::select(Year = YEAR, Percentage)
  
  #--------
  # Just save straight to the science folder
  print(xtable(BYCATCH.df, type = "latex"), include.rownames=FALSE, 
        file = file.path("Results",
                         "Tables",
                         "Descriptive_Tables",
                         "Bycatch_Percentage.txt"))  
  
