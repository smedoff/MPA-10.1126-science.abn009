
# This script will create a catch composition table 

library(tidyverse)
library(dplyr)
library(foreign)
library(xtable)
library(stargazer)

# ------------------------------------------------------------
# Cleaning R environment
enviro_vars.v <- c("data_set", "expansion_date")
rm(list=setdiff(ls(), enviro_vars.v))

# ------------------------------------------------------------
# Read in source files

# Use the raw (un species-panel'd data set)
  org_species_catch_FULL.df <- readRDS(file.path("Data",
                                             data_set,
                                             "04_PMNM_distanceSample.RDS")) %>% 
    mutate(YEAR = as.numeric(substr(SETBEGIN_DATE, 1, 4)),
           EXPANSION = ifelse(SETBEGIN_DATE < expansion_date, "Pre", "Post")) %>%
    filter(YEAR >= 2010 & YEAR < 2020)
    
  species.v <- unique(org_species_catch_FULL.df$ENGLISH_NAME)  


#----------------
# Assign groups of species to a unifed name 
  
  pomfrets.v <- grep("POMFRET", 
                     species.v, value = TRUE)
  
  marlin.v <- grep("MARLIN",
                   species.v, value = TRUE)
  
  turtle.v <- grep("TURTLE", 
                   species.v, value = TRUE)
  
  dolphin.v <- grep("DOLPHIN", 
                    species.v, value = TRUE)
  
  shark.v <- grep("SHARK", 
                  species.v, value = TRUE)
  
  lancet.v <- grep("LANCETFISH", 
                   species.v, value = TRUE)
  
  #----
  # Rename the grouped species as previously specified 
  species_catch_FULL.df <- org_species_catch_FULL.df %>% 
    mutate(ENGLISH_NAME = ifelse(ENGLISH_NAME %in% dolphin.v, 
                                       "DOLPHIN",
                                       ifelse(ENGLISH_NAME %in% lancet.v,
                                              "LANCETFISH",
                                              ifelse(ENGLISH_NAME %in% marlin.v,
                                                     "MARLIN",
                                                     ifelse(ENGLISH_NAME %in% pomfrets.v, 
                                                            "POMFRET",
                                                            ifelse(ENGLISH_NAME %in% shark.v,
                                                                   "SHARK",
                                                                   ifelse(ENGLISH_NAME %in% turtle.v,
                                                                          "TURTLE",
                                                                          ENGLISH_NAME))))))) 
  
  rm(pomfrets.v, marlin.v, turtle.v, dolphin.v, shark.v, lancet.v)
  
#----------------
# Calc stats for species group    

  valuable.v <- c("BIGEYE_TUNA", "YELLOWFIN_TUNA") 

  other_tuna.v <- c("SKIPJACK_TUNA", "PACIFIC_BLUEFIN_TUNA", "ALBACORE")
  
  billfish.v <- c("MARLIN", "SHORTBILL_SPEARFISH", "SWORDFISH")
  
  incidental.v <- c("MAHIMAHI", "OILFISH", "OPAH", "POMFRET", "WAHOO")
  
  discards.v <- c("DOLPHIN", "LANCETFISH", "SHARK", "TURTLE", 
                  "OTHER") # The other group gets officially made next
  
  others.v <- setdiff(unique(species_catch_FULL.df$ENGLISH_NAME),
                      c(valuable.v, other_tuna.v, billfish.v, incidental.v, discards.v))
  
  #-------
  # Validation Check - Make sure all species are accounted for 
  species_count <- sum(length(billfish.v), length(incidental.v), length(other_tuna.v), 
                       length(valuable.v), length(others.v), 
                       # sub 'other' group out of others.v, we havent created that category yet )
                       length(discards.v) - 1)
  if(species_count == n_distinct(species_catch_FULL.df$ENGLISH_NAME)){
    rm(species_count)
  }else{
    stop(print("STOP!!! Some species are misspecified."))
  }
  
  # Unify species in 'other' group 
  species_catch_FULL.df <- species_catch_FULL.df %>% 
    mutate(ENGLISH_NAME = ifelse(ENGLISH_NAME %in% others.v, "OTHER", ENGLISH_NAME))


#---------------------
# Creating stats for each group 
  
  EXP_PERIOD_HOOKS.df <- species_catch_FULL.df %>% 
    group_by(SET_ID, SETBEGIN_DATE, EXPANSION) %>% 
    summarize(HOOKS = unique(NUM_HOOKS))%>% 
    group_by(EXPANSION) %>% 
    summarize(HOOKS = mean(HOOKS, na.rm = TRUE))
  
  # Calc species statistics function
  calc_species_stats.df <- function(filtered_df){
    
    SPECIES_YEARLY.df <- filtered_df %>% 
      group_by(ENGLISH_NAME, EXPANSION) %>% 
      summarize(NUM_CAUGHT = mean(NUM_CAUGHT, na.rm = TRUE)) %>% 
      left_join(EXP_PERIOD_HOOKS.df) %>% 
      mutate(CPUE = NUM_CAUGHT*(1000/HOOKS)) %>% 
      dplyr::select(ENGLISH_NAME, EXPANSION, CPUE) %>%
      spread(EXPANSION, CPUE)
    
  }
  #------

  
  # -- Most Valuable species 
  ALL.df <- species_catch_FULL.df %>% 
    filter(ENGLISH_NAME %in% valuable.v) %>% 
    mutate(ENGLISH_NAME = "MOST_VALUABLE_SPP") %>% 
    calc_species_stats.df()
  
  SPECIES.df <- species_catch_FULL.df %>% 
    filter(ENGLISH_NAME %in% valuable.v) %>% 
    calc_species_stats.df()
  
  VALUABLE.df <- rbind(ALL.df, 
                       SPECIES.df)
  
  rm(ALL.df, SPECIES.df)
  
  
  
  # -- Other Species species 
  OTHER.df <- species_catch_FULL.df %>% 
    filter(!(ENGLISH_NAME %in% valuable.v)) %>% 
    mutate(ENGLISH_NAME = "OTHER_SPP") %>% 
    calc_species_stats.df()
  
  
  # -- Other Tuna
  ALL.df <- species_catch_FULL.df %>% 
    filter(ENGLISH_NAME %in% other_tuna.v) %>% 
    mutate(ENGLISH_NAME = "OTHER_TUNA") %>% 
    calc_species_stats.df()
  
  SPECIES.df <- species_catch_FULL.df %>% 
    filter(ENGLISH_NAME %in% other_tuna.v) %>% 
    calc_species_stats.df()
  
  OTHER_TUNA.df <- rbind(ALL.df, 
                       SPECIES.df)
  
  rm(ALL.df, SPECIES.df)
  
  
  # -- Billfish
  ALL.df <- species_catch_FULL.df %>% 
    filter(ENGLISH_NAME %in% billfish.v) %>% 
    mutate(ENGLISH_NAME = "BILLFISH") %>% 
    calc_species_stats.df()
  
  SPECIES.df <- species_catch_FULL.df %>% 
    filter(ENGLISH_NAME %in% billfish.v) %>% 
    calc_species_stats.df()
  
  BILLFISH.df <- rbind(ALL.df, 
                         SPECIES.df)
  
  rm(ALL.df, SPECIES.df)
  
  
  # -- Incidentals
  ALL.df <- species_catch_FULL.df %>% 
    filter(ENGLISH_NAME %in% incidental.v) %>% 
    mutate(ENGLISH_NAME = "INCIDENTAL") %>% 
    calc_species_stats.df()
  
  SPECIES.df <- species_catch_FULL.df %>% 
    filter(ENGLISH_NAME %in% incidental.v) %>% 
    calc_species_stats.df()
  
  INCIDENTAL.df <- rbind(ALL.df, 
                         SPECIES.df)
  
  rm(ALL.df, SPECIES.df)
  
  
  
  # -- Discards
  ALL.df <- species_catch_FULL.df %>% 
    filter(ENGLISH_NAME %in% discards.v) %>% 
    mutate(ENGLISH_NAME = "DISCARDS") %>% 
    calc_species_stats.df()
  
  SPECIES.df <- species_catch_FULL.df %>% 
    filter(ENGLISH_NAME %in% discards.v) %>% 
    calc_species_stats.df()
  
  DISCARDS.df <- rbind(ALL.df, 
                         SPECIES.df)
  
  rm(ALL.df, SPECIES.df)
  
  
  
  # -- Discards
  ALL.df <- species_catch_FULL.df %>% 
    mutate(ENGLISH_NAME = "ALL_SPP") %>% 
    calc_species_stats.df()
  
  # Rbinding all the relevent tables 
  CATCH_COMPOSITION <- VALUABLE.df %>% 
    rbind(OTHER.df) %>% 
    rbind(OTHER_TUNA.df) %>% 
    rbind(BILLFISH.df) %>% 
    rbind(INCIDENTAL.df) %>% 
    rbind(DISCARDS.df) %>% 
    rbind(ALL.df )
  
  CATCH_COMPOSITION_FINAL <- CATCH_COMPOSITION %>% 
    dplyr::select(ENGLISH_NAME, Pre, Post) %>% 
    mutate(ENGLISH_NAME = str_to_title(gsub("_", " ", ENGLISH_NAME)))
  
  names(CATCH_COMPOSITION_FINAL) <- c("Species", "Pre-expansion", "Post-expansion")
  
  
  #--------
  # Just save straight to the science folder
  print(xtable(CATCH_COMPOSITION_FINAL, type = "latex"), include.rownames=FALSE, 
        file = file.path("Results",
                         "Tables",
                         "Descriptive_Tables",
                         "CATCH_COMPOSITION.txt"))  
  
