
## The purpose of this function is to clean the HDR data set 

# What this function does 
  # Creates a set id with a 'clean' trip number (left pads tripnumbers with leading zero to make 4 character element)
  # Create flag for trips that have S and D for different sets

# Inputs 
  # HDR_DATASET:  an updated queried HDR data set

# Created Flagged Variables: T values mean the observation should be removed due to an input error 
  # TRIP_KEY_FLG == F: Remove trips that have S for some sets and D for others
  # LONG_FLG == F: Remove sets that pass the 0 longitude line
  # SEQ_FLG == F: Remove sets that occure out of sequence, begin set, end set, begin haul, end haul
  # DATE_FLG == F: Remove sets where departure date comes after return date
  # SOAK_FLG == F: Remove obervations where soak time lasted longer than 3 hours
  # DAYS_AT_SEA_FLG == F: Remove observations where trips were longer than 45 days 
  

#TODO 
  # somethigns wrong with flagged variables.  when i remove all T for everything 
  # except TRIP and LONG, im left with no observatios. 
#-----------------------------  
# Example Synthax
  #HDR_DATASET <- HDR
  #trip_key_flg <- c(T, F) # Keep all flaged and unflagged values 
  #seq_flg <- c(T, F)
  #long_flg <- c(T, F)
  #date_flg <- c(T, F)
  #soak_flg <- c(T, F)
  #days_at_sea_flg <- c(T, F)
  
#-----------------------------
  
  library(dplyr)
  library(tidyverse)

cleaning_HDR.f <- function(HDR_DATASET){
  
  #---------------------------------
  
  source(file.path("hlpr_func", 
                   "hlprfx_creating_date_objects.R"))
  
  source(file.path("hlpr_func", 
                   "hlprfx_converting_decdegree.R"))
  
  source(file.path("hlpr_func", 
                   "hlprfx_cleaning_mixed_vars.R"))
  
  #-------------------------------
  # Clean HDR 
  
  # Correct the trip number to match to the details data set 
  # and Creating identifiers
  HDR_update <- HDR_DATASET %>%
    mutate(TRIPNUM_mod = ifelse(sapply(TRIPNUM, FUN = function(z){nchar(z) == 4}),
                                TRIPNUM,
                                paste0(0, TRIPNUM))) %>%
    mutate(SET_ID = paste0(FLEET, LANDYR, 
                          TRIPNUM, SERIALNUM),
           INTEG_TRIP_KEY = paste0(LANDYR, TRIPNUM_mod)) %>% 
    dplyr::select(-TRIPNUM_mod)
  
  
  # Createing coordinate variables
  coordinate.df <- data.frame(
    SET_ID = HDR_update$SET_ID,
    INTEG_TRIP_KEY = HDR_update$INTEG_TRIP_KEY,
    SET_BEGIN_LAT = converting_decdegree(type = "BSLAT", df = HDR_update),
    SET_BEGIN_LON = converting_decdegree(type = "BSLONG", df = HDR_update),
    SET_END_LAT = converting_decdegree(type = "ESLAT", df = HDR_update),
    SET_END_LON = converting_decdegree(type = "ESLONG", df = HDR_update),
    HAUL_BEGIN_LAT = (converting_decdegree(type = "BHLAT", df = HDR_update))*-1,
    HAUL_BEGIN_LON = converting_decdegree(type = "BHLONG", df = HDR_update), 
    HAUL_END_LAT = converting_decdegree(type = "EHLAT", df = HDR_update),
    HAUL_END_LONG = converting_decdegree(type = "EHLONG", df = HDR_update)
  )
  
  # Creating date variables 
  date.df <- HDR_update %>% 
    dplyr::select(SET_ID, INTEG_TRIP_KEY, 
           SET_YR = SETYR, 
           SET_MON = SETMON, 
           SET_DAY = SETDAY, 
           DEPART_YR, DEPART_MON, DEPART_DAY,
           RETURN_YR, RETURN_MON, RETURN_DAY,
           HAUL_YR = HAULYR, 
           HAUL_MON = HAULMON, 
           HAUL_DAY = HAULDAY) %>% 
    unique() 
  
  date.df$SETBEGIN_DATE <- creating_dates.f(df = date.df, type = "SET")
  date.df$HAULBEGIN_DATE <- creating_dates.f(df = date.df, type = "HAUL")
  
  date.df <- date.df %>%
    dplyr::select(SET_ID, INTEG_TRIP_KEY, SETBEGIN_DATE, HAULBEGIN_DATE)
  
  #Creating duration variables
  date.df$SOAK <- date.df$HAULBEGIN_DATE - date.df$SETBEGIN_DATE
  
  #Creating SOAK variable flags  
  date.df$SOAK_FLG <- ifelse(date.df$SOAK < 0 | date.df$SOAK > 3,
                                         T, F)
  
  # Only keep clean obs and get rid of flagged obs. 
  HDR_cleaned <- HDR_update %>% 
    left_join(date.df) %>% 
    left_join(coordinate.df)
  
  #---------
  # Cleaning variables that are mixed across set_ids
  HDR_cleaned_vars <- HDR_cleaned %>% 
    
    # SETBEGIN_DATE 
    cleaning_mixed_vars.f(variable = "SETBEGIN_DATE", 
                          id_variable = "SET_ID") %>% 
    #SET_BEGIN_LAT
    cleaning_mixed_vars.f(variable = "SET_BEGIN_LAT", 
                          id_variable = "SET_ID") %>% 
    #SET_BEGIN_LON
    cleaning_mixed_vars.f(variable = "SET_BEGIN_LON", 
                          id_variable = "SET_ID") %>% 
    #VESSELNAME
    cleaning_mixed_vars.f(variable = "VESSELNAME", 
                          id_variable = "SET_ID") %>% 
    #SOAK
    cleaning_mixed_vars.f(variable = "SOAK", 
                          id_variable = "SET_ID") %>% 
    #BAIT
    cleaning_mixed_vars.f(variable = "BAIT", 
                          id_variable = "SET_ID") %>% 
    #MAINLINE
    cleaning_mixed_vars.f(variable = "MAINLINE", 
                          id_variable = "SET_ID") %>% 
    #HOOKSSET
    cleaning_mixed_vars.f(variable = "HOOKSSET", 
                          id_variable = "SET_ID") %>% 
    #LIGHTSTICKS
    cleaning_mixed_vars.f(variable = "LIGHTSTICKS", 
                          id_variable = "SET_ID") %>% 
    #FLEET
    cleaning_mixed_vars.f(variable = "FLEET", 
                          id_variable = "SET_ID")
  
  return(HDR_cleaned_vars)
}

