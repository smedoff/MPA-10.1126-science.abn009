
#-------------------------------
# Cleaning the Details data set 

source(file.path("hlpr_func", "hlprfx_cleaning_mixed_vars.R"))

cleaning_DETAIL.f <- function(DETAIL_DATASET){   
  
  DETAILS_cleaned <- DETAIL_DATASET %>% 
    mutate(TRIPNUM_mod = ifelse(sapply(HDR_TRIPNUM, FUN = function(z){nchar(z) == 4}),
                                HDR_TRIPNUM,
                                paste0(HDR_TRIPNUM))) %>%
    mutate(SET_ID = paste0(FLEET, HDR_LANDYR, 
                          TRIPNUM_mod, HDR_SERIALNUM),
           INTEG_TRIP_KEY = paste0(HDR_LANDYR, TRIPNUM_mod)) %>% 
    dplyr::select(-TRIPNUM_mod)
  
  
  #------------
  # Cleaning variables that are mixed entries across set_ids
  DETAIL_cleaned_vars <- DETAILS_cleaned %>% 
    cleaning_mixed_vars.f(variable = "SETDEPTH", 
                          id_variable = "SET_ID") %>% 
    cleaning_mixed_vars.f(variable = "PERMITNUM", 
                          id_variable = "SET_ID") %>% 
    cleaning_mixed_vars.f(variable = "VESSELNAME", 
                          id_variable = "SET_ID")
  
  
  
  
  return(DETAIL_cleaned_vars)
  
  }

