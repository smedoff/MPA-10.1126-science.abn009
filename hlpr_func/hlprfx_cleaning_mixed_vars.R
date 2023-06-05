
#df <- HDR_cleaned
#variable <- "SETBEGIN_DATE" 
#id_variable <- "SET_ID"

cleaning_mixed_vars.f <- function(df, variable, id_variable){
  
  func_data.df <- df %>% 
    rename(variable = variable, 
           id_variable = id_variable)
  
  #Create flag for mixed variables 
  # e.g., trips that have S and D for different sets
  MIXED_VAR.df <- func_data.df %>%
    group_by(id_variable) %>%
    summarise(n = n_distinct(variable)) %>%
    filter(n > 1)
  
  BAD_OBS.df <- func_data.df %>% 
    filter(id_variable %in% MIXED_VAR.df$id_variable)
  
  GOOD_OBS.df <- func_data.df %>% 
    anti_join(BAD_OBS.df)
  
  # Code Check - make sure the anti_join was done correct 
  if(!(nrow(BAD_OBS.df) + nrow(GOOD_OBS.df) == nrow(func_data.df))){
    stop(print("STOP!! Check the joining function in the 'Cleaning Set Type' code 
               chunk."))
  }
  
  CLEAN_BAD.l <- lapply(MIXED_VAR.df$id_variable,
                                FUN = function(id_var){
                                  
                                  # Grab one integ_trip_key
                                  ONE_BAD_TRIP.df <- BAD_OBS.df %>% 
                                    filter(id_variable == id_var)
                                  
                                  # Figure out the max set_type (which set_type)
                                  # accounts for majority of the observations
                                  max_trip_type <- table(ONE_BAD_TRIP.df$variable) %>% 
                                    data.frame() %>% 
                                    filter(Freq == max(Freq))
                                  
                                  # Assign the majority set_type
                                  ONE_BAD_TRIP.df$variable <- max_trip_type$Var
                                  
                                  return(ONE_BAD_TRIP.df)
                                  
                                })
  
  CLEAN_BAD.df <- do.call("rbind", CLEAN_BAD.l)
  
  df_clean <- rbind(GOOD_OBS.df, 
                    CLEAN_BAD.df) %>% 
    doBy::renameCol("variable", variable) %>% 
    doBy::renameCol("id_variable", id_variable)
  
  # Code Check - make sure we didnt loose/gain any observations in the cleaning
  if(!(nrow(df_clean) == nrow(func_data.df))){
    stop(print("STOP!! Observations were added in the 'Cleaning Set Type'
               code chunk."))
  }
  
  return(df_clean)
  
}