



# This function will create a descriptive statistic table based on JR example emailed 
# on 1/6/21.  You will pass a data frame through, either at the set level or species level.

# Data sets this function is applied to
# - the four different directional space shifts 


create_desc_stat_table.f <- function(df, space_shift.arg){
  
  # Create Column Label
  col_name <- ifelse(space_shift.arg == "N", "North ",
                     ifelse(space_shift.arg == "S", "South ",
                            ifelse(space_shift.arg == "E", "East ",
                                   ifelse(space_shift.arg == "W", "West ", NA))))
  
  #------------
  # Make sure we have a complete yearly data frame 
  # This was brought to light when analysing the western shift. 
  # There was no fishing activity in 2014 or 2017 in the western area, throwing
  # of the yearly averages 
  YEAR.df <- data.frame(YEAR = 2010:2019)

  #------------
  # Convert species df to set level
  set_level.df <- df %>% 
    mutate(YEAR = as.numeric(substr(SETBEGIN_DATE, 1, 4))) %>% 
    group_by(SET_ID, YEAR) %>% 
    summarize(PERMIT_NUM = unique(PERMIT_NUM),
              HOOKS = unique(NUM_HOOKS))
  
  #---------
  # Participation/ Effort
  
  # "On average, there are x vessels per year"
  avg_vessels <- set_level.df %>% 
    group_by(YEAR) %>% 
    summarize(VESSELS = n_distinct(PERMIT_NUM)) %>% 
    right_join(YEAR.df) %>%
    replace(is.na(.), 0) %>% 
    summarize(YEARLY_AVG = mean(VESSELS),
              YEARLY_SD = sd(VESSELS)) %>% 
    mutate(VARIABLE = "Number of Vessels") %>% 
    dplyr::select(VARIABLE, YEARLY_AVG, YEARLY_SD) %>% 
    mutate_if(is.numeric, round, digits = 1) %>% 
    mutate(REPORT = paste0(YEARLY_AVG, " (", YEARLY_SD, ")")) %>% 
    dplyr::select(VARIABLE, REPORT) 
  
  #---------
  # Effort
  
  # "On average, there are x sets made per year"
  avg_sets <- set_level.df %>% 
    group_by(YEAR) %>% 
    summarize(SET_ID = n_distinct(SET_ID)) %>% 
    right_join(YEAR.df) %>%
    replace(is.na(.), 0) %>% 
    summarize(YEARLY_AVG = mean(SET_ID),
              YEARLY_SD = sd(SET_ID)) %>% 
    mutate(VARIABLE = "Number of Sets") %>% 
    dplyr::select(VARIABLE, YEARLY_AVG, YEARLY_SD) %>% 
    mutate_if(is.numeric, round, digits = 1) %>% 
    mutate(REPORT = paste0(YEARLY_AVG, " (", YEARLY_SD, ")")) %>% 
    dplyr::select(VARIABLE, REPORT) 
  
  # "On average, fisherman us x (1,000) hooks per set"
  #avg_hooks <- data.frame(VARIABLE = "Number of Hooks (1,000)",
  #                        YEARLY_AVG = mean(set_level.df$HOOKS,na.rm = TRUE)/1000,
  #                        YEARLY_SD = sd(set_level.df$HOOKS/1000, na.rm = TRUE)) %>% 
  #  mutate_if(is.numeric, round, digits = 1) %>% 
  #  mutate(REPORT = paste0(YEARLY_AVG, " (", YEARLY_SD, ")")) %>% 
  #  dplyr::select(VARIABLE, REPORT) 
  
  # "On average, there are x (1,000) hooks deployed per year by the fleet"
  avg_hooks <- set_level.df %>% 
    group_by(YEAR) %>% 
    summarize(HOOKS_1000 = sum(HOOKS, na.rm = TRUE)/1000)  %>% 
    right_join(YEAR.df) %>%
    replace(is.na(.), 0) %>% 
    summarize(YEARLY_AVG = mean(HOOKS_1000, na.rm = TRUE),
              YEARLY_SD = sd(HOOKS_1000, na.rm = TRUE)) %>% 
    mutate(VARIABLE = "Number of Hooks (1,000)") %>% 
    dplyr::select(VARIABLE, YEARLY_AVG, YEARLY_SD) %>% 
    mutate_if(is.numeric, round, digits = 1) %>% 
    mutate(REPORT = paste0(YEARLY_AVG, " (", YEARLY_SD, ")")) %>% 
    dplyr::select(VARIABLE, REPORT) 
  
  
  # ---------
  # rbind the full descriptive stats table and rename the column wih the right 
  # area label 
  
  descrip_stats.df <- rbind(data.frame(VARIABLE = "Participation: ",
                                       REPORT = " "),
                            avg_vessels,
                            data.frame(VARIABLE = "Effort: ",
                                       REPORT = " "),
                            avg_sets,
                            avg_hooks) %>% 
    doBy::renameCol("VARIABLE", " ") %>% 
    doBy::renameCol("REPORT", col_name)
  
  
  return(descrip_stats.df)
}