

# This function will create a descriptive statistic table based on JR example emailed 
# on 1/6/21.  You will pass a data frame through, either at the set level or species level.

# Data sets this function is applied to
# - all the region radii data samples 
# - Just in the expansion area 
# - Over the entire sample 

# Example Synthax - you need to pass a label for the region parameter so we 
# can create pretty column labels
#file_titles <- paste0("donut_", c(100, 200, 300), "NM")
#file_titles <- c("Expan_Area", "All_Fishing_Grounds")

create_desc_stat_table.f <- function(df, region){
  
  # Create Column Label
  col_name <- ifelse(region == "100NM", "0-100NM",
                     ifelse(region == "200NM", "0-200NM",
                            ifelse(region == "300NM", "0-300NM",
                                   ifelse(region == "Expan_Area", "Expansion Area",
                                          ifelse(region == "All_Fishing_Grounds", 
                                                 "Historical Fishing Grounds", "0-600NM")))))
  
  #------------
  # Convert species df to set level
  set_level.df <- df %>% 
    mutate(YEAR = substr(SETBEGIN_DATE, 1, 4)) %>% 
    group_by(SET_ID, YEAR) %>% 
    summarize(PERMIT_NUM = unique(PERMIT_NUM),
              HOOKS = unique(NUM_HOOKS),
              SOAK = unique(SOAK),
              HOOKS_PER_FLT = unique(HOOKS_PER_FLT),
              FLOAT_LINE_DIAMETER = unique(FLOAT_LINE_DIAMETER),
              BAIT_TYPE = unique(BAIT_TYPE))
  
  #---------
  # Participation/ Effort
  
  # "On average, there are x vessels per year"
  avg_vessels <- set_level.df %>% 
    group_by(YEAR) %>% 
    summarize(VESSELS = n_distinct(PERMIT_NUM)) %>% 
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
    summarize(HOOKS_1000 = sum(HOOKS, na.rm = TRUE)/1000) %>% 
    summarize(YEARLY_AVG = mean(HOOKS_1000, na.rm = TRUE),
              YEARLY_SD = sd(HOOKS_1000, na.rm = TRUE)) %>% 
    mutate(VARIABLE = "Number of Hooks (1,000)") %>% 
    dplyr::select(VARIABLE, YEARLY_AVG, YEARLY_SD) %>% 
    mutate_if(is.numeric, round, digits = 1) %>% 
    mutate(REPORT = paste0(YEARLY_AVG, " (", YEARLY_SD, ")")) %>% 
    dplyr::select(VARIABLE, REPORT) 
  
  # "On average, fisherman saok their hooks for x hours"
  avg_soak <- data.frame(VARIABLE = "Soak Time (Hours)",
                         YEARLY_AVG = mean(set_level.df$SOAK, na.rm = TRUE),
                         YEARLY_SD = sd(set_level.df$SOAK, na.rm = TRUE)) %>% 
    mutate_if(is.numeric, round, digits = 1) %>% 
    mutate(REPORT = paste0(YEARLY_AVG, " (", YEARLY_SD, ")")) %>% 
    dplyr::select(VARIABLE, REPORT) 
  
  # "On average, there is a total of x hours of soak time per year"
  #avg_soak <- set_level.df %>% 
  #  group_by(YEAR) %>% 
  #  summarize(SOAK = sum(SOAK)) %>% 
  #  summarize(YEARLY_AVG = mean(SOAK),
  #            YEARLY_SD = sd(SOAK)) %>% 
  #  mutate(VARIABLE = "Soak Time (Hours)") %>% 
  #  dplyr::select(VARIABLE, YEARLY_AVG, YEARLY_SD) %>% 
  #  mutate_if(is.numeric, round, digits = 1) %>% 
  #  mutate(REPORT = paste0(YEARLY_AVG, " (", YEARLY_SD, ")")) %>% 
  #  dplyr::select(VARIABLE, REPORT) 
  
  #---------
  # Gear
  avg_hooks_flt <- data.frame(VARIABLE = "Number of Hooks per Float",
                              YEARLY_AVG = mean(set_level.df$HOOKS_PER_FLT, na.rm = TRUE),
                              YEARLY_SD = sd(set_level.df$HOOKS_PER_FLT, na.rm = TRUE)) %>% 
    mutate_if(is.numeric, round, digits = 1) %>% 
    mutate(REPORT = paste0(YEARLY_AVG, " (", YEARLY_SD, ")")) %>% 
    dplyr::select(VARIABLE, REPORT) 
  
  avg_flt_line_diam <- data.frame(VARIABLE = "Float Line Diam.",
                                  YEARLY_AVG = mean(set_level.df$FLOAT_LINE_DIAMETER, na.rm = TRUE),
                                  YEARLY_SD = sd(set_level.df$FLOAT_LINE_DIAMETER, na.rm = TRUE)) %>% 
    mutate_if(is.numeric, round, digits = 1) %>% 
    mutate(REPORT = paste0(YEARLY_AVG, " (", YEARLY_SD, ")")) %>% 
    dplyr::select(VARIABLE, REPORT) 
  
  #------
  # Bait Vars - this variable does not follow the same reporting format at the others
  # instead of reporting mean (sd), we are reporting % composition of bait type across
  # the sample.  Because of this, create a header that describes the columns so that
  # we can rbind it to the other data frames later
  
  # Make sure we include % for all bait in the historical sample
  full_bait_type <- data.frame(BAIT_TYPE = bait_species.v)
  
  # Creating the composition table
  bait_raw <- full_bait_type %>% 
    left_join(data.frame(table(set_level.df$BAIT_TYPE)) %>% 
                rename("BAIT_TYPE" = Var1,
                       "COUNT" = Freq) %>% 
                mutate(PERCENT = COUNT/nrow(set_level.df)*100) %>% 
                dplyr::select(BAIT_TYPE, PERCENT)) %>% 
    replace(is.na(.), 0)
  
  # Making sure the composition of bait adds up to 100% (all sets have a bait assigned)
  if(!(round(sum(bait_raw$PERCENT))==100)){
    stop(print("STOP!!:  % of sets using bait is not adding to 100"))
    }

  # Add % to values for asthetics
  bait_raw <- bait_raw %>% 
    mutate_if(is.numeric, round, digits = 1) %>% 
    #mutate(PERCENT = paste0(PERCENT, "%")) %>% 
    rename(VARIABLE = BAIT_TYPE,
           REPORT = PERCENT)
  
  # Unify the column names to rbind later 
  bait <- data.frame(VARIABLE = "Bait Type (%)",
                     REPORT = " ") %>% 
    rbind(bait_raw)


  # ---------
  # rbind the full descriptive stats table and rename the column wih the right 
  # area label 
  
  descrip_stats.df <- rbind(data.frame(VARIABLE = "Participation: ",
                                       REPORT = " "),
                            avg_vessels,
                            data.frame(VARIABLE = "Effort: ",
                                       REPORT = " "),
                            avg_sets,
                            avg_hooks, 
                            avg_soak, 
                            data.frame(VARIABLE = "Gear: ",
                                       REPORT = " "),
                            avg_hooks_flt,
                            avg_flt_line_diam,
                            bait) %>% 
    doBy::renameCol("VARIABLE", " ") %>% 
    doBy::renameCol("REPORT", col_name)

  
  return(descrip_stats.df)
}
