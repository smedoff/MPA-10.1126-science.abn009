

# Calculating off-sets 

library(tidyverse)
library(dplyr)
library(plm)

rm(list=setdiff(ls(), c("data_set")))

#---------------------------------------
# Sourcing helper functions 
source(file.path("hlpr_func", 
                 "hlprfx_calculating_offsets.R"))


#---------------------------------------
# Setting Parameters 

# Region specifications
rr_specifications <- paste(c(100, 200, 300), "NM", sep = "")

# Outcome variable
outcome_var <- "raw"  # relative_CPUE, ln_CPUE, CPUE

# Expansion date 
expansion_date <- as.Date("2016-08-26")

# Real Analysis, Placebo Test, Sensitivity Analysis? 
analysis_type <- "real"

# Fixed Effects Specifications 
fe_type <- c("timeFE")



#---------------------------------------
# Creating a final_file_name for the final saved data 
  data_file <- ifelse(data_set == "Observer Data", "OB", "LB")
  outcome_file <- ifelse(outcome_var == "standardized_CPUE", "std", "raw")
  final_file_name <- paste0(data_file, "_",
                            outcome_file, "_",
                            analysis_type)



#---------------------------------------
# Creating the offset data frame 

  OFF_SET.l <- lapply(1:length(rr_specifications), FUN = creating_one_rr_offset_df.f)
  OFF_SET.df <- do.call("rbind", OFF_SET.l)
  
  write_csv(OFF_SET.df, file.path("Results",
                                 "Figures and Data",
                                 paste0(final_file_name, "_offset.csv")))

#---------------------------------------
# Calculate fishing activity inside MPA by year 
  
  full_species_panel.df <- readRDS(file.path("Data",
                                             data_set,
                                             "05_FullSetPanel_2000_2020.RDS")) %>% 
    mutate(YEAR = as.numeric(substr(SETBEGIN_DATE, 1, 4))) %>%
    filter(YEAR >= 2010 & YEAR < 2020,
           DECLARED_TRIP_TYPE == "D")
  
  source(file.path("hlpr_func", "hlprfx_finding_sets_inside_mpa.R"))
  inside_sets.df <- identifying_sets_inside_mpa.f(set_df = full_species_panel.df)
  
  inside_sets_yearly.df <- inside_sets.df %>% 
    filter(ENGLISH_NAME %in% c("BIGEYE_TUNA", "YELLOWFIN_TUNA"),
           INSIDE_PMNM == 1) %>% 
    group_by(YEAR, SPECIES = ENGLISH_NAME) %>% 
    summarize(n_SETS = n_distinct(SET_ID),
              HOOKS = sum(NUM_HOOKS, na.rm = TRUE),
              NUM_CAUGHT = sum(NUM_CAUGHT, na.rm = TRUE))
  
  write_csv(inside_sets_yearly.df, file.path("Results",
                                        "Figures and Data",
                                        paste0(final_file_name, "_inside_MPA_catch.csv")))
  