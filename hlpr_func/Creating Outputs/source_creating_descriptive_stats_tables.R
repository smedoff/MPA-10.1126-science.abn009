  
# This script will compile summary statistics for each of the region-radii specifications, the 600NM 
# boundary, and the historical fishing grounds. 

library(tidyverse)
library(dplyr)
library(foreign)
library(xtable)
library(stargazer)

# ------------------------------------------------------------
# Cleaning R environment
  enviro_vars.v <- c("analysis_type", "data_set", 
                     "outcome_var", "rr_specifications")
  rm(list=setdiff(ls(), enviro_vars.v))
  
  source(file.path("hlpr_func",
                   "hlprfx_create_descriptive_stat_table.R"))

#--------------------------
# Bring in the full sample so we can get a complete list of bait species used
  full_species_panel.df <- readRDS(file.path("Data",
                                              data_set,
                                              "05_FullSetPanel_2000_2020.RDS")) %>% 
    mutate(YEAR = as.numeric(substr(SETBEGIN_DATE, 1, 4))) %>%
    filter(YEAR >= 2010 & YEAR < 2020,
           DECLARED_TRIP_TYPE == "D")
  
  bait_species.v <- unique(full_species_panel.df$BAIT_TYPE)

#--------------------------- 
# Descriptive Stats tables for RR 
  
  rr_specifications <- c(rr_specifications, "cnt_dist")
  
  RR_stats.l <- lapply(1:length(rr_specifications), FUN = function(i){
    
    rr_specification <- rr_specifications[i]
    
    # Grabbing universal file name 
    source(file.path("hlpr_func", "hlprfx_creating_file_name.R"))
    file_name <- creating_file_name.f(data_set.arg = data_set,
                                      rr_specification.arg = rr_specification,
                                      outcome_var.arg = outcome_var,
                                      analysis.arg = analysis_type)
    
    donut_sample.df <- readRDS(file.path("Data",
                                         data_set,
                                         "Specification Specific",
                                         paste0("06a_", file_name, ".RDS"))) 

    #-------
    # Create descriptive stats table for one region specification
    create_desc_stat_table.f(df = donut_sample.df, 
                             region = rr_specification)
    
  })
  
  RR_stats.df <- purrr::reduce(RR_stats.l, left_join)
  

  
#--------------------------- 
# Descriptive Stats tables for Expansion Area and Entire fishing grounds
  
  file_titles <- c("Expan_Area", "All_Fishing_Grounds")
  
  source(file.path("hlpr_func", "hlprfx_finding_sets_inside_mpa.R"))
  MPA_sets.df <- identifying_sets_inside_mpa.f(set_df = full_species_panel.df)
  
  
  OTHER_stats.l <- lapply(1:length(file_titles), FUN = function(i){
    
    file_title <- file_titles[i]
    
    print(file_title)
    
    # Bringing in the corresponding data set 
    if(file_title == "Expan_Area"){
      area_sample.df <- MPA_sets.df %>% 
        filter(INSIDE_PMNM == 1)
      
      # We need to make these filtering conditions because when I made the sample, 
      # i wanted to use that df for the % of effort in the monument for another
      # table
      
    }else{
      area_sample.df <- MPA_sets.df
      }
    
    # Creating the statistics table
    create_desc_stat_table.f(df = area_sample.df, 
                             region = file_title)
    
  })
  
  OTHER_stats.df <- purrr::reduce(OTHER_stats.l, left_join)
  
#--------------------------- 
# Combine statistics tables 
  
  FULL_stats.df <- left_join(RR_stats.df, OTHER_stats.df) %>% 
    dplyr::select(" ", "Expansion Area", 
                  "0-100NM", "0-200NM", "0-300NM", "0-600NM",
                  "Historical Fishing Grounds") %>%
    mutate_each(funs(prettyNum(., big.mark=",")))

#--------------------------- 
# Save final latex code as a txt file 
  print(xtable(FULL_stats.df, type = "latex"), include.rownames=FALSE, 
               file = file.path("Results",
                                "Tables",
                                "Descriptive_Tables",
                                "Descriptive_Stats.txt"))
  
  
  # sm edits:  You will have to do some editing once the stats table is saved.  
  
  # If you are using print.xtable the file will save as a txt file
  # If youre using print(xtable(df, type = latex, include.rowname = F), file)
  # itll print as a text file. 
  
  # Copy and paste the latex code directly into your main tex file 
  # You might have to include/specify the number of rows in the final table
 
  # Tables should be formated such that 
  # \begin{tabular}{c|c|c|c}
  # 1 & 2 & 3 & 4 \\
  # 5 & 6 & 7
  # \end{tabular}
  
  # Edits you might need to make 
  # \begin{tabular}{r|c|c|c|c|c}
  # - switch between r, c, l for alignment of text
  # - There should be ""| for each column
  # Make sure each row of tex code does not start or end with &
    
 
  