
# This script will summarize the pre-expansion statistics table by species, diff in diff 
# coefficient for standardized CPUE, and diff in diff coefficient for raw CPUE. 

library(tidyverse)
library(dplyr)
library(foreign)
library(xtable)
library(stargazer)

# ------------------------------------------------------------
# Cleaning R environment
  enviro_vars.v <- c("data_set", "outcome_var", "rr_specifications",
                     "analysis_type")
  rm(list=setdiff(ls(), enviro_vars.v))

  species.v <- c("BIGEYE_TUNA", "YELLOWFIN_TUNA", "OTHER", "ALL")
  
  source(file.path("hlpr_func", 
                   "hlprfx_creating_file_name.R"))
  
  #-------------
  # Bringing in the pre-expansion mean for species x region RR
  
  pre_stats.l <- lapply(1:length(rr_specifications), FUN = function(r){
    
    rr_specification <- rr_specifications[r]
    
    species_stats.l <- lapply(1:length(species.v), FUN = function(s){
      
      one_species <- species.v[s]
      
      file_name <- creating_file_name.f(data_set.arg = data_set,
                                        rr_specification.arg = rr_specification,
                                        outcome_var.arg = outcome_var,
                                        analysis.arg = analysis_type)
      
      file_name <- paste0(file_name, "_",
                         one_species, "_",
                         "PreStats.RDS")
      
      # This data set was made in the hlpr_func/Regression/6c
      # Data is filtered for 10-year sample and D set
      one_species_pre_stats.df <- readRDS(file.path("Results",
                                                    "Figures and Data",
                                                    file_name)) %>% 
        mutate(REGION = rr_specification)
      
      return(one_species_pre_stats.df)})
    
    one_rr_species_stats.df <- do.call("rbind", species_stats.l)
    
  })
  
  
  pre_stats.df <- do.call("rbind", pre_stats.l) %>% 
    mutate_if(is.numeric, round, digits = 2) %>% 
    mutate(PRE_STATS = paste0(PRE_AVG, " (", STANDDEV, ")"),
           SPECIES = str_to_title(gsub("_", " ", SPECIES)),
           REGION = ifelse(REGION == "100NM", "NM100",
                           ifelse(REGION == "200NM", "NM200",
                                  ifelse(REGION == "300NM", "NM300", NA)))) 
  
  rm(pre_stats.l)
  
  
  
  #-------------
  # Pulling in the regression coeficients 
  
  pulling_reg_coef.f <- function(r, cpue_type){
    
    rr_specification <- rr_specifications[r]
    
    file_name <- creating_file_name.f(data_set.arg = data_set,
                                      rr_specification.arg = rr_specification,
                                      outcome_var.arg = cpue_type,
                                      analysis.arg = analysis_type)
    
    file_name <- paste0(file_name, "_Reg.RDS")
    
    one_reg.df <- readRDS(file.path("Results",
                                    "Figures and Data",
                                    file_name)) %>% 
      filter(graph_specification == "Cap MonthYr") %>% 
      mutate(REGION = rr_specification,
             SPECIES = gsub(" ", "_", SPECIES)) %>% 
      mutate_if(is.numeric, round, digits = 2) %>% 
      mutate(SPECIES = str_to_title(gsub("_", " ", SPECIES)),
             REGION = ifelse(REGION == "100NM", "NM100",
                             ifelse(REGION == "200NM", "NM200",
                                    ifelse(REGION == "300NM", "NM300", NA))),
             BETA = paste0(DiD_BETA, " (", DiD_se, ")")) %>% 
      dplyr::select(REGION, SPECIES, BETA)
    
    return(one_reg.df)
    
  }
  
  std_reg_coef.l <- lapply(1:length(rr_specifications), FUN = pulling_reg_coef.f, 
                           cpue_type = "standardized_CPUE")
  
  std_reg_coef.df <- do.call("rbind", std_reg_coef.l) 
  
  raw_reg_coef.l <- lapply(1:length(rr_specifications), FUN = pulling_reg_coef.f, 
                           cpue_type = "CPUE")
  
  raw_reg_coef.df <- do.call("rbind", raw_reg_coef.l) 
  
  
  #---------------------
  # Re shapping
  
  speceis.df <- pre_stats.df %>% 
    dplyr::select(REGION, SPECIES, PRE_STATS) %>% 
    spread(REGION, PRE_STATS)  %>%
    # arrange bet, yf, all, other
    slice(2, 4, 1, 3)  
    #rename('Pre Statistics' = SPECIES)
  
  DiD_sd.df <- std_reg_coef.df %>% 
    dplyr::select(REGION, SPECIES, BETA) %>% 
    spread(REGION, BETA)  %>%
    # arrange bet, yf, all, other
    slice(2, 4, 1, 3)
    #rename('Standardized DID' = SPECIES)
  
  DiD_raw.df <- raw_reg_coef.df %>% 
    dplyr::select(REGION, SPECIES, BETA) %>% 
    spread(REGION, BETA)  %>%
    # arrange bet, yf, all, other
    slice(2, 4, 1, 3)
    #rename('Raw DID' = SPECIES)
  
  pre_stats.df <- data.frame(SPECIES = "Pre-Statistics [average (sd)]:",
                         NM100 = NA,
                         NM200 = NA,
                         NM300 = NA) %>% 
    rbind(speceis.df) %>% 
    rbind(data.frame(SPECIES = "Standardized Diff-in-Diff:",
                     NM100 = NA,
                     NM200 = NA,
                     NM300 = NA) )%>% 
          rbind(DiD_sd.df) %>% 
    rbind(data.frame(SPECIES = "Raw Diff-in-Diff:",
                     NM100 = NA,
                     NM200 = NA,
                     NM300 = NA)) %>% 
    rbind(DiD_raw.df)
  
  names(pre_stats.df) <- c(" ", "0-200NM", "0-400NM", "0-600NM")
  
  
  #---------
  if(data_set == "Observer Data"){
    file_prefex <- "OB"
  }
  
  if(data_set == "Logbook Data"){
    file_prefex <- "LB"
  }

  
  #write_csv(pre_stats.df, file.path("Results",
  #                                        "Tables",
  #                                        "Descriptive_Tables",
  #                                        paste0(file_prefex, "_Pre_Stats_Coef.csv")))
  
  print(xtable(pre_stats.df, type = "latex"), include.rownames=FALSE, 
        file = file.path("Results",
                         "Tables",
                         "Descriptive_Tables",
                         paste0(file_prefex, "Pre_Stats_Coef.txt")))
  
    