  
# This script will create time series graphs of the covariates used in the gear-control regressions

rm(list = ls())

library(tidyverse)
library(dplyr)

#---------------------------------------
# Setting Parameters 
  
  # Data set used 
  data_set <- "Observer Data"
  
  # Region specifications
  rr_specifications <- paste(c(100, 200, 300), "NM", sep = "")
  
  # Outcome variable
  outcome_var <- "standardized_CPUE"  # relative_CPUE, ln_CPUE, CPUE
  
  # Expansion date 
  expansion_date <- as.Date("2016-08-26")
  
  # Real Analysis, Placebo Test, Sensitivity Analysis? 
  analysis_type <- "real"
  


#---------------------------------------
# Loop through region radii specifications

for(j in 1:length(rr_specifications)){
  
  rr_specification <- rr_specifications[j]
  
  # Creating universal file name 
  source(file.path("hlpr_func", "hlprfx_creating_file_name.R"))
  file_name <- creating_file_name.f(data_set.arg = data_set,
                                    rr_specification.arg = rr_specification,
                                    outcome_var.arg = outcome_var,
                                    analysis.arg = analysis_type)
  
  
  # This data set was made in the hlpr_func/Regression/6a
  # Data is filtered for 10-year sample and D set
  donut_sample <- readRDS(file.path("Data", 
                                  data_set,
                                  "Specification Specific",
                                  paste0("06a_", file_name, ".RDS")))
  
  #---------------
  # Create a set level df with covariates used 
  set.df <- donut_sample %>% 
    dplyr::select(DATE = SETBEGIN_DATE, SET_ID, donut_id, BAIT_TYPE, 
           FLOAT_LINE_DIAMETER, HOOKS_PER_FLT, SOAK, NUM_HOOKS) %>% 
    unique() %>% 
    mutate(YEAR = as.Date(substr(DATE, 1,4), "%Y"),
           donut_id = ifelse(donut_id == "large", "Far", "Near"))  %>% 
    filter(!(YEAR == as.Date("2020", "%Y"))) 
  
  # Code check:  make sure all observations have a unique set_id
  if(!(n_distinct(donut_sample$SET_ID) == nrow(set.df))){
    print("STOP!! SET_IDs are not unique across observations")
  }
  
  # Create a yearly data set 
  cov_yr.df <- set.df %>% 
    group_by(donut_id, YEAR) %>% 
    summarize(Avg_Hooks_per_Float = mean(HOOKS_PER_FLT, na.rm = TRUE),
              Total_Hooks = sum(NUM_HOOKS, na.rm = TRUE)/100000,
              Avg_Soak_Time = mean(SOAK, na.rm = TRUE),
              Total_Sets = n_distinct(SET_ID)) %>% 
    gather(COVARIATE, 
           VALUE, 
           Avg_Hooks_per_Float, Total_Hooks, Avg_Soak_Time, Total_Sets) %>% 
    mutate(COVARIATE = gsub("_", " ", COVARIATE),
           COVARIATE = ifelse(COVARIATE == "Total Hooks", 
                              "Total Hooks (100,000)", 
                              COVARIATE),
           COVARIATE = ordered(as.factor(COVARIATE), 
                                    levels = c("Total Sets", "Total Hooks (100,000)",
                                               "Avg Soak Time", "Avg Hooks per Float")))
  
  # Saving data set 
  saveRDS(cov_yr.df, file.path("Results",
                               "Figures and Data",
                               paste0(file_name, "_COVARIATES_TS.RDS")))
  
  # Plot these yearly values 
  ggplot(cov_yr.df, aes(YEAR, VALUE)) + 
    geom_line(aes(color = donut_id, group = donut_id)) + 
    facet_wrap(.~COVARIATE, scales = "free_y") + 
    geom_vline(xintercept = as.Date("2016", "%Y")) + 
    ylab(" ") + 
    xlab(" ") + 
    theme(legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=20),
          axis.text = element_text(size = 10),
          strip.text.x = element_text(size = 17))
  ggsave(file.path("Results",
                   "Figures and Data",
                   paste0(file_name, "_COVARIATES_TS.png")))
  
}
  
  
  
  
  