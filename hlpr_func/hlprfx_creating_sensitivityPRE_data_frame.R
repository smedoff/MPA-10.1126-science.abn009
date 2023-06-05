

# The purpose of this function is to create a data frame that summarizes the 
# regression outputs for the "pre-expansion start year" sensitivity analysis 
library(plm)
library(tidyverse)
library(dplyr)

creating_pre_expansion_sensitivity_data_frames.f <- function(one_year.arg){
  
  # Define the parameters 
  data_set <- "Observer Data"
  rr_specifications <- c("100NM", "200NM", "300NM", "cnt_dist")
  outcome_var <- "standardized_CPUE"
  analysis_type <- "sensitivityPRE"
  
  # For one start year
  pre_expansion_start_yr <- one_year.arg
  
  # ------------------
  # For each rr specification 
  # Resulting list will be for one year, each layer will be a region radii 
  one_start_year.l <- lapply(1:length(rr_specifications), FUN = function(r){
    
    rr_specification <- rr_specifications[r]
    
    #---------------------
    # Creating universal file name 
    source(file.path("hlpr_func", "hlprfx_creating_file_name.R"))
    file_name <- creating_file_name.f(data_set.arg = data_set,
                                      rr_specification.arg = rr_specification,
                                      outcome_var.arg = outcome_var,
                                      analysis.arg = analysis_type)
    
    file_name <- paste0(file_name, "_", pre_expansion_start_yr)
    
    #---------------------
    # Bring in the "pre-expansion start"/"region radii" specific regressions 
    load(file.path("Data",
                    data_set,
                    "Specification Specific",
                    paste0("06c_", file_name, ".RDS")))
    
    # Pulling out the regression coef 
    bet.reg <- species_reg.l[["timeFE"]][["BIGEYE TUNA"]]
    yf.reg <- species_reg.l[["timeFE"]][["YELLOWFIN TUNA"]]
    all.reg <- species_reg.l[["timeFE"]][["ALL"]]
    other.reg <- species_reg.l[["timeFE"]][["OTHER"]]
    
    # Pulling out the DiD coef 
    bet.coef <- summary(bet.reg)$coefficients["DiD", "Estimate"]
    yf.coef <- summary(yf.reg)$coefficients["DiD", "Estimate"]
    all.coef <- summary(all.reg)$coefficients["DiD", "Estimate"]
    other.coef <- summary(other.reg)$coefficients["DiD", "Estimate"]
    
    # Creating robust standard errors (only extract the DiD se)
    bet.se <- list(sqrt(diag(vcovHC(bet.reg, type = "HC1"))))[[1]]["DiD"]
    yf.se <- list(sqrt(diag(vcovHC(yf.reg, type = "HC1"))))[[1]]["DiD"]
    all.se <- list(sqrt(diag(vcovHC(all.reg, type = "HC1"))))[[1]]["DiD"]
    other.se <- list(sqrt(diag(vcovHC(other.reg, type = "HC1"))))[[1]]["DiD"]
    
    # Creating data frame that has 
    # beta, se, region radii, start year, and species 
    sensitivity_one.df <- data.frame(SPECIES = c("BET", "YF", "ALL", "OTHER"),
                                     START_YR = pre_expansion_start_yr,
                                     REGION = rr_specification, 
                                     BETA = c(bet.coef, yf.coef, all.coef, other.coef),
                                     SE = c(bet.se, yf.se, all.se, other.se)) %>% 
      mutate(BETA = BETA,
             SE = SE)
    
    return(sensitivity_one.df)
  })
  
  # Compile each region-radii into a data frame. 
  # The resulting data frame will have regession stats for one start year
  # for each region radii.  The final function should be looped through years
  one_start_year.df <- do.call("rbind", one_start_year.l)
  
  return(one_start_year.df)
}