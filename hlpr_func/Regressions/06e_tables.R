
# This script produces the final latex tables of the diff-in-diff regression

library(tidyverse)
library(dplyr)
library(stargazer)
library(data.table)
library(lmtest)
library(AER)
library(plm)
library(gridExtra)
library(grid)
library(broom)

# ------------------------------------------------------------
# Cleaning R environment
  source(file.path("hlpr_func", "source_cleaning_Renviro.R"))
  rm(list=setdiff(ls(), enviro_vars.v))

# -----------------------------------
# Bringing in the data 
  load(file.path("Data",
                 data_set,
                 "Specification Specific",
                 paste0("06c_", file_name, ".RDS")))
  
  
  source(file.path("hlpr_func",
                   "hlprfx_creating_reg_table.R"))

#=================
# Unpack the regressions
#=================    
  
  for(f in 1:length(species_reg.l)){
    
    one_fe_type <- names(species_reg.l)[f]
    fe.l <- species_reg.l[[one_fe_type]]
    
    # Unpacking the species regressions
    bet.reg <- fe.l[["BIGEYE TUNA"]]
    yf.reg <- fe.l[["YELLOWFIN TUNA"]]
    other.reg <- fe.l[["OTHER"]]
    all.reg <- fe.l[["ALL"]]
    
    # Creating robust standard errors 
    bet.se <- list(sqrt(diag(vcovHC(bet.reg, type = "HC1"))))
    yf.se <- list(sqrt(diag(vcovHC(yf.reg, type = "HC1"))))
    other.se <- list(sqrt(diag(vcovHC(other.reg, type = "HC1"))))
    all.se <- list(sqrt(diag(vcovHC(all.reg, type = "HC1"))))
    
    
    # Creating regression tables 
    create_table.f(bet_reg.arg = bet.reg, 
                   yf_reg.arg = yf.reg, 
                   other_reg.arg = other.reg, 
                   all_reg.arg = all.reg,
                   bet_se.arg = bet.se,
                   yf_se.arg = yf.se,
                   all_se.arg = all.se,
                   other_se.arg = other.se,
                   specification = one_fe_type)
    
    
  }
  
  