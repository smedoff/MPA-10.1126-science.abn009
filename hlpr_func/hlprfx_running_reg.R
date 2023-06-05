
# This script has two functions 
  # 1. running regressions on species specific data sets 
    # - this will calc relative CPUE for each speices group 
    # - run regressions 
  # 2. create a df that is combatiple with ggplot2

# -------------------


# -------
# Example synthax 

  #df <- SPECIES_PANEL.df 
  #species.v = species_of_interest
  #specification <- "noFE"

# -------

running_did.f <- function(df, species.v, specification, outcome_var){
  
# Running regressions for 3 different FE specifications 
  
  plm.l <- lapply(1:nrow(species.v), FUN = function(i){
    
    one_species <- species.v$ENGLISH_NAME[i]
    
    one_species_reg.df <- df %>%
      filter(ENGLISH_NAME == one_species)
    
    # --------------------------
    # Calc species-specific standardized CPUE if
    # outcome_var == "standardized_CPUE"
    
    if(outcome_var == "standardized_CPUE"){
      one_species_reg.df <- compute_standardize_CPUE.f(df = one_species_reg.df,
                                                       variable = "CPUE",
                                                       treatment_date = expansion_date,
                                                       species_type = one_species,
                                                       df_date = "DATE")}

    
    # ------------------------------------------------------------    
    
    if(specification == "noFE"){
      
      #-----------------
      # No FE regression
      
      noFE.form <- formula(paste0(outcome_var, " ~ DiD + EXP_DUMMY + DIST_TREATMENT "))
      
      noFE.reg <- lm(noFE.form, one_species_reg.df)
      
      return(noFE.reg)
      
    }else{
      if(specification == "timeFE"){
        
        #------
        # Vessel & Seasonality FE regression
        
        timeFE.form <- formula(paste0(outcome_var, " ~ DiD + EXP_DUMMY + DIST_TREATMENT + factor(SET_MONTHYR)",
                                      " + factor(PERMIT_NUM)"))
        
        timeFE.reg <- plm(timeFE.form, one_species_reg.df, index = c("PERMIT_NUM"), 
                          model = "within", effect="individual")
        
        return(timeFE.reg)
        
      }else{
        
        if(specification == "seamountsFE"){
          
          seamountFE.form <- formula(paste0(outcome_var, " ~ DiD + EXP_DUMMY + DIST_TREATMENT ", 
                                            "+ DEPTH  ", 
                                            "+ factor(SET_MONTHYR) + factor(PERMIT_NUM)"))
          
          seamountFE.reg <- plm(seamountFE.form, one_species_reg.df, 
                                index = c("PERMIT_NUM"), model = "within", effect="individual")

          return(seamountFE.reg)
          
        }else{
          
          if(specification == "kitchenFE"){
            
            #------
            # Kitchen sink regression
            # Covariates depend on data set  
            if(data_set == "Observer Data"){
              kitchenFE.form <- formula(paste0(outcome_var, " ~ DiD + EXP_DUMMY + DIST_TREATMENT ", 
                                               "+ factor(SET_MONTHYR) + factor(PERMIT_NUM) + factor(BAIT_TYPE)", 
                                               "+ SOAK + FLOAT_LINE_DIAMETER + HOOKS_PER_FLT"))
            }
            if(data_set == "Logbook Data"){
              kitchenFE.form <- formula(paste0(outcome_var, " ~ DiD + EXP_DUMMY + DIST_TREATMENT ", 
                                               "+ factor(SET_MONTHYR) + factor(PERMIT_NUM) + factor(BAIT_TYPE)", 
                                               "+ SOAK"))
            }
            
            
            kitchenFE.reg <- plm(kitchenFE.form, one_species_reg.df, index = c("PERMIT_NUM"), 
                                 model = "within", effect="individual")
            
            return(kitchenFE.reg)
            
          }else{
            stop("ERROR: specification argument is not correctly defined")
            
          } #end of kitchenFE condition
        } #end of seamountsFD condition
      } #end of timeFE condition
    } #end of noFE condition
  }) #end of lapply
  
  # Add spaces instead of underscores for list names (i think this is for formating purposes for the table)
  names(plm.l) <- gsub("_", " ", species.v$ENGLISH_NAME, fixed=TRUE)
 
  return(plm.l)
}




