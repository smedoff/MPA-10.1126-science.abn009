
# This function plots species-by-species reg coef for each FE specification
# -------------------

# Calculating relative cpue 

#df <- top20_panel.df
#species_name <- "BIGEYE_TUNA"

calc_rel_CPUE.f <- function(df, species_name){
  
  one_species.df <- df %>%
    filter(ENGLISH_NAME == species_name)
  
  # --------------------------
  # Calc species-specific relatvie CPUE 
  
  # Calc yearly averages to compute relative CPUE
  pre_mean.element <- one_species.df %>% 
    group_by(EXP_DUMMY, donut_id) %>% 
    summarize(AVG = mean(CPUE, na.rm = TRUE)) %>% 
    pull(AVG) %>% 
    .[1]
  
  one_species.df <- one_species.df %>% 
    mutate(relative_CPUE = CPUE/pre_mean.element)
}


# -------
# Example synthax 

#df <- SPECIES_PANEL.df 
#species.v = species_of_interest
#specification <- "noFE"

# -------

running_did_spillover_over_time.f <- function(df, species.v, specification){
  
  # Running regressions for 3 different FE specifications 
  
    plm.l <- lapply(1:nrow(species.v), FUN = function(i){
    
    one_species <- species.v$ENGLISH_NAME[i]
    
    # Calc relative cpue
    one_species_reg.df <- calc_rel_CPUE.f(df, one_species)
    
    # ------------------------------------------------------------    
    
    if(specification == "noFE"){
      
      #-----------------
      # No FE regression
      
      noFE.form <- formula(paste0("relative_CPUE ~ DiD + EXP_DUMMY + DIST_TREATMENT ",
                                  "+ DiD2010 + DiD2011 + DiD2012 + DiD2013 + DiD2014 + DiD2015 + DiDpre",  
                                  "+ DiD2017 + DiD2018 + DiD2019 + DiD2020"))
      
      noFE.reg <- lm(noFE.form, one_species_reg.df)
      
      return(noFE.reg)
      
    }else{
      if(specification == "timeFE"){
        
        #------
        # Vessel & Seasonality FE regression
        
        timeFE.form <- formula(paste0("relative_CPUE ~ DiD + EXP_DUMMY + DIST_TREATMENT ",
                                      "+ DiD2010 + DiD2011 + DiD2012 + DiD2013 + DiD2014 + DiD2015 + DiDpre",  
                                      "+ DiD2017 + DiD2018 + DiD2019 + DiD2020",
                                      "+ factor(SET_MONTHYR) + factor(PERMIT_NUM)"))
        
        timeFE.reg <- plm(timeFE.form, one_species_reg.df, index = c("PERMIT_NUM"), 
                          model = "within", effect="individual")
        
        return(timeFE.reg)
        
      }else{
        
        if(specification == "seamountsFE"){
          
          seamountFE.form <- formula(paste0("relative_CPUE ~ DiD + EXP_DUMMY + DIST_TREATMENT ",
                                            "+ DiD2010 + DiD2011 + DiD2012 + DiD2013 + DiD2014 + DiD2015 + DiDpre",  
                                            "+ DiD2017 + DiD2018 + DiD2019 + DiD2020",
                                            "+ DEPTH + factor(SET_MONTHYR) + factor(PERMIT_NUM)"))
          
          seamountFE.reg <- plm(seamountFE.form, one_species_reg.df, 
                                index = c("PERMIT_NUM"), model = "within", effect="individual")
          
          return(seamountFE.reg)
          
        }else{
          
          if(specification == "kitchenFE"){
            
            #------
            # Kitchen sink regression
            
            
            kitchenFE.form <- formula(paste0("relative_CPUE ~ DiD + EXP_DUMMY + DIST_TREATMENT ",
                                             "+ DiD2010 + DiD2011 + DiD2012 + DiD2013 + DiD2014 + DiD2015 + DiDpre",  
                                             "+ DiD2017 + DiD2018 + DiD2019 + DiD2020",
                                             "+ DEPTH + factor(SET_MONTHYR) + factor(PERMIT_NUM) + factor(BAIT_TYPE)", 
                                             "+ FLOAT_LINE_DIAMETER + HOOKS_PER_FLT"))
            
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




#-------------------------------------------------------------
# Creating a df of regression statistics for each specification 

# -------
# Example synthax 

#one_species_reg_results.l <- plm_noFE.l
#species.df <- species_of_interest
#specification = "noFE"

# -------

creating_spillovertime_graphing_df.f <- function(one_species_reg_results.l, 
                                   species.df = top_20, 
                                   specification = c("noFE", "timeFE", "kitchenFE")){
  
  reg_stats.l <- lapply(1:length(one_species_reg_results.l), 
                        FUN = function(i){
                          
                          one_species_reg_results.plm <- one_species_reg_results.l[[i]]
                          
                          one_species <- names(one_species_reg_results.l[i])
                          
                          reg <- summary(one_species_reg_results.plm)
                          
                          reg_output <- data.frame(reg$coefficients) %>%
                            add_rownames("COEF") %>% 
                            rename(PVAL = 'Pr...t..',
                                   SE = 'Std..Error') %>% 
                            mutate(SPECIES = one_species,
                                   CONF_LEVEL = 1-PVAL) %>% 
                            filter(COEF %in% c("DiD",
                                               "DiD2017",
                                               "DiD2018",
                                               "DiD2019"))
                          
                        #--------
                        # Adding Significance Level 
                          # PVAL <= alpha than estimate is at the alpha stat sig level
                          
                          finding_sig_level.f <- function(PVAL){
                            
                            ifelse(0 < PVAL & PVAL <= 0.001, "0.01%",
                                   ifelse(0.001 < PVAL & PVAL <= 0.01, "1%",
                                          ifelse(0.01 < PVAL & PVAL <= 0.05, "5%",
                                                 ifelse(0.05 < PVAL & PVAL <= 0.1, "10%", "Not Sig"))))
                            
                            
                          }
                        #--------
                          
                          reg_output <- reg_output %>% 
                            mutate(SIGNIFICANCE = finding_sig_level.f(PVAL))
                          
                          
                          return(reg_output)
                        })
  
  # Creating labels for graph specification 
  graph_specification <- if(specification == "noFE"){
    
    "No FE"
    
  }else{
    if(specification == "timeFE"){
      
      "Cap/Month-Yr FE"
      
    }else{
      if(specification == "seamountsFE"){
        
        "Seamounts FE"
        
      }else{
        if(specification == "kitchenFE"){
          
          "Kitchen FE"
          
        }
      }
    }
  }
  
  edit_species.df <- species.df %>%
    dplyr::select(ENGLISH_NAME, LENGTH_cm) %>%
    mutate(ENGLISH_NAME = gsub("_", " ", ENGLISH_NAME),
           graph_specification = graph_specification)
  
  graphing_parameters.df <- do.call(rbind, reg_stats.l) %>%
    left_join(edit_species.df, by = c("SPECIES" = "ENGLISH_NAME")) %>%
    arrange(desc(LENGTH_cm)) %>%
    mutate(GRAPHING_SPECIES = paste0(SPECIES, " (", LENGTH_cm, "ft)")) %>%
    mutate(GRAPHING_SPECIES = gsub("_", " ", GRAPHING_SPECIES, fixed=TRUE))
}

