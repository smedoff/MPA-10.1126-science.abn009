#-------------------------------------------------------------
# Creating a df of regression statistics for each specification 

# -------
# Example synthax 

#one_species_reg_results.l <- plm_noFE.l
#species.df <- species_of_interest
#specification = "noFE"

# -------

creating_graphing_df.f <- function(one_species_reg_results.l, 
                                   species.df = top_20, 
                                   specification = c("noFE", "timeFE", "kitchenFE")){
  
  reg_stats.l <- lapply(1:length(one_species_reg_results.l), 
                        FUN = function(i){
                          
                          # Grab name of species for this regression
                          one_species <- names(one_species_reg_results.l[i])
                          
                          # Grab the regression from the list                      
                          one_species_reg_results.plm <- one_species_reg_results.l[[i]]
                          
                          # Create a summary based on robust standard erros
                          reg <- coeftest(one_species_reg_results.plm, vcov. = vcovHC, type = "HC1")
                          reg.df <- tidy(reg)
                          
                          reg_output <- reg.df %>% 
                            filter(term == "DiD") %>% 
                            rename(PVAL = p.value,
                                   Estimate = estimate,
                                   Std.Error = std.error)
                          
                          coef_interest <- reg_output$Estimate
                          
                          se <- reg_output$Std.Error
                          
                          pval_interest <- reg_output$PVAL
                          
                          Conf_Level <- 1-pval_interest
                          
                          one_coef.df <- data.frame(SPECIES = one_species,
                                                    DiD_BETA = coef_interest,
                                                    DiD_se = se,
                                                    PVAL = ifelse(Conf_Level >= .9999 & Conf_Level <= 1, "0.1%",
                                                                  ifelse(Conf_Level < .9999 & Conf_Level >= .99, "1%",
                                                                         ifelse(Conf_Level < .99 & Conf_Level >= .95, "5%",
                                                                                ifelse(Conf_Level < .95 & Conf_Level >= .9, "10%", "Not Sig")))))
                          
                          
                          
                          return(one_coef.df)
                        })
  
  # Creating labels for graph specification 
  graph_specification <- if(specification == "noFE"){
    
    "No FE"
    
  }else{
    if(specification == "timeFE"){
      
      "Cap MonthYr"
      
    }else{
      if(specification == "seamountsFE"){
        
        "Seamounts"
        
      }else{
        if(specification == "kitchenFE"){
          
          "Kitchen Sink"
          
        }
      }
    }
  }
  
  edit_species.df <- species.df %>%
    dplyr::select(ENGLISH_NAME) %>%
    mutate(ENGLISH_NAME = gsub("_", " ", ENGLISH_NAME),
           graph_specification = graph_specification)
  
  graphing_parameters.df <- do.call(rbind, reg_stats.l) %>%
    left_join(edit_species.df, by = c("SPECIES" = "ENGLISH_NAME")) 
  
  return(graphing_parameters.df)
}

