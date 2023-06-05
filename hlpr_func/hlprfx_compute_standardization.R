
# This script to used to calc relative CPUE 
  # itll output two elements 
  # 1) the df with an added "relative CPUE" column 
  # 2) a vector in which the second elemtn is the pre-exp mean

#--------------
# Example Synthax
   #df = SPECIES_PANEL.df
   #variable = "SET_CPUE"
   #treatment_date = expansion_date
   #df_date = "DATE"
   #species_type = "All" 

#--------------
compute_standardize_CPUE.f <- function(df, 
                                 variable, 
                                 treatment_date,
                                 species_type, 
                                 df_date = "DATE"){
  
  # Find pre expansion mean 
  function.df <-  df %>% 
    rename(DATE = df_date,
           VAR = variable) 
  
  stats.df <- function.df %>% 
    filter(DATE < treatment_date) %>% 
    group_by(SET_ID) %>% 
    summarize(VAR = unique(VAR)) %>% 
    summarize(AVG = mean(VAR, na.rm = TRUE),
              standdev = stats::sd(VAR, na.rm = TRUE)) 
  
  pre_mean.element <- stats.df %>% 
    pull(AVG) %>% 
    .[1]
  
  pre_sd.element <- stats.df %>% 
    pull(standdev) %>% 
    .[1]
  
  function.df <- function.df %>%
    mutate(standardized_CPUE = (VAR - pre_mean.element)/pre_sd.element)
  
  #---------------
  # Code check - make sure standardizations worked 
  test_panel.df <- function.df %>% 
    filter(DATE < expansion_date) %>% 
    dplyr::select(standardized_CPUE) %>% 
    summarize(AVG = round(mean(standardized_CPUE, na.rm = TRUE)),
              SD = round(stats::sd(standardized_CPUE, na.rm = TRUE)))
  
  if(!(test_panel.df$AVG == 0) | !(test_panel.df$SD == 1)){
       stop("STOP!!!!  standardization was not computed correct")
     }
  
  #-------------
  # Saving mean - For main observer specification 
  
  if(analysis_type == "real"){
    
    # Creating a df for statistic 
    pre_mean.df <- data.frame(SPECIES = species_type,
                              PRE_AVG = pre_mean.element,
                              STANDDEV = pre_sd.element)
    
    
    # Working dir - Save pre mean data 
    saveRDS(pre_mean.df, file.path("Results",
                                   "Figures and Data",
                                   paste0(file_name, "_", 
                                          species_type, "_",
                                          "PreStats.RDS")))
    
  }
  

  return(function.df)
  
}
