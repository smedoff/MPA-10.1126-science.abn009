
# Example synthax 

  # data_set <- "Observer Data"
  # rr_specifications <- "100NM"
  # outcome_var <- "standardized_CPUE"  # relative_CPUE, ln_CPUE, CPUE
  # placebo_test <- FALSE


#------------------------
  creating_file_name.f <- function(data_set.arg = c("Observer Data", "Lobook Data"),
                                   rr_specification.arg = c(paste(c(100, 200, 300), "NM", sep = ""), 
                                                            "cnt"),
                                   outcome_var.arg = c("standardized_CPUE", 
                                                   "CPUE"),
                                   analysis.arg){
    
    data_set.lab <- ifelse(data_set.arg == "Observer Data", "OB", "LB")
  
    outcome_var.lab <- ifelse(outcome_var.arg == "standardized_CPUE", "std", "raw")
    
    
    file_title <- paste0(data_set.lab, "_",
                         outcome_var.lab, "_",
                         analysis.arg, "_",
                         rr_specification.arg)
    
    return(file_title)
    
  }
