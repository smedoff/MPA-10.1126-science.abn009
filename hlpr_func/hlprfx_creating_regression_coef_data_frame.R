

# The purpose of this function is to pull in all regression data frames that are produced
# in the hlpr_func/Regression script/06d_regression_figure_and_df.R
# and compile them into one data frame for the regression figure in the main text

#-------------------
# Example synthax
# rr_specifications <- c("100NM", "200NM", "300NM", "cnt_dist")

creating_regression_coef_data_frame.f <- function(rr_specifications.arg){
  
  # Importing the regression data frames from the hlpr_func/Regression script
  # 06d_regression_figure_and_df.R
  reg_df.l <- lapply(1:length(rr_specifications.arg), FUN = function(i){
    
    rr_specification <- rr_specifications.arg[i]
    
    # Create values for REGION_SPECIFICATION column 
    if(rr_specification == "cnt_dist"){
      rr_specification_lab <- "Continuous 600NN"
    }else{ 
      rr_specification_lab <- paste0("donut_", rr_specification)}
    
    # Creating universal file name 
    source(file.path("hlpr_func", "hlprfx_creating_file_name.R"))
    file_name <- creating_file_name.f(data_set.arg = "Observer Data",
                                      rr_specification.arg = rr_specification,
                                      outcome_var.arg = "standardized_CPUE",
                                      analysis.arg = "real")
    
    species_graphing.df <- readRDS(file = file.path("Results",
                                                    "Figures and Data",
                                                    paste0(file_name, "_Reg.rds"))) %>% 
      mutate(REGION_SPECIFICATION = rr_specification_lab)
    
  })
  
  reg_df.df <- do.call("rbind", reg_df.l)
  
  return(reg_df.df)
  
}

