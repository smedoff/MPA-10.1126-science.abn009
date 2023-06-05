 
# This function calculates offsets for c("BIGEYE_TUNA", "YELLOWFIN_TUNA")


# Example synthax 
# data_set.arg <- "Logbook Data" 
  
creating_one_rr_offset_df.f <- function(j){
  
  rr_specification <- rr_specifications[j]
  
  # Creating universal file name 
  source(file.path("hlpr_func", "hlprfx_creating_file_name.R"))
  file_name <- creating_file_name.f(data_set.arg = data_set,
                                    rr_specification.arg = rr_specification,
                                    outcome_var.arg = outcome_var,
                                    analysis.arg = analysis_type)
  
#----------
# Creating the DiD_BETA portion of the data frame 
  
  # Pull in the regression list from hlpr_func/Regression/06c
  load(file.path(".", 
                 "Data",
                 data_set,
                 "Specification Specific",
                 paste0("06c_", file_name, ".rds")))
  
  # Create the coef portion for each fe_type 
  FE_COEF.l <- lapply(1:length(fe_type),
                      FUN = function(f){
                        
                        one_fe_type <- fe_type[f]
                        
                        # Grabbing the species regressions
                        bet.reg <- species_reg.l[[one_fe_type]][["BIGEYE TUNA"]]
                        yf.reg <- species_reg.l[[one_fe_type]][["YELLOWFIN TUNA"]]
                        
                        # Pulling out the DiD coef 
                        bet.coef <- summary(bet.reg)$coefficients["DiD", "Estimate"]
                        yf.coef <- summary(yf.reg)$coefficients["DiD", "Estimate"]
                        
                        ONE_FE_COEF.df <- data.frame(
                          SPECIES = c("BIGEYE_TUNA", "YELLOWFIN_TUNA"),
                          DiD_BETA = c(bet.coef, yf.coef),
                          SPECIFICATION = rr_specification,
                          FE_SPECIFICATION = one_fe_type)
                        
                        return(ONE_FE_COEF.df)
                      })
  
  # Bind them together to get one data frame 
  FE_COEF.df <- do.call("rbind", FE_COEF.l)
  
#----------
# Creating the fishing activity in the 'near' area 
  
  donut_sample.df <- readRDS(file.path("Data", 
                                       data_set,
                                       "Specification Specific",
                                       paste0("06a_", file_name, ".RDS")))
  
  near_sets.df <- donut_sample.df %>% 
    filter(donut_id == "small",
           ENGLISH_NAME %in% c("BIGEYE_TUNA", "YELLOWFIN_TUNA")) %>% 
    group_by(YEAR = SET_YR, SPECIES = ENGLISH_NAME) %>% 
    summarize(HOOKS = sum(NUM_HOOKS, na.rm = TRUE),
              NUM_CAUGHT = sum(NUM_CAUGHT, na.rm = TRUE))
  
#----------
# Merging them all together 
  
  offset.df <- near_sets.df %>% 
    left_join(FE_COEF.df) %>% 
    mutate(SPECIES = gsub("_", " ", SPECIES))
  
  return(offset.df)
}





