
# This script will create the regression figure where each panel is a region radii, each 
# cluster of points is a species, and each point shape is a fixed effect specification

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(grid)


# ------------------------------------------------------------
# Cleaning R environment
  rr_specifications <- c(rr_specifications, "cnt_dist")
  enviro_vars.v <- c("data_set", "outcome_var", "rr_specifications",
                     "analysis_type")
  rm(list=setdiff(ls(), enviro_vars.v))
  
  source(file.path("hlpr_func", 
                   "hlprfx_creating_file_name.R"))


#----------
# Bringing each regression data frame 
#-------------
# Bringing in the pre-expansion mean for species x region RR
  
  reg.l <- lapply(1:length(rr_specifications), FUN = function(r){
    
    rr_specification <- rr_specifications[r]
    
    file_name <- creating_file_name.f(data_set.arg = data_set,
                                      rr_specification.arg = rr_specification,
                                      outcome_var.arg = outcome_var,
                                      analysis.arg = analysis_type)
    
    file_name <- paste0(file_name, "_Reg.RDS")
    
    # This data set was made in the hlpr_func/Regression/6c
    # Data is filtered for 10-year sample and D set
    one_reg.df <- readRDS(file.path("Results",
                                    "Figures and Data",
                                    file_name)) %>% 
      mutate(REGION = rr_specification)
    
    
    
  })


  reg.df <- do.call("rbind", reg.l)
  rm(reg.l)





#--------------------
# Separate by Species 
  
 # example synthax 
 # one_region.df <- reg.df %>% 
 #  filter(REGION == "donut_100NM")
  
  # Create graphing function 
  create_reg_graph.f <- function(one_region.df){
    
    one_region <- unique(one_region.df$REGION)
    
    theme_set(theme_bw())
    p <- ggplot(data=one_region.df, 
           mapping = aes(x=factor(SPECIES, level = c('YELLOWFIN TUNA', 'BIGEYE TUNA', 'ALL', 'OTHER')), 
                         y=DiD_BETA, 
                         ymin= DiD_BETA - 1.96*DiD_se, 
                         ymax = DiD_BETA + 1.96*DiD_se, 
                         color=SPECIES,
                         shape=graph_specification)) + 
      geom_hline(yintercept = 0, linetype = "dashed")
    
    one_species_graph.p <- p + 
      geom_pointrange(position = position_dodge2(width=0.5),size=1.2) + 
      theme(legend.position = "none") + 
      
      # Assigning color-blind color palet 
      scale_color_manual(values=c("#000000", # black
                                  "#0072B2", # blue
                                  "#999999", # gray
                                  "#F0E442" )) + # yellow
      labs(x="", y="")
    
    return(one_species_graph.p)
    
  }
  
  #------------
  # 100NM 
  donut_100.p <- create_reg_graph.f(reg.df %>% 
                                      filter(REGION == "100NM"))
  
  #------------
  # 200NM 
  donut_200.p <- create_reg_graph.f(reg.df %>% 
                                      filter(REGION == "200NM"))
  
  #------------
  # 300NM 
  donut_300.p <- create_reg_graph.f(reg.df %>%  
                                      filter(REGION == "300NM"))
  
  #------------
  # Cnt 
  cnt.p <- create_reg_graph.f(reg.df %>%
                                filter(REGION == "cnt_dist"))
  

#-----------------------------
# Save final output 
  
  yleft <- textGrob("Coefficient Estimate", rot = 90, gp = gpar(fontsize = 20))
  
  p <- grid.arrange(donut_100.p, donut_200.p, donut_300.p, cnt.p, 
                    left = yleft, 
                    bottom = " ", 
                    ncol=1)
  
#----------
# Saving files 
  data_set.lab <- ifelse(data_set == "Observer Data", "OB", "LB")
  
  outcome_var.lab <- ifelse(outcome_var == "standardized_CPUE", "std", "raw")
  
  
  file_title <- paste0(data_set.lab, "_",
                       outcome_var.lab, "_",
                       analysis_type, "_",
                       "RegFig")
  
  ggsave(file.path("Results",
                   "Figures and Data",
                   paste0(file_title, "_FINAL.png")), p)
  
  saveRDS(reg.df, file.path("Results",
                            "Figures and Data",
                            paste0(file_title, "_FINAL.RDS")))
  
  
  
  
  
  

