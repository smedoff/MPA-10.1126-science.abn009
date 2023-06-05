
# This script tries to visualize spillovers by plotting a parallel trends 
# plot of relative CPUE 


# prepare the workspace --------------------------------------------------------
options(scipen=999)
library(tidyverse)
library(dplyr)
library(stargazer)
library(data.table)
library(lmtest)
library(AER)
library(plm)
library(gridExtra)
library(grid)

# ------------------------------------------------------------
# Cleaning R environment
  source(file.path("hlpr_func", "source_cleaning_Renviro.R"))
  rm(list=setdiff(ls(), enviro_vars.v))
  
  
#-------
  source(file.path("hlpr_func",
                   "hlprfx_plotting_spillovers_over_time.R"))

# --------------------------
# Bringing in the data 
  
  donut_sample.df <- readRDS(file.path("Data", 
                                       data_set,
                                       "Specification Specific",
                                       paste0("06a_", file_name, ".RDS")))
  
  
  species_of_interest.v <- c("BIGEYE_TUNA", "YELLOWFIN_TUNA")


#---------
# Calculate the standardized difference between 'near' and 'far'
  
  panel.df <- donut_sample.df %>% 
    group_by(donut_id, SET_YR, ENGLISH_NAME) %>% 
    summarize(CPUE = mean(CPUE, na.rm = TRUE)) %>% 
    mutate(SPECIES = gsub("_", " ", ENGLISH_NAME, fixed=TRUE)) %>% 
    spread(donut_id, CPUE) %>% 
    mutate(large = replace_na(large, 0),
           small = replace_na(small, 0),
           DIFF_DONUT = small - large)
  
  pre_exp_stats.df <- panel.df %>% 
    filter(SET_YR <= substr(expansion_date, 1, 4)) %>% 
    group_by(SPECIES) %>% 
    dplyr::summarize(avg = mean(DIFF_DONUT, na.rm = TRUE),
                     standdev = stats::sd(DIFF_DONUT, na.rm = TRUE))
  
  panel.df <- panel.df %>% 
    left_join(pre_exp_stats.df) %>% 
    mutate(std_DIFF = (DIFF_DONUT - avg)/standdev)
  
  

#------------
  # Bind data sets
  yr_panel.df <- panel.df %>% 
    filter(!(SET_YR == 2020),
           ENGLISH_NAME %in% c(species_of_interest.v, "ALL")) %>% 
    mutate(SPECIES = ifelse(SPECIES == "YELLOWFIN TUNA", "YFT",
                            ifelse(SPECIES == "BIGEYE TUNA", "BET", 
                                   "ALL")))
  
  saveRDS(yr_panel.df, file.path("Results", 
                                 "Figures and Data",
                                 paste0(file_name, "_TimeTrend.rds")))
  
    


#--------------
# Creating time trend plots  
  
  p <-  ggplot(yr_panel.df, aes(SET_YR, std_DIFF, group = SPECIES, color = SPECIES)) + 
    geom_line(aes(linetype= SPECIES, size = SPECIES)) +
    scale_linetype_manual(values=c("longdash", "solid", "solid", "solid", "solid")) +
    scale_size_manual(values = c(1.2, 1.5, 1.5, 1.5, 1.5) ) + 
    scale_color_manual(values=c("black", "#0072B2", "#F0E442"))+ 
    #labs(title = paste0(mpa_folder, " ", donut_width_miles, ": Comparing Fishing Activity Across Boundaries")) + 
    geom_vline(xintercept = substr(expansion_date, 1, 4), color = "black", linetype = "solid") +
    ylim(-3, 5.5) + 
    theme_set(theme_bw()) +
    theme(plot.title = element_text(size=23),
          axis.title.x=element_blank(), 
          axis.text = element_text(size = 8), 
          axis.title = element_text(size = 8)) + 
    ylab("Difference in CPUE btw. Near & Far Regions") +
    scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) 
  
  # Only add legend to the bottom of the last mi specification 
  if(rr_specification == 100){
    
    p +
      theme(legend.title = element_blank(),
            legend.position = c(0.87, 0.15),
            legend.text = element_text(size = 10),
            legend.key.size = unit(.5, "cm"),
            legend.key.width = unit(0.5,"cm")) + 
      guides(shape = guide_legend(override.aes = list(size = 0.5)),
             color = guide_legend(override.aes = list(size = 0.5)))
    ggsave(file = file.path("Results", 
                            "Figures and Data",
                            paste0(file_name, "_TimeTrend.png")), 
           device = "png", height = 5*2, width = 6.75*2, units = "in")

    
  }else{
    
    p +
      theme(legend.position = "none")
    ggsave(file = file.path("Results", 
                            "Figures and Data",
                            paste0(file_name, "_TimeTrend.png")), 
           device = "png", height = 5*2, width = 6.75*2, units = "in")
    
    
  }

  
 