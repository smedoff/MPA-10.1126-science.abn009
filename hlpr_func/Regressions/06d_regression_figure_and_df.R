
# This script creates regression figures of the diff-in-diff 
# estimator based on the regression results


# prepare the workspace --------------------------------------------------------
options(scipen=999)
library(tidyverse)
library(dplyr)
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

  
  species_of_interest <- data.frame(ENGLISH_NAME = c("BIGEYE_TUNA",
                                                     "YELLOWFIN_TUNA",
                                                     "ALL",
                                                     "OTHER"))

  source(file.path("hlpr_func",
                   "hlprfx_creating_graphing_df.R"))
  


  
#=================
  # Creating a regression statistic data.frame    
#================= 
  
  species_graphing.l <- lapply(1:length(species_reg.l), FUN = function(f){
    
    fe_type <- names(species_reg.l)[f]
    fe.reg <- species_reg.l[[fe_type]]
    
    creating_graphing_df.f(one_species_reg_results.l = fe.reg,
                           species.df = species_of_interest,
                           specification = fe_type)
    
    
  })
  
  # Turn list into data frame 
  species_graphing.df <- do.call("rbind", species_graphing.l)   
 
  # Cleaning up variables    
  species_graphing.df$PVAL <- factor(species_graphing.df$PVAL, c("0.1%", "1%", "5%", "10%", "Not Sig"))
  
  species_graphing.df$graph_specification <- factor(species_graphing.df$graph_specification, 
                                            levels=c("No FE","Cap MonthYr",#"Seamounts", 
                                                     "Kitchen Sink"), 
                                            labels=c("No FE","Cap MonthYr",#"Seamounts", 
                                                     "Kitchen Sink"))
  
#-------------
#-------------
    
    p <- ggplot(species_graphing.df, aes(SPECIES, DiD_BETA, color = PVAL, group = PVAL)) +
      geom_point(aes(), size = 3) +
      scale_x_discrete(limits=c("BIGEYE TUNA","YELLOWFIN TUNA", "ALL", "OTHER")) +
      geom_errorbar(aes(ymin=DiD_BETA-qnorm(1-.10/2)*DiD_se, ymax=DiD_BETA+qnorm(1-.10/2)*DiD_se),
                    size=.3,
                    width=.2,
                    position=position_dodge(.9)) +
      geom_hline(yintercept = 0, linetype = "dashed")+
      scale_color_manual(values = c("0.1%" = "#018571", 
                                    "1%" = "#80cdc1", 
                                    "5%" = "#dfc27d", 
                                    "10%" = "blue", 
                                    "Not Sig" = "black"), 
                         #labels = c("0.1%" = "0.1% we would obs this coef if the true beta val was 0"),
                         name = "PVAL") +
      theme(plot.title = element_text(size=25),
            axis.title.x=element_blank(),
            axis.text.x = element_text(size = 12, angle = 70, vjust = 1, hjust = 1),
            axis.title.y=element_text(size = 15),
            axis.text.y = element_text(size = 12)) +
     # labs(title = paste0("Diff-in-Diff Estimate: ",donut_width_miles, " Mile Region-Radius")) + 
      ylab("Diff-in-Diff Estimate") + 
      xlab("Species Group") +
      facet_grid(rows = vars(graph_specification)) 
 
    #----------------------
    # Saving figure 
    p
    ggsave(file.path("Results",
                     "Figures and Data",
                     paste0(file_name, "_Reg.png")), 
           device = "png", width = 900/72, height = 550/72, dpi = 72)  
    
    
#------------
  # Saving graphing df 

    saveRDS(species_graphing.df, file = file.path("Results",
                                                  "Figures and Data",
                                                   paste0(file_name, "_Reg.rds")))
    
    