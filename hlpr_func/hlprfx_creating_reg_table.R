
# This function will create a latex regression table for bet, yf, other, and all for 
  # one specifiation (no FE, time FE, seamount FE, kitchen FE)

# Run this function for each FE specification and each region radius specification

library(tidyverse)
library(dplyr)
library(stargazer)
library(data.table)
library(lmtest)
library(AER)
library(plm)
library(gridExtra)
library(grid)
library(broom)

# Example Synthax 
#bet.reg <- bet_noFE
#yf.reg <- yf_noFE
#other.reg <- other_noFE
#all.reg <- all_noFE
#bet.se <- bet_noFE_se
#yf.se <- yf_noFE_se
#other.se <- other_noFE_se
#all.se <- all_noFE_se
#specification <- "kitchenFE" #"timeFE"  "seamountsFE" "kitchenFE"
#analysis_specification = "Logbook"

create_table.f <- function(bet_reg.arg, 
                           yf_reg.arg, 
                           other_reg.arg, 
                           all_reg.arg,
                           bet_se.arg,
                           yf_se.arg,
                           all_se.arg,
                           other_se.arg,
                           specification){
  
  # Creating labels for graph specification 
  graph_specification <- ifelse(specification == "noFE", "No FE",
                                ifelse(specification == "timeFE", "Cap MonthYr",
                                       ifelse(specification == "seamountsFE", "Seamounts",
                                              ifelse(specification == "kitchenFE", "Kitchen Sink"))))
  
  table_notes <- paste0("Each successive column displays results for the 4 species groups Bigeye, ", 
                        "Yellowfin, All, and Other, using a difference-in-difference regression. DID ", 
                        "coefficients measure the effect of fishing close to PMNM after PMNM was expanded ", 
                        "in the 2016. The sample runs from January 6, 2010 to January 11, 2020. ", 
                        "$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                        " Heteroskedasticity-robust standard errors presented in parentheses.")
  
  stargazer(bet_reg.arg, 
            yf_reg.arg, 
            all_reg.arg,
            other_reg.arg, 
            title= paste0(graph_specification, 
                          " (", rr_specification, "): ", 
                          table_notes),
            intercept.bottom = FALSE, 
            label= paste0("tbl:", specification, rr_specification),
            column.labels= c("Bigeye", "Yellowfin", "All", "Other"),
            dep.var.labels.include = FALSE, 
            dep.var.caption = "",
            se = c(bet_se.arg,
                   yf_se.arg,
                   all_se.arg,
                   other_se.arg),
            omit.stat=c("LL","ser","f","adj.rsq"), no.space=TRUE, 
            keep = c("DiD", "EXP_DUMMY", "DIST_TREATMENT"),
            covariate.labels = c("Diff in Diff", "Expansion Dummy", "Distance Dummy"),
            notes.append = FALSE,
            omit.table.layout = "n",
            #notes = "\\parbox[t]{4in}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01 for two-sided t test of statistical significance using heteroskedasticity-robust standard errors.}",
            notes.align = "l",
            #style = "qje",
            float = TRUE,
            model.names= FALSE,
            type = 'text',
            out= file.path("Results",
                           "Tables",
                           paste0(file_name, "_", 
                                  specification,
                                  ".tex"))
            )
  
}
