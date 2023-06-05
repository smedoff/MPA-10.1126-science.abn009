
#=============
# Title: Querrying Data (Observer)
# Author: Sarah Medoff 
#=============

# The purpose of this script is to query the NOAA observer data sets
# newobs.LDS_CATCH_V
# newobs.LDS_SET_ENVIRON_V
# newobs.LDS_TRIPS_V
# newobs.LOP_GEAR_CFG_V

library(RODBC) # for connecting to Oracle the SQL way
library(tidyverse) # for data manipulations

ch <- odbcConnect(dsn="PIC",
                  uid=rstudioapi::askForPassword("Oracle user name"),
                  pwd=rstudioapi::askForPassword("Oracle password"),
                  believeNRows=FALSE)



#-------------
# Check the list of data sets under thuis schema
  sqlTables(ch, schema='NEWOBS')

#-------------
  
  LDS_CATCH_V <- sqlQuery(ch, paste("SELECT *",
                                    "FROM newobs.LDS_CATCH_V"))
  
  saveRDS(LDS_CATCH_V,  
          file.path("Data", 
                    "Observer Data", 
                    "Source", 
                    "newobs_LDS_CATCH_V.RDS"))

#-------------
  
  LDS_SET_ENVIRON_V <- sqlQuery(ch, paste("SELECT *",
                                          "FROM newobs.LDS_SET_ENVIRON_V"))
  
  saveRDS(LDS_SET_ENVIRON_V,  
          file.path("Data", 
                    "Observer Data", 
                    "Source", 
                    "newobs_LDS_SET_ENVIRON_V.RDS"))

#-------------
  
  LDS_TRIPS_V <- sqlQuery(ch, paste("SELECT *",
                                    "FROM newobs.LDS_TRIPS_V"))
  
  saveRDS(LDS_TRIPS_V,  
          file.path("Data", 
                    "Observer Data", 
                    "Source", 
                    "newobs_LDS_TRIPS_V.RDS"))


#-------------
  
  LOP_GEAR_CFG_V <- sqlQuery(ch, paste("SELECT *",
                                       "FROM newobs.LOP_GEAR_CFG_V"))
  
  saveRDS(LOP_GEAR_CFG_V,  
          file.path("Data", 
                    "Observer Data", 
                    "Source", 
                    "newobs_LOP_GEAR_CFG_V.RDS"))

