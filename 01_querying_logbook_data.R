
#=============
# Title: Querrying Data (Logbook)
# Author: Sarah Medoff 
#=============

# The purpose of this script is to query the NOAA logbook data sets
# LLDS.LLDS_HDR_20210315HAC
# LLDS.LLDS_DETAIL_20210315HAC

library(RODBC) # for connecting to Oracle the SQL way
library(tidyverse) # for data manipulations


ch <- odbcConnect(dsn="PIC",
                  uid=rstudioapi::askForPassword("Oracle user name"),
                  pwd=rstudioapi::askForPassword("Oracle password"),
                  believeNRows=FALSE)



#-------------
# Check the list of data sets under thuis schema
sqlTables(ch, schema='LLDS')

#-------------

  HDR <- sqlQuery(ch, paste("SELECT *",
                            "FROM LLDS.LLDS_HDR_20210315HAC"))
  
  saveRDS(HDR, file.path("Data", 
                         "Logbook Data", 
                         "Source", 
                         "LLDS_HDR_20210315HAC.RDS"))

#-------------

DETAILS <- sqlQuery(ch, paste("SELECT *",
                              "FROM LLDS.LLDS_DETAIL_20210315HAC"))

saveRDS(DETAILS, file.path("Data", 
                           "Logbook Data", 
                           "Source", 
                           "LLDS_DETAIL_20210315HAC.RDS"))


