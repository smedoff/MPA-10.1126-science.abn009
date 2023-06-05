
# Finding distance to monument based off of begin set coordinates.  This will be used to 
# identify the sets made near to the monument and far from the monument for each 
# region radii specification.

# The DIST variable is measured in nautical miles (NM)


# prepare the workspace --------------------------------------------------------

library(rgdal)
library(sp)
library(rgeos)
library(tidyverse)
library(raster)
library(sf)
library(spatialEco)
library(ggmap)
library(hexbin)
library(maps)
library(mapdata)
library(gridExtra)

  
# ------------------------------------------------------------
source(file.path("Monument Shapefiles", "hlpr_func", "st_rotate.R"))
source(file.path("Monument Shapefiles", "hlpr_func", "sfc_as_cols.R"))
  
  desired_proj <- "+proj=utm +zone=8 +south +datum=WGS84 +units=m +no_defs"

# ------------------------------------------------------------
# Read in source files
  
  # Catch data frame
  species_catch_FULL.df <- readRDS(file.path(".",
                                             "Data",
                                             data_set,
                                             "02_species_catch_FULL_df.RDS")) 

  
  # Spatial set shapefile
  set.sf <- readRDS(file.path(".",
                              "Data",
                              data_set,
                              "03_set_sf.RDS"))
  
  # PMNM shapfile
  MPA.sf <- readRDS(file.path("Monument Shapefiles",
                             "01_MPA_shp.RDS"))
  
# ------------------------------------------------------------
# # Unify the crs between the MPA shapefile and the set shapefile
  
  MPA_transform.sf <- st_transform(MPA.sf, desired_proj)
  
  set_transform.sf <- st_transform(set.sf, crs(MPA_transform.sf))
  
  ggplot() + 
    geom_sf(data = set_transform.sf) + 
    geom_sf(data = MPA_transform.sf)
  

#---------------------------------------
# Calc distance between set and monument boarder 
  
  set.sf <- set_transform.sf %>% 
    mutate(distance = st_distance(set_transform.sf, 
                                  MPA_transform.sf, 
                                  by_element = TRUE)) #units meters
  
  # st_distance calc distance in meters.  Convert to NM 
  set.df <- st_drop_geometry(set.sf) %>% 
    mutate(DIST_NM = as.numeric(distance)/1852) %>% 
    dplyr::select(SET_ID, DIST_NM)
  
  

  
#--------------------------------------
# Merge with observer data 
  
  distance_sample.df <- species_catch_FULL.df %>%
    left_join(set.df, by = "SET_ID") %>% 
    filter(!is.na(DIST_NM)) %>% 
    mutate(DIST_NM_scaled = DIST_NM/1000) 
  
  saveRDS(distance_sample.df, file.path(".",
                                        "Data", 
                                        data_set,
                                        paste0("04_PMNM_distanceSample.RDS")))





