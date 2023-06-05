
# This script identifies sets made within the expansion area, before the monument boarders
# were expanded.  

# The only purpose of the data set produced is to add it into the descriptive statistics table 

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



identifying_sets_inside_mpa.f <- function(set_df){
  
  source(file.path("Monument Shapefiles", "hlpr_func", "st_rotate.R"))
  source(file.path("Monument Shapefiles", "hlpr_func", "sfc_as_cols.R"))
  desired_proj <- "+proj=utm +zone=8 +south +datum=WGS84 +units=m +no_defs"
  
  # ------------------------------------------------------------
  # Read in source files
  
  # Full species panel (set_df) is pre-loaded into R environment 
  # - This data set covers 2004 - 2020 
  # - Includes deep and shallow 
  
  # Spatial set shapefile
  set.sf <- readRDS(file.path(".",
                              "Data",
                              data_set,
                              "03_set_sf.RDS"))
  
  # PMNM shapfile
  MPA.sf <- readRDS(file.path("Monument Shapefiles",
                              "01_MPA_shp.RDS"))
  
  # ------------------------------------------------------------
  # sub set the set panel for 2010-2020 and deep set 
  
  species_panel.df <- set_df %>% 
    filter(SETBEGIN_DATE >= as.Date("2010-01-01"),
           DECLARED_TRIP_TYPE == "D")
  
  
  #-------------------------------------------------------------
  # Unify the crs between shapefiles
  MPA_transform.sf <- st_transform(MPA.sf, desired_proj)
  
  set_transform.sf <- st_transform(set.sf, crs(MPA_transform.sf))
  
  # obtaining sets from small and large donut
  MPA_sets.sf <- st_intersection(set_transform.sf, MPA_transform.sf)
  
  ggplot() + 
    geom_sf(data = MPA_transform.sf,  mapping = aes(), colour = "red", fill = NA) + 
    geom_sf(data = MPA_sets.sf, mapping = aes()) 
  
  # MPA_sets.sf use sets across the entire data set 
  # this includes sets outside of the [2010, 2020] yearly range.  This is why MPA_sets.df
  # will have NA values and/or end up being a df that is <= MPA_sets.sf.  
  # Only inner_join the two data sets
  MPA_sets.df <- st_drop_geometry(MPA_sets.sf)  %>%
    mutate(INSIDE_PMNM = 1) %>% 
    dplyr::select(SET_ID, INSIDE_PMNM) %>%
    right_join(species_panel.df, by = "SET_ID") %>% 
    mutate(INSIDE_PMNM = replace_na(INSIDE_PMNM, 0))
  
  return(MPA_sets.df)
  
}


  
  
  
  
  
  
