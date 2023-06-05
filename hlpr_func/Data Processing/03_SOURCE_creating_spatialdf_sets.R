


library(dplyr)
library(sf)
library(rgdal)
library(ggplot2)


source(file.path("Monument Shapefiles", "hlpr_func", "st_rotate.R"))
source(file.path("Monument Shapefiles", "hlpr_func", "sfc_as_cols.R"))



  species_catch_FULL.df <- readRDS(file.path(".",
                                             "Data",
                                             data_set,
                                             "02_species_catch_FULL_df.RDS")) 
#---------------
  
  lat_extent <- range(species_catch_FULL.df$SET_BEGIN_LAT, na.rm = TRUE)
  
  long_extent <- range(species_catch_FULL.df$SET_BEGIN_LON, na.rm = TRUE)
  
  # Turn sets into spatial objects and reproject so shapefile and spatial points have same 
  # projection 
  
  set.df <- species_catch_FULL.df %>% 
    group_by(SET_ID) %>%
    summarize(lon = unique(SET_BEGIN_LON),
              lat = unique(SET_BEGIN_LAT)) %>%
    mutate(x = as.numeric(lon),
           y = as.numeric(lat))
  
  # Turn the begin set lat/lon into spatial points while preserving SET_ID
  set.sf <- set.df %>% 
    st_as_sf(coords = c("x", "y"), crs = 4326, agr = "constant") %>% 
    st_rotate() 
  
  saveRDS(set.sf, file.path(".",
                            "Data",
                            data_set,
                            "03_set_sf.RDS"))
  
  