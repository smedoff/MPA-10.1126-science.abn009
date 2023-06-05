#=============
# Title: Processing Monument Shape File 
# Author: Sarah Medoff 
#=============

# The purpose of this script is to process the MPA shape file in the 
# following ways: 
# - filter for Papahanamokuakea polygon 
# - rotate the shapefile so it alligns with set locations in later scripts
# - create placebo space shapefiles by shifting the MPA boarder by 10 
#   degrees north, south, east, and west (space_shift_deg parameter)



library(sf)
library(tidyverse)
library(ggrepel)
library(rgeos)
library(rgdal)

rm(list = ls())

#--------------------
# Setting Parameters

  # Projection
  proj_deg <- "epsg:4326"

  # Space shift for placebo test 
  space_shift_deg <- 10
  
  # Load custom function to "rotate" the view
  source(file.path("Monument Shapefiles", "hlpr_func", "st_rotate.R"))
  source(file.path("Monument Shapefiles", "hlpr_func", "sfc_as_cols.R"))
  

#===============
# Processing PMNM Shapefile
#===============
# Processing MPA shapefile for main specifications
  
  MPA.sf <- read_sf(file.path("Monument Shapefiles", "data"),
                  layer = "WDPA_Feb2018_marine-shapefile-polygons",
                  quiet = T,
                  stringsAsFactors = F) %>% 
    janitor::clean_names() %>%
    filter(wdpaid %in% c(220201)) %>% #https://www.protectedplanet.net/
    select(wdpaid, name) %>% 
    st_rotate() %>% 
    group_by(wdpaid) %>% 
    summarize() %>% 
    ungroup() %>% 
    mutate(feature = "PMNM") %>% 
    select(feature)
   
  # Verification Check 
  # Plot the MPA shapefile 
  ggplot() + 
    geom_sf(data = MPA.sf, aes()) 
  
  saveRDS(MPA.sf, file.path(".",
                           "Monument Shapefiles",
                           "01_MPA_shp.RDS"))



#===============
# Creating Space Shift PMNM Shapefiles 
#===============
  
  # Write functions for processing
  # function to "rotate" the view around the dateline so maps are Pacific-centric
  st_rotate.f <- function(x){
    
    # project to degree-based coordinate system, so we can add/subtract appropriately
    x1 <- st_transform(x, proj_deg)
    
    # wrap around dateline
    x2 <- (sf::st_geometry(x1) + c(360,90)) %% c(360) - c(0,90) 
    x3 <- sf::st_wrap_dateline(sf::st_set_crs(x2 - c(180,0), proj_deg)) + c(180,0)
    x4 <- sf::st_set_crs(x3, proj_deg)
    x <- sf::st_set_geometry(x, x4)
    
    return(x)
  }
  
  # function to offset/shift polygon
  shift.f <- function(orig_poly, lat_shift_deg, lon_shift_deg){
    
    # project to degree-based projection so offset is measured in deg
    # doing this instead of nm because it shifts straight up/down/left/right on unprojected map
    proj_poly <- st_transform(orig_poly, proj_deg)
    
    # shift lat/lon by some # of degrees
    shift_poly <- st_geometry(proj_poly) + c(lat_shift_deg, lon_shift_deg)
    
    # set the CRS, convert to sf object, and then rotate for mapping
    # (it's already on degree-based coord system, so no need to reproject at the end)
    shift_poly <- st_set_crs(shift_poly, st_crs(proj_poly))
    shift_poly <- st_sf(shift_poly)
    shift_poly <- st_rotate.f(shift_poly)
    
    return(shift_poly)
  }
  
  #-----------
  

  # read in the MPA shapefile
  pmnm_poly <- read.csv(file.path("Monument Shapefiles", 
                                  "data",
                                  "pmnm.csv"),header=TRUE)[,2:3]
  names(pmnm_poly) <- c("lon","lat")
  
  # not projecting/rotating here because we are going to edit this shapefile later.
  pmnm <- sf::read_sf(file.path("Monument Shapefiles", 
                                "data",
                                "pmnm_py",
                                "PMNM_py_files"), layer = "PMNM_py")
  
  # convert from coordinates to polygon
  pmnm_poly <- pmnm_poly %>%
    st_as_sf(coords = c("lon", "lat"), crs = st_crs(pmnm)) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") %>%
    st_transform(crs = proj_deg) %>%
    st_rotate.f()
  
 
  # space placebo
  # inputs are the original polygon to shift and the # of degrees lat/lon shift
  shift_N.shp <- shift.f(pmnm_poly, 0, space_shift_deg)
  shift_S.shp <- shift.f(pmnm_poly, 0, -1*space_shift_deg)
  shift_E.shp <- shift.f(pmnm_poly, space_shift_deg, 0)
  shift_W.shp <- shift.f(pmnm_poly, -1*space_shift_deg, 0)
  
  # Verification check 
  # plot placebo shapefiles 
  ggplot() + 
    geom_sf(data = shift_N.shp) + 
    geom_sf(data = shift_S.shp) + 
    geom_sf(data = shift_E.shp) + 
    geom_sf(data = shift_W.shp) + 
    geom_sf(data = pmnm_poly, color = "red")

  # Saving Placebo Shapefiles
  saveRDS(shift_N.shp, file.path("Monument Shapefiles",
                                 "placebo_shp",
                                 paste0("shift_N", space_shift_deg, ".RDS")))
  
  saveRDS(shift_S.shp, file.path("Monument Shapefiles",
                                 "placebo_shp",
                                 paste0("shift_S", space_shift_deg, ".RDS")))
  
  saveRDS(shift_E.shp, file.path("Monument Shapefiles",
                                 "placebo_shp", 
                                 paste0("shift_E", space_shift_deg, ".RDS")))
  
  saveRDS(shift_W.shp, file.path("Monument Shapefiles",
                                 "placebo_shp", 
                                 paste0("shift_W", space_shift_deg, ".RDS")))
