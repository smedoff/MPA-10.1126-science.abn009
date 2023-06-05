

# This script will intersect the 600NM MPA buffer with the set locations.  
# The final data set will only consist sets made within 600NM of the expansion
# border. 

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
# Cleaning R environment
  source(file.path("hlpr_func", "source_cleaning_Renviro.R"))
  rm(list=setdiff(ls(), enviro_vars.v))


# ------------------------------------------------------------

source(file.path("Monument Shapefiles", "hlpr_func", "st_rotate.R"))
source(file.path("Monument Shapefiles", "hlpr_func", "sfc_as_cols.R"))


desired_proj <- "+proj=utm +zone=8 +south +datum=WGS84 +units=m +no_defs"

# ------------------------------------------------------------
# Read in source files

  # Full species panel.  All sets (at all distances) from 2000-2020
  full_set_panel.df <- readRDS(file.path(".",
                                         "Data",
                                         data_set,
                                         paste0("05_FullSetPanel_2000_2020.RDS")))

  # Spatial set shapefile
  set.sf <- readRDS(file.path(".",
                              "Data",
                              data_set,
                              "03_set_sf.RDS"))
  
  
  # PMNM shapfile
  if(analysis_type == "placeboSPACE"){
    MPA.sf <- readRDS(file.path("Monument Shapefiles",
                                "placebo_shp",
                                paste0("shift_", 
                                       space_shift, 
                                       space_shift_deg, ".RDS")))
  }else{
    
    MPA.sf <- readRDS(file.path("Monument Shapefiles",
                                "01_MPA_shp.RDS"))
  }
  
# ------------------------------------------------------------
# sub set the set panel for 6 years prior to expansion year and 4 years post
  
  # The first statement in the ifelse statment is scripted to handle the 
  # 08_pre_exp_sensitivity_analaysis in which we externally specify our 
  # 4 different sample start years (2006, 2007, 2008, 2009)
  if(analysis_type == "sensitivityPRE"){
    
    lower_bound_year <- pre_expansion_start_yr
    lower_bound_date <- as.Date(paste0(lower_bound_year, "-01-01"))
  }else{
    
    lower_bound_year <- as.numeric(substr(expansion_date, 1, 4)) - 6
    lower_bound_date <- as.Date(paste0(lower_bound_year, "-01-01"))
  }
  
  upper_bound_year <- as.numeric(substr(expansion_date, 1, 4)) + 3
  upper_bound_date <- as.Date(paste0(upper_bound_year, 
                                     "-12-31"))
  
  subset_set_panel.df <- full_set_panel.df %>% 
    filter(SETBEGIN_DATE >= lower_bound_date &
             SETBEGIN_DATE <= upper_bound_date,
           DECLARED_TRIP_TYPE == "D")

  
#---------------------------------------
# Creating a fishing boundary based on fishing_boundary_nm
  
  # Convert region-radii specification from NM to meters 
  rr_numeric <- as.numeric(fishing_boundary_nm)
  
  fishing_boundary_meters <- rr_numeric * 1852
  
  # Unify the crs between shapefiles
  MPA_transform.sf <- st_transform(MPA.sf, desired_proj)

  # Extending the outside boarder to create small and large boundary 
  MPA_boundary <- st_buffer(MPA_transform.sf, dist = fishing_boundary_meters) 

  
  # Remove the middle of each shape to create a small and large donut 
  donut_boundary <- MPA_boundary %>%
    st_difference(MPA_transform.sf)


  
#---------------------------------------
# Create a donut data frame 
  
    # Unify the crs between shapefiles
    set_transform.sf <- st_transform(set.sf, crs(MPA_transform.sf))
  
    # obtaining sets from small and large donut
    donut_boundary_sets.sf <- st_intersection(set_transform.sf, donut_boundary)
    
    #------------------
    # Verification check - Make sure the sets are overlapping with the 
    # regions 
    
    
    # Plot the regions
    donut.p <- ggplot() + 
      geom_sf(data = MPA_transform.sf, aes(), colour = scales::alpha("gray",0)) + 
      geom_sf(data = donut_boundary, aes(fill = "red")) 
    
    # Plot sets with region 
    donut.p +
      geom_sf(data = donut_boundary_sets.sf, aes()) +
      labs(title = paste0("Map: ", rr_specification, " (NM) Mile Region-Radius")) + 
      theme(legend.position = "none") +
      theme(plot.title = element_text(size=19))
    
    #ggsave(file.path("Results",
    #                 "Figures",
    #                 paste0("MAP_", file_name, ".png")), 
    #       device = "png")
    
    #------------------
    
    donut_boundary_sets.df <- st_drop_geometry(donut_boundary_sets.sf) 
    
    
    # donut_boundary_sets.df is going to be across the entire data set 
    # this includes sets outside of the [2010, 2019] yearly range.  This is why boundary_sample
    # will have NA values.  Only inner_join the two data sets
    boundary_sample <- donut_boundary_sets.df %>%
      dplyr::select(SET_ID) %>%
      inner_join(subset_set_panel.df, by = "SET_ID") %>% 
      mutate(SET_MONTH = substr(SETBEGIN_DATE, 6, 7), 
             SET_YR = substr(SETBEGIN_DATE, 1, 4),
             SET_MONTHYR = substr(SETBEGIN_DATE, 1, 7))
    
    #-----------
    # Verification check 
    # At this point, it seems natural to run a verification check to make sure 
    # n_distinct(boundary_sample$SET_ID) == n_distinct(subset_set_panel.df$SET_ID)
    # This will most likely be FALSE because the donut_boundary_sets.df is filtered 
    # on distance to MPA (but not date or set depth) where as the
    # subset_set_panel.df is filtered for date and set depth (not distance).  The 
    # most we can do is make sure nrow(boundary_sample) <= nrow(subset_set_panel.df)
    if(!(n_distinct(boundary_sample$SET_ID) < n_distinct(subset_set_panel.df$SET_ID))){
      print("STOP!!! number of set_ids in the final sample exceeds the number of set_ids
            in the original sample.  Check for duplicate observations")
    }
    
    
  #--------
  # Save the data 
    saveRDS(boundary_sample, file.path("Data", 
                                    data_set,
                                    "Specification Specific",
                                    paste0("06a_", file_name, ".RDS")))


    
    
    