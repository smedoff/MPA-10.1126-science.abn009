

#Converting deg/min to dec degree
# - https://www.rapidtables.com/convert/number/degrees-minutes-seconds-to-degrees.html 

converting_decdegree <- function(type, df){
  
  deg <- paste0(type, "DEG")
  min <- paste0(type, "MIN")
  dir <- paste0(type, "DIR")
  
  dec_degree.df <- df %>% 
    dplyr::select(deg_numeric = deg,
           min_numeric = min,
           dir_value = dir)
  
  dec_degree.df <- dec_degree.df %>%
    mutate(coordinate = ifelse(dir_value %in% c("S", "W"),
                               -1 * deg_numeric + min_numeric/60,
                               deg_numeric + min_numeric/60)) %>% 
    dplyr::select(coordinate)

  
  return(dec_degree.df$coordinate)
}
