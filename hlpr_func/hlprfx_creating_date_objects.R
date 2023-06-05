

creating_dates.f <- function(df, type){
  # Ex Synthax
   # df = OBS_DATA
   # type = "ARRIVAL"
  
  year <- paste0(type, "_YR")
  month <- paste0(type, "_MON")
  day <- paste0(type, "_DAY")
  
  date.df <- df %>%
    dplyr::select(YEAR = year, 
           MONTH = month, 
           DAY = day) %>%
    mutate(DATE = as.Date(paste0(YEAR, "-",
                                 MONTH, "-", 
                                 DAY))) 
  
  return(date.df$DATE)
}
