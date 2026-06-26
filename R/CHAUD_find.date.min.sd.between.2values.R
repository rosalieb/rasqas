#' @title Return the date with the minimum SD between 2 values
#' 
#' @description
#' Return the date with the minimum sd 
#' 
#' @author Rosalie Bruel
#' 
#' @param dt dataset
#' @param DateTime.col name of the column with the date time information or integer of column number. Also takes in date. Format must be "%Y-%m-%d %H:%M:%S" or "%Y-%m-%d"
#' @param Value1.col name of the column with the 1st value data (or integer of column number).
#' @param Value2.col name of the column with the 2nd value data (or integer of column number).
#' @param thresh Threshold: the date we will return should have a sd between val 1 and 2 below that threshold (Useful so that no value is return if sd too high. Default = NA

find.date.min.sd.between.2values <- function(dt, DateTime.col, Value1.col, Value2.col, thresh = NULL) {
  library(tidyverse); library(lubridate)
  
  # Preparing the data ####
  # rename the columns date and value
  dt <- dt %>% 
    rename(DateTime = DateTime.col, 
           Val1 = Value1.col, 
           Val2 = Value2.col) %>%
    mutate(DateTime = lubridate::parse_date_time(DateTime, orders = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")),
           Date = as.Date(DateTime),
           .before = 1) %>%
    arrange(DateTime)
  
  stopthefunction = 0
  for(i in c("DateTime", "Val1", "Val2")) {
    if(!i %in% names(dt)) {
      message(paste0("The variable ", i, " is not in the table but is needed to run the function."))
      stopthefunction = stopthefunction + 1
    }
  }
  if(stopthefunction>0) stop("Add the variable to dt before running the function.")

  continue.fun = TRUE
  if(nrow(dt) == 0) continue.fun = FALSE
  
  datefound = FALSE
  if(continue.fun) {
    # Create sd 
    dt$Val.sd <- apply(dt %>% select(Val1, Val2), 1, sd, na.rm = TRUE)
    if(length(dt$Val.sd[!is.na(dt$Val.sd)] > 0)) {
      dateresult <- dt %>% arrange(Val.sd) %>% slice(1) %>% pull(Date) 
      Val.sd <- dt %>% arrange(Val.sd) %>% slice(1) %>% pull(Val.sd) 
      comment = NA
      
      mindate = min(dt$Date)
      maxdate = max(dt$Date)
      
      # Verifying that the sd return is not above a certain threshold
      if(is.na(thresh)) {
        if(Val.sd>thresh) {
          dateresult = NA
          comment = "sd_above_chosen_threshold"
        } else {
          datefound = TRUE
        }
      }
    } else {
      dateresult = NA
      comment = "no_data_available"
      mindate = NA
      maxdate = NA
    }
  } else {
    dateresult = NA
    comment = "no_data_available"
    mindate = NA
    maxdate = NA
  }
  
  out <- tibble(dateresult = dateresult, 
                start.calculation.date = mindate, 
                end.calculation.date = maxdate, 
                thresh = thresh,
                comment = comment,
                datefound = datefound)
    
  return(out)

}
