#' @title Return the first date with a value below a certain threshold.
#' 
#' @description
#' Return the first date with a value below a threshold. The function allows to decide whether we want that period to last for several iterations.
#' 
#' @author Rosalie Bruel
#' 
#' @param dt dataset
#' @param DateTime.col name of the column with the date time information or integer of column number. Also takes in date. Format must be "%Y-%m-%d %H:%M:%S" or "%Y-%m-%d"
#' @param Value.col name of the column with the value data (or integer of column number).
#' @param thresh Threshold: the date we will return must be below that threshold. The values are turned as absolute, so if thresh = 0.3, the date will have a corresponding value between -0.3 and 0.3.
#' @param duration Duration the difference must last for. Default = 1: only one observation
#' @param error.tolerated Proportion (0-1) of the duration that can be above of the selected threshold. Default = 1, ALL the values must be within the difference.

# list2env(list(dt = dt, DateTime.col = "DateTime", Value.col = "DiffTemp", 
#               thresh = thresh2test, duration = duration2test, error.tolerated = error.tolerated2test), 
#          envir = .GlobalEnv)

find.date.value.below.thresh <- function(dt, DateTime.col, Value.col, thresh, duration, error.tolerated = 1) {
  library(tidyverse); library(lubridate)
  
  duration = duration - 1 # We will use duration to add, so need to remove 1 that is in fact just the observation.
  duration = ifelse(duration<0, 0, duration)
  
  # Preparing the data ####
  # rename the columns date and value
  dt <- dt %>% 
    rename(DateTime = DateTime.col, 
           Val = Value.col) %>%
    mutate(DateTime = lubridate::parse_date_time(DateTime, orders = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")),
           Date = as.Date(DateTime),
           .before = 1) %>%
    arrange(DateTime)
  
  stopthefunction = 0
  for(i in c("DateTime", "Val")) {
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
    for(i in c(1:c(nrow(dt)-duration))[c(1:c(nrow(dt)-duration))>0]) {
      if(!datefound & !is.na(dt$Val[i]) & abs(dt$Val[i])<=thresh) {
        if(length(which(c(abs(dt$Val[c(i:c(i+duration))]) <= thresh) == TRUE)) >= round(error.tolerated * length(abs(dt$Val[c(i:c(i+duration))])), 0)) {
          datefound = TRUE
          dateresult <- dt$Date[i]
          comment = NA
        }
      }
    }
    
    if(!datefound) {
      dateresult = NA
      comment = "did_not_find_date_meeting_thresh"
    }
    
    mindate = dt$Date[1]
    maxdate = dt$Date[i]
    
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
                duration = duration +1,
                error.tolerated = error.tolerated,
                comment = comment,
                datefound = datefound)
    
  return(out)

}
