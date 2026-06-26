#' @title Return the first date with an inversion between 2 value (when Val1>Val2).
#' 
#' @description
#' Return the first date with an inversion between 2 value (when Val1>Val2). The function allows to decide whether we want it to last for several a period.
#' 
#' @author Rosalie Bruel
#' 
#' @param dt dataset
#' @param DateTime.col name of the column with the date time information or integer of column number. Also takes in date. Format must be "%Y-%m-%d %H:%M:%S" or "%Y-%m-%d"
#' @param Value1.col name of the column with the 1st value data (or integer of column number).
#' @param Value2.col name of the column with the 2nd value data (or integer of column number).
#' @param which.val.to.substract Default = 1, if you want to do Value1.col-Value2.col. If which.val.to.substract = 2, then the function will do Value2.col - Value1.col.
#' @param thresh Threshold: the date we will return must be below that threshold. Default = 0 to find the inversion. 
#' @param difference A value to add to 'thresh': in some cases, measurements are a bit imprecise, and it may be useful to confirm we went above that value. Typically with soil or lakes, we can specify for example 2: after 15 days, we expect the value to have gone above that additional value if we really want to capture the process. Default = 0. 
#' @param absolutevalue Default = FALSE. If TRUE, will find the first value that is above the threshold, using the absolute difference between Val1 and Val2.
#' @param duration Duration the difference must last for. Default = 1: only one observation
#' @param error.tolerated Proportion (0-1) of the duration that can be above of the selected threshold. Default = 1, ALL the values must be within the difference.

# list2env(list(dt = dt, DateTime.col = "DateTime", Value1.col = "TempSurf", Value2.col= "TempDeep",
#               thresh = 0, duration = 15, error.tolerated = error.tolerated2test,
#               which.val.to.substract = which.val.to.substract, absolutevalue = absolutevalue), envir = .GlobalEnv)
# list2env(list(dt = P_reprise, DateTime.col = "date", Value1.col = 2, Value2.col = NULL, duration = 3, thresh = 0, difference = 0), envir = .GlobalEnv)

find.date.value.above.threshold <- function(dt, DateTime.col, Value1.col, Value2.col, 
                                            which.val.to.substract = 1, thresh = 0, difference = 0, 
                                            duration, error.tolerated = 1, absolutevalue = FALSE) {
  library(tidyverse); library(lubridate)
  
  duration = duration - 1 # We will use duration to add, so need to remove 1 that is in fact just the observation.
  duration = ifelse(duration<0, 0, duration)
  
  # Preparing the data ####
  if(is.null(Value2.col)) {
    dt <- dt %>% mutate(newcol = 0)
    Value2.col <- ncol(dt)
  }
  # Which value to substract
  if(which.val.to.substract == 1) {
    Valref = Value1.col
    Val2substract = Value2.col
  } else { # The opposite
    Valref = Value2.col
    Val2substract = Value1.col
  }
  # rename the columns date and value
  dt <- dt %>% 
    rename(DateTime = DateTime.col, 
           Val1 = Valref, 
           Val2 = Val2substract) %>%
    mutate(DateTime = lubridate::parse_date_time(DateTime, orders = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")),
           Date = as.Date(DateTime),
           .before = 1) %>%
    arrange(DateTime) %>%
    mutate(DiffVal = Val1-Val2)
  
  if(absolutevalue) {
    dt <- dt %>% mutate(DiffVal = abs(DiffVal)) 
  }
  
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
    for(i in c(1:c(nrow(dt)-duration))[c(1:c(nrow(dt)-duration))>0]) {
      if(!datefound & !is.na(dt$DiffVal[i]) & dt$DiffVal[i]>thresh) {
        if(length(which(c(dt$DiffVal[c(i:c(i+duration))] > thresh) == TRUE)) >= round(error.tolerated * length(dt$DiffVal[c(i:c(i+duration))]), 0) &
           dt$DiffVal[c(i+duration)] > c(thresh + difference)) {
          datefound = TRUE
          dateresult <- dt$Date[i]
          comment = ifelse(absolutevalue, "used_absolute_value", NA)
        }
      }
    }
    
    if(!datefound) {
      dateresult = NA
      comment = ifelse(absolutevalue, "comment1.used_absolute_value__comment2.did_not_find_date_inversion", "did_not_find_date_inversion")
    }
    
    mindate = dt$Date[1]
    maxdate = dt$Date[i]
    
  } else {
    dateresult = NA
    comment = ifelse(absolutevalue, "comment1.used_absolute_value__comment2.no_data_available", "no_data_available")
    mindate = NA
    maxdate = NA
  }
  
  out <- tibble(dateresult = dateresult, 
                start.calculation.date = mindate, 
                end.calculation.date = maxdate, 
                thresh = thresh,
                difference = difference,
                duration = duration +1,
                error.tolerated = error.tolerated,
                comment = comment,
                datefound = datefound)
    
  return(out)

}
