#' @title Find lake spring mixing using temperature measured in the epi- and hypolimnion
#' 
#' @description
#' Take in a dataset at hourly time-interval with shallow and deep depth and find the likely mixing date. This function was developped for mountain lakes and cannot necessarily be reapplied directly to other lakes.
#' 
#' @author Rosalie Bruel
#' 
#' @param dt dataset
#' @param DateTime.col name of the column with the date time information or integer of column number.
#' @param Temp.Surf.col name of the column with the surface-water temperature data (or integer of column number).
#' @param Temp.Deep.col name of the column with the deep-water temperature data (or integer of column number).
#' @param YearBrass Year being looked at.
#' @param start.date Starting date (julian date, 1-366) to crop data.
#' @param end.date End date (julian date, 1-366) to crop data.
#' @param grouping_var Additional grouping variables, such as SiteID, depth_m, sensor, etc.  Must be columns names that exist in dt.
#' @param optioncalcul Options are c("belowthresh", "inversion", "minsd"). Different ways of calculating the date of mixing: with a value below a certain threshold abs(Val1-Val2)<thresh with thresh = 0.5 for instance, using the moment of inversion (Val1-Val2)>thresh with thresh = 0 typically, or the minimum sd between the two values, assuming that low sd = the moment when the two values are the closest = the moment of mixing. Options are c("belowthresh", "inversion", "minsd").
#' @param prop.data.available A proportion (0-1) of temperature data that needs to be available to carry on and trust the result
#' @param thresh Threshold: the date we will return must be below that threshold.
#' @param duration Duration the difference must last for. Default = 1: only one observation
#' @param error.tolerated Proportion (0-1) of the duration that can be above of the selected threshold. Default = 1, ALL the values must be within the difference.
#' @param which.val.to.substract Argument for the function find.date.value.above.threshold that gives the direction of the difference. Default = 1, if you want to do Temp.Surf.col-Temp.Deep.col If which.val.to.substract = 2, then the function will do Temp.Deep.col - Temp.Surf.col
#' @param absolutevalue Default = FALSE. If TRUE, will find the first value that is above the threshold, using the absolute difference between Val1 and Val2.

# list2env(list(dt = temp_surface_fond, DateTime.col = "date", 
#               Temp.Surf.col = "temperature_surface", Temp.Deep.col = "temperature_fond", 
#               YearBrass = annee, start.date = startperiodBrassSpring, end.date = endperiodBrassSpring, 
#               optioncalcul = "belowthresh", thresh = .6, duration = 10, 
#               error.tolerated = Seuil_data_available, grouping_var = NULL),
#          envir = .GlobalEnv)

find.date.first.mixing <- function(dt, DateTime.col, Temp.Surf.col, Temp.Deep.col, 
                                   YearBrass, start.date, end.date, grouping_var = NULL, 
                                   optioncalcul, duration2test = NULL, prop.data.available, thresh, duration, 
                                   error.tolerated, which.val.to.substract = 1, absolutevalue = FALSE) {
  library(tidyverse); library(lubridate)
  
  if(is.null(optioncalcul)) {
    message("You need to select one of the option to compute the spring mixing date. Options are c('belowthresh', 'inversion', 'minsd').")
  }
  
  # Preparing the data ####
  # rename the columns date and value
  dt <- dt %>% 
    rename(DateTime = DateTime.col, 
           TempSurf = Temp.Surf.col,
           TempDeep = Temp.Deep.col) %>%
    mutate(DateTime = lubridate::parse_date_time(DateTime, orders = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")),
           Date = as.Date(DateTime),
           Year = year(Date),
           doy = yday(Date),
           .before = 1)
  
  stopthefunction = 0
  for(i in c("DateTime", "TempSurf", "TempDeep", grouping_var)) {
    if(!i %in% names(dt)) {
      message(paste0("The variable ", i, " is not in the table but is needed to run the function."))
      stopthefunction = stopthefunction + 1
    }
  }
  if(stopthefunction>0) stop("Add the variable to dt before running the function.")

  dt <- dt %>%
    filter(Year == YearBrass) %>%
    filter(doy >= start.date & doy <= end.date) %>%
    mutate(DiffTemp = TempSurf - TempDeep) %>%
    arrange(DateTime)
    
  
  continue.fun = TRUE
  # Case where one of the two depth is missing for all.
  #if(nrow(dt %>% filter(!is.na(DiffTemp))) == 0) continue.fun = FALSE
  
  out <- NULL
  if(continue.fun) {
    #### OPTION 1 : Find date with value below a certain threshold  ####
    if("belowthresh" %in% optioncalcul) {
      for(thresh2test in thresh) {
        for(duration2test in duration) {
          for(error.tolerated2test in error.tolerated) {
            out <- bind_rows(out,
                             find.date.value.below.thresh(dt = dt, DateTime.col = "DateTime", Value.col = "DiffTemp", 
                                                          thresh = thresh2test, duration = duration2test, error.tolerated = error.tolerated2test) %>%
                               mutate(methodMixRegime = "belowthresh"))
          }}}}
    
    #### OPTION 2 : Find date when TempSurf>TempDeep   ####
    if("inversion" %in% optioncalcul) {
      for(thresh2test in thresh) {
        for(duration2test in duration) {
          for(error.tolerated2test in error.tolerated) {
            out <- bind_rows(out,
                             find.date.value.above.threshold(dt = dt, DateTime.col = "DateTime", Value1.col = "TempSurf", Value2.col= "TempDeep",
                                                             thresh = 0, duration = duration2test, error.tolerated = error.tolerated2test, 
                                                             which.val.to.substract = which.val.to.substract, absolutevalue = absolutevalue) %>%
                               mutate(methodMixRegime = "inversion"))
          }}}}
    
    #### OPTION 3 : Find date with min sd between TempSurf and TempDeep ####
    if("minsd" %in% optioncalcul) {
      out <- bind_rows(out,
                       find.date.min.sd.between.2values(dt = dt, DateTime.col = "DateTime", Value1.col = "TempSurf", Value2.col= "TempDeep",
                                                        thresh = thresh) %>%
                         mutate(methodMixRegime = "minsd"))
      } 
  }
  
  # Add some stats about data availability
  out <- out %>% mutate(
    nExpectedDays = end.date-start.date + 1,
    nMissingDays = length(which(c(!c(start.date:end.date) %in% c(dt %>% filter(!is.na(DiffTemp)) %>% pull(doy) %>% unique)) == TRUE)),
    percDayAvailable = 1-nMissingDays/nExpectedDays
  )


  
  # Check that the date is neither the first nor the last date available
  if(nrow(dt %>%filter(!is.na(DiffTemp))) >0) {
    out <- out %>% mutate(
      first_or_last_day_with_data = ifelse(dateresult == c(dt %>%filter(!is.na(DiffTemp)) %>% pull(Date) %>% min()), "Result_is_first_date_with_data.", 
                                           ifelse(dateresult == c(dt %>%filter(!is.na(DiffTemp)) %>% pull(Date) %>% max()), "Result_is_last_date_with_data.", NA))
    )
  } else {
    out <- out %>% mutate(
      first_or_last_day_with_data = "At_least_one_depth_has_no_data_at_all._Cannot_proceed."
    )
}
    
  
  # Note that the argument prop.data.available does nothing for now
  
return(out)
}
