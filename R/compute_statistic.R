#' @title Compute parameter statistics over a given period
#'
#' @description
#' Computes temperature statistics (maximum, minimum, mean, or median)
#' over a specified Julian day period within a given year.
#' Also returns the date of occurrence for min/max values when relevant.
#'
#' @details
#' The function filters the dataset for a given year and a range of Julian days
#' (e.g., summer period), then computes the requested statistic if sufficient
#' data are available. A minimum data availability threshold is required unless
#' explicitly overridden using the exception mechanism.
#'
#' @author
#' Raphaelle Napoleoni \cr
#' Rosalie Bruel
#'
#' @param df A data.frame containing at least a date column and a temperature column.
#' @param date_column Column name or index containing date-time values
#'   (expected format: "yyyy-mm-dd hh:mm").
#' @param value_column Column name or index containing temperature values.
#' @param year Integer. Year to filter the data.
#' @param start_doy Integer. First Julian day of the analysis period.
#' @param end_doy Integer. Last Julian day of the analysis period.
#' @param statistic Character. One of "min", "max", "mean", "median".
#' @param min_data_threshold Numeric between 0 and 1. Minimum proportion of days
#'   with available data required to perform the calculation.
#' @param use_exception Logical. If TRUE, allows bypassing the data threshold check.
#' @param exception_id Vector identifying the current case (e.g., c("LakeName", year)).
#' @param exception_list List of allowed exceptions.
#'
#' @return A data.frame with one row and the following columns:
#' \describe{
#'   \item{statistic}{Type of statistic computed}
#'   \item{value}{Computed temperature value (rounded to 1 decimal)}
#'   \item{date}{Date of occurrence (only for min/max, otherwise NA)}
#'   \item{data_availability}{Proportion of days with available data}
#'   \item{comment}{Additional information (e.g., missing data warning)}
#' }
#'
#' @examples
#' \dontrun{
#' compute_statistic(data, "date", "temp", 2020, 150, 250, "max", 0.8)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom lubridate year yday
#'
#' @export
compute_statistic <- function(df,
                              date_column,
                              value_column,
                              year,
                              start_doy,
                              end_doy,
                              statistic = c("min", "max", "mean", "median"),
                              min_data_threshold,
                              use_exception = FALSE,
                              exception_id = NULL,
                              exception_list = NULL) {

  # statistic <- match.arg(statistic)
  # if(any(!statistic %in% c("min", "max", "mean", "median"))) cat("Only c")
  # statistic <- statistic[statistic %in% c("min", "max", "mean", "median")]

  # ---- Exception handling ----
  if (use_exception) {
    if (is.null(exception_id) || is.null(exception_list)) {
      stop("If use_exception = TRUE, 'exception_id' and 'exception_list' must be provided.")
    }

    if (!list(exception_id) %in% exception_list) {
      use_exception <- FALSE
    }
  }

  # ---- Data preparation ----
  df[[value_column]] <- as.numeric(df[[value_column]])

  data_year <- dplyr::filter(df, lubridate::year(df[[date_column]]) == year)
  data_period <- data_year[, c(date_column, value_column)]

  data_period <- dplyr::filter(
    data_period,
    lubridate::yday(data_period[[date_column]]) %in% start_doy:end_doy
  )

  # ---- Data availability check ----
  data_no_na <- data_period[!is.na(data_period[[value_column]]), ]

  n_days_with_data <- length(unique(lubridate::yday(data_no_na[[date_column]])))
  n_expected_days <- end_doy - start_doy + 1

  data_availability <- n_days_with_data / n_expected_days

  # ---- Initialize output table ----
  result <- NULL

  for(whichstat in statistic) {
    # ---- Perform computation ----
    if ((data_availability >= min_data_threshold) || use_exception) {

      values <- data_period[[value_column]]

      # Initialize outputs
      value <- NA
      date_value <- NA
      comment <- "ok"

      if (whichstat == "max") {
        value_raw <- max(values, na.rm = TRUE)
        idx <- which(values == value_raw)[1]
        date_value <- as.character(as.Date(data_period[[date_column]][idx]))

      } else if (whichstat == "min") {
        value_raw <- min(values, na.rm = TRUE)
        idx <- which(values == value_raw)[1]
        date_value <- as.character(as.Date(data_period[[date_column]][idx]))

      } else if (whichstat == "mean") {
        value_raw <- mean(values, na.rm = TRUE)

      } else if (whichstat == "median") {
        value_raw <- stats::median(values, na.rm = TRUE)
      } else {
        value_raw = NA
        comment <- "not a supported statistic"
      }

      #value <- round(value_raw, 1)
      value <- value_raw

    } else {
      value <- NA
      date_value <- NA
      comment <- "NA due to insufficient data"
    }

    # ---- Output ----
    result <- bind_rows(
      result,
      data.frame(
        statistic = whichstat,
        value = value,
        date = date_value,
        threshold_data_availability = min_data_threshold,
        data_availability = round(data_availability, 2),
        comment = comment,
        stringsAsFactors = FALSE
      )
    )
  }
  return(result)
}
