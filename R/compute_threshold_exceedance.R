#' @title Compute the Period During Which Temperature Exceeds a Threshold
#'
#' @description
#' Computes the first and last dates during which temperature exceeds a specified
#' threshold for a given year, together with the duration of the exceedance period.
#'
#' @details
#' The function filters the dataset for the selected year and identifies all
#' observations where temperature is greater than or equal to a given threshold.
#'
#' It returns:
#' \itemize{
#'   \item the first date exceeding the threshold,
#'   \item the last date exceeding the threshold,
#'   \item the duration of the exceedance period (in days),
#'   \item the proportion of expected days with available observations,
#'   \item and comments describing potential quality issues.
#' }
#'
#' Several quality checks are performed:
#' \itemize{
#'   \item the exceedance period cannot begin on the first available measurement,
#'   \item the exceedance period cannot end on the last available measurement,
#'   \item sufficient data coverage must be available between the detected dates.
#' }
#'
#' If the temperature never reaches the specified threshold, the duration is
#' returned as 0 and the exceedance dates are set to `NA`.
#'
#' @author
#' Raphaelle Napoleoni \cr
#' Rosalie Bruel
#'
#' @param df A data.frame containing a date column and one or more
#'   value columns.
#' @param date_column Column name or index containing date-time values.
#' @param value_column Column name or index containing the value to evaluate.
#' @param year Integer. Year for which the threshold exceedance is computed.
#' @param start_doy Integer. First Julian day for which data are expected.
#'   Used only for the data availability quality check.
#' @param end_doy Integer. Last Julian day for which data are expected.
#'   Used only for the data availability quality check.
#' @param value_threshold Numeric. Value threshold to evaluate.
#' @param min_data_availability Numeric between 0 and 1. Minimum proportion of
#'   expected days with observations required to compute the exceedance duration.
#'
#' @return A tibble containing:
#' \describe{
#'   \item{value_threshold}{Threshold value used}
#'   \item{start_date}{First date exceeding the threshold}
#'   \item{end_date}{Last date exceeding the threshold}
#'   \item{duration_days}{Duration of the exceedance period in days}
#'   \item{data_availability}{Proportion of expected days with available data}
#'   \item{comment}{Information about data quality or computation}
#' }
#'
#' @examples
#' \dontrun{
#' compute_threshold_exceedance()(
#'   df,
#'   date_column = "date",
#'   value_column = "temperature",
#'   year = 2020,
#'   start_doy = 120,
#'   end_doy = 300,
#'   value_threshold = 20,
#'   min_data_availability = 0.9
#' )
#' }
#'
#' @importFrom dplyr filter
#' @importFrom lubridate year yday
#'
#' @export

compute_threshold_exceedance <- function(df,
                                         date_column,
                                         value_column,
                                         year,
                                         start_doy,
                                         end_doy,
                                         value_threshold,
                                         min_data_availability) {

  # Preparation  ####

  df[[value_column]] <- as.numeric(df[[value_column]])

  df <- df |>
    dplyr::filter(lubridate::year(.data[[date_column]]) == year) |>
    dplyr::select(
      date = all_of(date_column),
      temperature = all_of(value_column)
    )

  if (nrow(df) == 0) {
    return(
      tibble::tibble(
        value_threshold = value_threshold,
        start_date = as.Date(NA),
        end_date = as.Date(NA),
        duration_days = NA_real_,
        data_availability = NA_real_,
        comment = "missing_data"
      )
    )
  }

  # Threshold exceedance  ####

  exceedance <- dplyr::filter(df, temperature >= value_threshold)

  if (nrow(exceedance) == 0) {

    available <- df |>
      dplyr::filter(!is.na(temperature)) |>
      dplyr::filter(
        as.Date(date) >= as.Date(start_doy, origin = paste0(year, "-01-01")),
        as.Date(date) <= as.Date(end_doy, origin = paste0(year, "-01-01"))
      )

    data_availability <-
      length(unique(lubridate::yday(available$date))) /
      (end_doy - start_doy + 1)

    return(
      tibble::tibble(
        value_threshold = value_threshold,
        start_date = as.Date(NA),
        end_date = as.Date(NA),
        duration_days = 0,
        data_availability = round(data_availability, 2),
        comment = paste0(
          "In ", year,
          ", temperature never reached the selected threshold."
        )
      )
    )
  }

  # Compute exceedance period  ####

  start_date <- as.Date(exceedance$date[1])
  end_date <- as.Date(exceedance$date[nrow(exceedance)])

  duration_days <- as.numeric(end_date - start_date) + 1

  # Quality checks  ####

  if (start_date == min(as.Date(df$date))) {

    return(
      tibble::tibble(
        value_threshold = value_threshold,
        start_date = as.Date(NA),
        end_date = as.Date(NA),
        duration_days = NA_real_,
        data_availability = NA_real_,
        comment = "threshold exceedance starts on the first available observation"
      )
    )
  }

  if (end_date == max(as.Date(df$date))) {

    return(
      tibble::tibble(
        value_threshold = value_threshold,
        start_date = as.Date(NA),
        end_date = as.Date(NA),
        duration_days = NA_real_,
        data_availability = NA_real_,
        comment = "threshold exceedance ends on the last available observation"
      )
    )
  }

  available <- df |>
    dplyr::filter(!is.na(temperature)) |>
    dplyr::filter(
      as.Date(date) >= max(
        c(
          as.Date(start_doy, origin = paste0(year, "-01-01")),
          start_date
        )
      ),
      as.Date(date) <= min(
        c(
          as.Date(end_doy, origin = paste0(year, "-01-01")),
          end_date
        )
      )
    )

  n_days_with_data <- length(unique(lubridate::yday(available$date)))
  expected_days <- lubridate::yday(end_date) - lubridate::yday(start_date) + 1

  data_availability <- n_days_with_data / expected_days

  if (is.na(data_availability))
    data_availability <- 0

  if (data_availability < min_data_availability) {

    duration_days <- NA

    comment <-
      "insufficient data available to estimate exceedance duration"

  } else {

    comment <- "ok"

  }

  # Output ####

  tibble::tibble(
    value_threshold = value_threshold,
    start_date = start_date,
    end_date = end_date,
    duration_days = duration_days,
    data_availability = round(data_availability, 2),
    comment = comment
  )
}

