#' @title Find the date at which a growing degree day threshold is reached
#'
#' @description
#' Computes the first date at which cumulative Growing Degree Days (GDD)
#' reach a user-defined threshold.
#'
#' @details
#' The function first computes daily and cumulative GDD using
#' \code{\link{compute_GDD}} with interpolation of missing values.
#' It then identifies the first date at which the interpolated cumulative
#' GDD exceeds the requested threshold.
#'
#' To ensure reliable estimates, the function checks that enough daily values
#' are available before the threshold date. If data availability is below the
#' requested minimum, the threshold is considered not reached.
#'
#' @author
#' Rosalie Bruel
#'
#' @param df A data.frame containing the input data.
#' @param date_column Column name or index containing dates.
#' @param value_column Column name or index containing temperature values.
#' @param gdd_threshold Numeric. Cumulative GDD threshold to detect.
#' @param start_date Start date (YYYY-MM-DD). Required.
#' @param end_date End date (YYYY-MM-DD). If NULL, December 31 of the
#' same year as `start_date` is used.
#' @param t_base Base temperature used for GDD calculation.
#' @param min_data_coverage Maximum proportion of missing observations
#' allowed within a day when computing daily mean temperature.
#' @param min_data_availability Minimum proportion (0-1) of days with
#' valid GDD values required before the threshold date.
#' @param interpolation_rule Rule passed to [stats::approx()].
#' @param output_type Either `"summary"` (default) or `"timeseries"`.
#'
#' @return
#' If `output_type = "summary"`, a tibble with:
#' \describe{
#'   \item{date_threshold_reached}{First date reaching the GDD threshold}
#'   \item{threshold_reached}{Logical}
#'   \item{gdd_threshold}{Threshold used}
#'   \item{gdd_at_threshold}{Interpolated cumulative GDD}
#'   \item{gdd}{Observed cumulative GDD}
#'   \item{gdd_interpolated}{Interpolated cumulative GDD}
#'   \item{data_availability}{Proportion of available days}
#'   \item{comment}{Execution information}
#' }
#'
#' If `output_type = "timeseries"`, the daily GDD time series produced by
#' \code{\link{compute_GDD}} is returned.
#'
#' @examples
#' \dontrun{
#' get_day_GDD_threshold(
#'   df,
#'   date_column = "date",
#'   value_column = "temperature",
#'   gdd_threshold = 400,
#'   start_date = "2022-01-01"
#' )
#' }
#'
#' @importFrom dplyr filter slice mutate
#' @importFrom tibble tibble
#'
#' @export
get_day_GDD_threshold <- function(df,
                                  date_column,
                                  value_column,
                                  gdd_threshold,
                                  start_date,
                                  end_date = NULL,
                                  t_base = 0,
                                  min_data_coverage = 0.4,
                                  min_data_availability = 0.95,
                                  interpolation_rule = 2,
                                  output_type = "summary") {

  # ---- Checks -------------------------------------------------------------

  start_date <- as.Date(start_date)

  if (is.null(end_date)) {
    end_date <- as.Date(paste0(lubridate::year(start_date), "-12-31"))
  } else {
    end_date <- as.Date(end_date)
  }

  # ---- Compute GDD --------------------------------------------------------

  gdd_ts <- compute_GDD(
    df = df,
    date_column = date_column,
    value_column = value_column,
    start_date = start_date,
    end_date = end_date,
    t_base = t_base,
    min_data_coverage = min_data_coverage,
    interpolation_rule = interpolation_rule,
    output_type = "timeseries"
  ) |>
    mutate(cumulative_gdd = cumsum(daily_gdd),
           cumulative_gdd_corr = cumsum(daily_gdd_corr))

  # ---- Find threshold -----------------------------------------------------

  crossing <- gdd_ts |>
    dplyr::filter(cumulative_gdd_corr >= gdd_threshold) |>
    dplyr::slice(1)

  if (nrow(crossing) == 0) {

    threshold_reached <- FALSE
    date_threshold_reached <- as.Date(NA)
    gdd_at_threshold <- NA_real_

    comment <- paste0(
      "gdd_threshold_", gdd_threshold, "_not_reached"
    )

  } else {

    threshold_reached <- TRUE
    date_threshold_reached <- crossing$Date
    gdd_at_threshold <- crossing$cumulative_gdd_corr

    comment <- "ok"

  }

  # ---- Quality checks -----------------------------------------------------

  if (threshold_reached) {

    period <- gdd_ts |>
      dplyr::filter(Date <= date_threshold_reached)

    data_availability <-
      mean(!is.na(period$daily_gdd))

    if (data_availability < min_data_availability) {

      threshold_reached <- FALSE
      date_threshold_reached <- as.Date(NA)

      comment <- "insufficient_data_availability"

    }

  } else {

    data_availability <- mean(!is.na(gdd_ts$daily_gdd))

  }

  # ---- Output -------------------------------------------------------------

  if (output_type == "timeseries") {
    return(gdd_ts)
  }

  tibble::tibble(
    date_threshold_reached = date_threshold_reached,
    threshold_reached = threshold_reached,
    gdd_threshold = gdd_threshold,
    gdd_at_threshold = gdd_at_threshold,
    gdd = if (threshold_reached)
      sum(gdd_ts$daily_gdd[gdd_ts$Date <= date_threshold_reached],
          na.rm = TRUE)
    else
      NA_real_,
    gdd_interpolated = if (threshold_reached)
      gdd_at_threshold
    else
      NA_real_,
    start_date = start_date,
    end_date = end_date,
    t_base = t_base,
    min_data_coverage = min_data_coverage,
    min_data_availability = min_data_availability,
    data_availability = round(data_availability, 3),
    comment = comment
  )
}
