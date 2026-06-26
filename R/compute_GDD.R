#' @title Compute Growing Degree Days (GDD)
#'
#' @description
#' Computes Growing Degree Days (GDD) over a defined period and optionally
#' returns a full daily time series with interpolated values.
#'
#' GDD is defined as the sum of daily mean temperatures above a base temperature.
#'
#' @details
#' The function:
#' \itemize{
#'   \item filters data over a selected time window,
#'   \item computes daily mean temperature,
#'   \item applies a data availability threshold per day,
#'   \item calculates daily GDD as max(T - Tbase, 0),
#'   \item optionally interpolates missing values and returns a full time series,
#'   \item aggregates results by grouping variable.
#' }
#'
#' Period definition supports:
#' \itemize{
#'   \item explicit start and end dates,
#'   \item start date + relative duration,
#'   \item end date + relative duration.
#' }
#'
#' @author
#' Rosalie Bruel
#'
#' @param df A data.frame containing the input dataset.
#' @param date_column Column name or index containing date values.
#' @param value_column Column name or index containing temperature values.
#'
#' @param start_date Start date of the computation period (YYYY-MM-DD).
#' @param end_date End date of the computation period (YYYY-MM-DD).
#' @param start_date_relative Number of days before `end_date` (if `start_date` is NULL).
#' @param end_date_relative Number of days after `start_date` (if `end_date` is NULL).
#'
#' @param t_base Base temperature used for GDD calculation.
#' @param min_data_coverage Maximum proportion of missing values allowed per day.
#'
#' @param grouping_var Optional grouping variable (e.g. SiteID, depth, sensor).
#'
#' @param interpolation_rule Rule used for extrapolation in `approx()`.
#' Default = 2 (carry nearest value outside range).
#'
#' @param output_type Output format:
#' \itemize{
#'   \item `"GDD"` (default): aggregated results
#'   \item `"timeseries"`: full daily time series
#' }
#'
#' @return A data.frame or tibble:
#' \describe{
#'   \item{group}{Grouping variable (if provided)}
#'   \item{start_date}{Start of computation period}
#'   \item{end_date}{End of computation period}
#'   \item{t_base}{Base temperature used}
#'   \item{min_data_coverage}{Threshold for missing data}
#'   \item{GDD}{Growing Degree Days (uncorrected)}
#'   \item{GDD_corr}{Interpolated GDD}
#'   \item{n_expected_days}{Expected number of days in period}
#'   \item{n_missing_days}{Estimated missing days}
#'   \item{data_availability}{Proportion of available data}
#'   \item{comment}{Execution notes}
#' }
#'
#' @importFrom dplyr filter mutate group_by summarise ungroup arrange
#' @importFrom lubridate as_date
#' @importFrom tibble tibble
#'
#' @export
compute_GDD <- function(df,
                        date_column,
                        value_column,
                        start_date = NULL,
                        end_date = NULL,
                        start_date_relative = NULL,
                        end_date_relative = NULL,
                        t_base = NULL,
                        min_data_coverage = 0.4,
                        grouping_var = NULL,
                        interpolation_rule = 2,
                        output_type = "GDD") {

  library(dplyr)
  library(lubridate)

  # ---- Date handling  ---------------------------------------------------------------------------

  if (!is.null(start_date)) start_date <- as.Date(start_date)
  if (!is.null(end_date)) end_date <- as.Date(end_date)

  if (is.null(start_date) && !is.null(end_date) && !is.null(start_date_relative)) {
    start_date <- end_date - start_date_relative
  }

  if (is.null(end_date) && !is.null(start_date) && !is.null(end_date_relative)) {
    end_date <- start_date + end_date_relative
  }

  if (is.null(start_date) || is.null(end_date)) {
    stop("You must define a valid start_date/end_date combination.")
  }

  # ---- Rename and prepare  ---------------------------------------------------------------------------

  data <- df |>
    mutate(
      Date = as.Date(.data[[date_column]]),
      Value = as.numeric(.data[[value_column]])
    )

  if (is.null(grouping_var)) {
    data$group <- "all"
    grouping_var <- "group"
  }

  # ---- Filter period  ---------------------------------------------------------------------------

  data <- data |>
    filter(Date >= start_date & Date <= end_date)

  # ---- Daily aggregation  ---------------------------------------------------------------------------

  gdd_daily <- data |>
    group_by(across(all_of(grouping_var)), Date) |>
    summarise(
      missing_ratio = sum(is.na(Value)) / n(),
      Value = ifelse(missing_ratio <= min_data_coverage,
                     mean(Value, na.rm = TRUE),
                     NA_real_),
      daily_gdd = pmax(Value - t_base, 0),
      .groups = "drop"
    )

  # ---- No data case  ---------------------------------------------------------------------------

  if (nrow(gdd_daily) == 0) {

    return(
      tibble::tibble(
        group = unique(data[[grouping_var]]),
        start_date = start_date,
        end_date = end_date,
        t_base = t_base,
        min_data_coverage = min_data_coverage,
        GDD = NA_real_,
        GDD_corr = NA_real_,
        n_expected_days = as.numeric(end_date - start_date + 1),
        n_missing_days = NA_real_,
        data_availability = NA_real_,
        comment = "no_data_available"
      )
    )
  }

  # ---- Full daily grid  ---------------------------------------------------------------------------

  groups <- unique(data[[grouping_var]])

  full_grid <- expand.grid(
    Date = seq.Date(start_date, end_date, by = "day"),
    group = groups
  )

  names(full_grid) <- c("Date", grouping_var)

  merged <- full_join(full_grid, gdd_daily)

  # ---- Interpolation  ---------------------------------------------------------------------------

  merged <- merged |>
    arrange(across(all_of(c(grouping_var, "Date"))))

  merged$daily_gdd_corr <- NA_real_

  for (g in groups) {

    idx <- merged[[grouping_var]] == g
    sub <- gdd_daily[gdd_daily[[grouping_var]] == g, ]

    if (nrow(sub) == 0 || all(is.na(sub$daily_gdd))) {

      merged$daily_gdd_corr[idx] <- NA_real_

    } else if (nrow(sub) == 1) {

      merged$daily_gdd_corr[idx] <-
        ifelse(merged$Date[idx] == sub$Date, sub$daily_gdd, NA_real_)

    } else {

      merged$daily_gdd_corr[idx] <- approx(
        x = sub$Date,
        y = sub$daily_gdd,
        xout = merged$Date[idx],
        rule = interpolation_rule,
        ties = "mean"
      )$y
    }
  }

  # ---- Final aggregation ---------------------------------------------------------------------------

  result <- merged |>
    group_by(across(all_of(grouping_var))) |>
    summarise(
      start_date = start_date,
      end_date = end_date,
      t_base = t_base,
      min_data_coverage = min_data_coverage,
      GDD = sum(daily_gdd, na.rm = TRUE),
      GDD_corr = sum(daily_gdd_corr, na.rm = TRUE),
      n_expected_days = as.numeric(end_date - start_date + 1),
      n_missing_days = sum(is.na(daily_gdd)),
      data_availability = 1 - n_missing_days / n_expected_days,
      interpolation_rule = interpolation_rule,
      comment = "",
      .groups = "drop"
    )

  # ---- Output ---------------------------------------------------------------------------

  if (output_type == "GDD") return(result)
  if (output_type == "timeseries") return(merged)
}
