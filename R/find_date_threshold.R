#' @title Find first date where a difference exceeds or is below a threshold
#'
#' @description
#' Identifies the first date where the difference between two variables exceeds
#' or is below a specified threshold for a given duration. This can be used, for
#' example, to detect inversion events (e.g., when surface temperature becomes
#' higher than deep temperature).
#'
#' @details
#' The function computes the confirmation_margin between two variables and detects the
#' first occurrence where this confirmation_margin:
#' \itemize{
#'   \item exceeds a given threshold,
#'   \item persists over a specified duration,
#'   \item satisfies a tolerance level for missing/non-compliant values,
#'   \item and optionally exceeds an additional confirmation margin (`confirmation_margin`).
#' }
#'
#' The function also reports simple data-quality metrics over the analysed
#' period, including:
#' \itemize{
#'   \item the expected number of days,
#'   \item the number and proportion of days with available data,
#'   \item whether the detected event occurs on the first or last day with
#'   available data.
#' }
#'
#' If `value2_column` is NULL, the function compares `value1_column` to 0.
#'
#' @author
#' Rosalie Bruel
#'
#' @param df A data.frame containing the data.
#' @param date_column Column name or index containing date-time values.
#'   Accepted formats: "%Y-%m-%d %H:%M:%S" or "%Y-%m-%d".
#' @param value1_column Column name or index for the first variable.
#' @param value2_column Column name or index for the second variable (optional).
#'   If NULL, comparison is done against 0.
#' @param which_value_to_subtract Integer (1 or 2). Defines the subtraction order:
#'   \itemize{
#'     \item 1: Value1 - Value2 (default)
#'     \item 2: Value2 - Value1
#'   }
#' @param threshold Numeric. Minimum value the confirmation_margin must exceed.
#' @param confirmation_margin Numeric. Additional margin required at the end of the duration.
#'   Useful to confirm the signal (default = 0).
#' @param duration Integer. Number of consecutive observations required (default = 1).
#' @param direction Character string specifying the detection direction.
#'   One of:
#'   \itemize{
#'     \item `"above"` (default): detects the first date where the value exceeds `threshold`.
#'     \item `"below"`: detects the first date where the value is less than or equal to `threshold`.
#'   }
#'   When `confirmation_margin > 0`, the last observation of the detection window
#'   must exceed `threshold + confirmation_margin` (`"above"`) or be below
#'   `threshold - confirmation_margin` (`"below"`).
#' @param error_tolerated Numeric between 0 and 1. Proportion of values within the
#'   duration that must satisfy the threshold condition (default = 1).
#' @param absolute_value Logical. If TRUE, uses absolute confirmation_margin.
#' @param min_data_availability Numeric between 0 and 1. Minimum proportion of
#'   expected days with observations required to compute the exceedance duration.
#'
#' @return A data frame with:
#' \describe{
#'   \item{date_result}{First date where condition is met}
#'   \item{start_date}{First date in dataset}
#'   \item{end_date}{Last evaluated date}
#'   \item{threshold}{Threshold used}
#'   \item{confirmation_margin}{Additional margin used}
#'   \item{duration}{Duration used}
#'   \item{error_tolerated}{Tolerance level}
#'   \item{comment}{Information about execution}
#'   \item{date_found}{Logical indicating success}
#' }
#'
#' @examples
#' \dontrun{
#' find_date_threshold(df, "date", "temp_surface", "temp_bottom",
#'                     threshold = 0, duration = 5)
#' }
#'
#' @importFrom dplyr arrange mutate
#' @importFrom lubridate parse_date_time
#' @importFrom tibble tibble
#'
#' @export
find_date_threshold <- function(df,
                                date_column,
                                value1_column,
                                value2_column = NULL,
                                which_value_to_subtract = 1,
                                threshold = 0,
                                confirmation_margin = 0,
                                duration = 1,
                                direction = "above",
                                error_tolerated = 1,
                                absolute_value = FALSE,
                                min_data_availability = 0.9) {

  # ---- Checks ----
  if (duration < 1) duration <- 1
  duration_idx <- duration - 1

  direction <- match.arg(direction, c("above", "below"))

  # ---- Handle Value2 NULL ----
  if (is.null(value2_column)) {
    df$temp_zero <- 0
    value2_column <- "temp_zero"
  }

  # ---- Select subtraction order ----
  if (which_value_to_subtract == 1) {
    val_ref <- value1_column
    val_sub <- value2_column
  } else {
    val_ref <- value2_column
    val_sub <- value1_column
  }

  # ---- Prepare data ----
  df$DateTime <- lubridate::parse_date_time(
    df[[date_column]],
    orders = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")
  )

  df$Date <- as.Date(df$DateTime)
  df$Val1 <- df[[val_ref]]
  df$Val2 <- df[[val_sub]]

  df <- dplyr::arrange(df, df$DateTime)

  df$DiffVal <- df$Val1 - df$Val2

  if (absolute_value) {
    df$DiffVal <- abs(df$DiffVal)
  }

  # ---- Edge case: empty data ----
  if (nrow(df) == 0) {
    return(tibble::tibble(
      date_result = NA,
      start_date = NA,
      end_date = NA,
      threshold = threshold,
      confirmation_margin = confirmation_margin,
      duration = duration,
      error_tolerated = error_tolerated,
      comment = "no_data_available",
      date_found = FALSE
    ))
  }

  # ---- Core logic ----

  date_found <- FALSE
  date_result <- NA

  condition <- switch(
    direction,
    above = df$DiffVal > threshold,
    below = df$DiffVal <= threshold
  )

  for (i in seq_len(nrow(df) - duration_idx)) {

    if (!is.na(condition[i]) && condition[i]) {

      window_vals <- df$DiffVal[i:(i + duration_idx)]
      window_condition <- condition[i:(i + duration_idx)]

      valid_count <- sum(window_condition, na.rm = TRUE)

      last_condition <- switch(
        direction,
        above = window_vals[length(window_vals)] >
          (threshold + confirmation_margin),
        below = window_vals[length(window_vals)] <=
          (threshold - confirmation_margin)
      )

      if (valid_count >= round(error_tolerated * length(window_condition)) &&
          last_condition) {

        date_result <- df$Date[i]
        date_found <- TRUE
        break
      }
    }
  }

  # ---- Comments ----
  comment <- if (!date_found) {
    if (absolute_value) {
      "used_absolute_value__no_event_detected"
    } else {
      "no_event_detected"
    }
  } else {
    if (absolute_value) "used_absolute_value" else "ok"
  }

  # ---- Data quality metrics -----------------------------------------------

  valid_dates <- sort(unique(df$Date[!is.na(df$DiffVal)]))

  n_expected_days <- as.integer(df$Date[nrow(df)] - df$Date[1]) + 1

  if (length(valid_dates) > 0) {

    n_missing_days <- n_expected_days - length(valid_dates)

    data_availability <- 1 - n_missing_days / n_expected_days

    first_or_last_day_with_data <-
      if (!date_found) {
        NA_character_
      } else if (date_result == min(valid_dates)) {
        "result_is_first_day_with_data"
      } else if (date_result == max(valid_dates)) {
        "result_is_last_day_with_data"
      } else {
        NA_character_
      }

  } else {

    n_missing_days <- n_expected_days
    data_availability <- 0

    first_or_last_day_with_data <-
      "at_least_one_variable_has_no_data"

  }

  # ---- Output --------------------------------------------------------------

  tibble::tibble(
    date_result = date_result,
    start_date = df$Date[1],
    end_date = df$Date[nrow(df)],
    threshold = threshold,
    confirmation_margin = confirmation_margin,
    direction = direction,
    duration = duration,
    error_tolerated = error_tolerated,
    n_expected_days = n_expected_days,
    n_missing_days = n_missing_days,
    data_availability = round(data_availability, 3),
    first_or_last_day_with_data = first_or_last_day_with_data,
    comment = comment,
    date_found = date_found
  )
}
