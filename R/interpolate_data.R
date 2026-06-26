#' Interpolate data along depth intervals and plot interpolated temperature heatmap over depth and time
#'
#' This function interpolates values (e.g., temperature) along a depth gradient
#' for each date and produces a heatmap-style plot. It is designed for datasets
#' like vertical water column measurements with irregular depth sampling.
#'
#' @author
#' Rosalie Bruel
#'
#' @param df A data frame or tibble containing at least date, depth, and value columns.
#' @param date_col Name of the date column (character).
#' @param depth_col Name of the depth column (character).
#' @param value_col Name of the variable to interpolate and plot (character).
#' @param depth_interval Numeric. Resolution of the depth grid (default = 0.1).
#' @param datetime Logical. If TRUE, treats date_col as POSIXct/datetime (default = FALSE).
#' @param average_close_depths Logical. If TRUE (default), values measured at depths
#' closer than `depth_interval` are averaged before interpolation.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Expands the dataset to a full grid of Date × Depth
#'   \item Optionally averages values measured at very similar depths (e.g. 0.50 and 0.51 m)
#'   \item Performs linear interpolation (`stats::approx`) along depth for each date
#'   \item Requires at least 2 non-missing values per date to interpolate
#' }
#'
#' When `average_close_depths = TRUE`, the function detects if multiple measurements
#' fall within the same depth interval and averages them. A message is displayed
#' when this occurs.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{The interpolated dataset}
#'   \item{plot}{A ggplot2 heatmap-style plot}
#' }
#'
#' @export
interpolate_data <- function(df,
                         date_col,
                         depth_col,
                         value_col,
                         depth_interval = 0.1,
                         datetime = FALSE,
                         average_close_depths = TRUE) {

  require(dplyr)
  require(ggplot2)

  # Standardize column names
  data <- df %>%
    dplyr::rename(
      Date = all_of(date_col),
      depth_m = all_of(depth_col),
      value = all_of(value_col)
    )

  # Safety checks
  if (nrow(data) == 0) stop("Input data has 0 rows after filtering.")
  if (all(is.na(data$depth_m))) stop("Depth column contains only NA values.")
  if (all(is.na(data$Date))) stop("Date column contains only NA values.")

  # Detect close depths and average them if average_close_depths == TRUE
  if (average_close_depths) {
    d <- sort(unique(data$depth_m))
    d_diff <- diff(d)

    if (length(d_diff) > 0 && any(d_diff < depth_interval, na.rm = TRUE)) {
      close_vals <- d[which(c(FALSE, d_diff < depth_interval))]

      message(
        "Some depths are closer than 'depth_interval'. ",
        "Averaging applied (e.g. around depth ",
        paste(head(close_vals, 3), collapse = ", "),
        ")."
      )
    }

    # Apply binning
    data <- data %>%
      dplyr::mutate(
        depth_bin = round(depth_m / depth_interval) * depth_interval
      ) %>%
      dplyr::group_by(Date, depth_bin) %>%
      dplyr::summarise(
        depth_m = mean(depth_m, na.rm = TRUE),
        value = mean(value, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Create grid
  if (!datetime) {
    fullDate <- seq(min(data$Date, na.rm = TRUE),
                    max(data$Date, na.rm = TRUE),
                    by = "day")
  } else {
    fullDate <- sort(unique(data$Date))
  }

  min_depth <- min(data$depth_m, na.rm = TRUE)
  max_depth <- max(data$depth_m, na.rm = TRUE)

  if (!is.finite(min_depth) || !is.finite(max_depth)) {
    stop("Depth range is not finite. Check your data.")
  }

  fullDepth <- seq(min_depth, max_depth, by = depth_interval)

  grid <- expand.grid(Date = fullDate, depth_m = fullDepth)

  # Align depths to grid
  data <- data %>%
    dplyr::mutate(
      depth_m = round(depth_m / depth_interval) * depth_interval
    )

  # Join grid
  data <- dplyr::full_join(data, grid, by = c("Date", "depth_m")) %>%
    dplyr::arrange(Date, depth_m)

  # Interpolation
  data <- data %>%
    dplyr::group_by(Date) %>%
    dplyr::mutate(
      value_interp = if (sum(!is.na(value)) >= 2) {
        stats::approx(
          x = depth_m[!is.na(value)],
          y = value[!is.na(value)],
          xout = depth_m,
          rule = 1
        )$y
      } else {
        NA_real_
      }
    ) %>%
    dplyr::ungroup()

  # Plot
  p <- ggplot(data, aes(x = Date, y = depth_m, fill = value_interp)) +
    geom_tile() +
    scale_y_reverse() +
    labs(x = "Date", y = "Depth (m)", fill = value_col) +
    theme_minimal()

  # Restore names
  data <- data %>%
    dplyr::rename(
      !!date_col := Date,
      !!depth_col := depth_m,
      !!value_col := value_interp
    )

  return(list(data = data, plot = p))
}
