#' @title Plot Lake Temperature Indicators
#'
#' @description
#' Creates a ggplot showing lake temperature dynamics for a reference dataset
#' and an anomalous year, with optional indicators such as recovery day,
#' summer temperature, growing degree days (GDD), and maximum temperature.
#'
#' @details
#' The function allows flexible inclusion of indicators via the `indicators`
#' argument. All indicator values (e.g., recovery dates, Tmax) must be computed
#' beforehand and passed as inputs.
#'
#' Available indicators:
#' \itemize{
#'   \item "recovery_day": mixing/recovery dates
#'   \item "summer_temp": summer mean temperature curves
#'   \item "gdd": growing degree days ribbons
#'   \item "tmax": maximum temperature points
#' }
#'
#' @author
#' Rosalie Bruel
#'
#' @param df_ref Reference dataset (e.g., climatology), must contain:
#'   yday, mean_value1, sd_value1
#' @param df_anom Dataset for the anomalous year, must contain:
#'   date, yday, value1
#' @param year_anom Numeric. Year of the anomalous dataset
#' @param indicators Character vector specifying which indicators to plot
#' @param recovery_ref Data.frame with recovery day for reference (must contain yday)
#' @param recovery_anom Data.frame with recovery day for anomalous year (must contain yday)
#' @param tmax_ref Numeric or data.frame containing Tmax info (optional)
#' @param tmax_anom Numeric or data.frame containing Tmax info (optional)
#' @param yday_to_show Vector of yday values for x-axis labels
#' @param lake_name Character. Name of the lake
#' @param col_ref Color for reference
#' @param col_anom Color for anomalous year
#' @param col_lake Color for main temperature line
#' @param col_gdd_base Color for base GDD
#' @param col_gdd_add Color for additional GDD
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @importFrom dplyr filter mutate left_join
#' @importFrom lubridate yday year
#'
#' @export
plot_lake_indicators <- function(
    df_ref,
    df_anom,
    year_anom,
    indicators = c("recovery_day", "summer_temp", "gdd", "tmax"),
    recovery_ref = NULL,
    recovery_anom = NULL,
    yday_to_show,
    lake_name = "",
    col_ref = "grey70",
    col_anom = "tomato",
    col_lake = "#9CCBEC",
    col_gdd_base = "#FFF5D7",
    col_gdd_add = "#FEE082"
) {

  p <- ggplot(df_ref) +

    geom_hline(yintercept = 4, linewidth = 0.2) +

    geom_ribbon(
      aes(x = yday,
          ymin = mean_value1 - sd_value1,
          ymax = mean_value1 + sd_value1),
      fill = "lightgrey", alpha = 0.5
    ) +

    geom_line(aes(yday, mean_value1, color = "Average temperature"),
              linewidth = 0.5)

  # ---- GDD ----
  if ("gdd" %in% indicators) {

    df_gdd <- df_anom %>%
      dplyr::filter(
        lubridate::year(date) == year_anom,
        date <= as.Date(paste0(year_anom, "-08-31"))
      ) %>%
      dplyr::mutate(yday = lubridate::yday(date) + 365)

    p <- p +
      geom_ribbon(
        data = df_gdd,
        aes(x = yday, ymin = 4, ymax = value1, fill = "gdd_base"),
        alpha = 0.2
      )
  }

  # ---- Summer temperature ----
  if ("summer_temp" %in% indicators) {

    p <- p +
      geom_line(
        data = df_ref %>%
          dplyr::filter(yday >= 182 + 365 & yday <= 243 + 365),
        aes(yday, mean_value1, color = "summer_ref"),
        linewidth = 2
      ) +
      geom_line(
        data = df_anom %>%
          dplyr::filter(date >= as.Date(paste0(year_anom, "-07-01")),
                        date <= as.Date(paste0(year_anom, "-08-31"))),
        aes(yday, value1, color = "summer_anom"),
        linewidth = 2
      )
  }

  # ---- Tmax ----
  if ("tmax" %in% indicators) {

    p <- p +
      geom_point(
        data = df_ref %>%
          dplyr::filter(mean_value1 == max(mean_value1, na.rm = TRUE)),
        aes(yday, mean_value1, shape = "tmax_ref"),
        fill = col_ref, size = 3
      ) +
      geom_point(
        data = df_anom %>%
          dplyr::filter(value1 == max(value1, na.rm = TRUE)),
        aes(yday, value1, shape = "tmax_anom"),
        fill = col_anom, size = 3
      )
  }

  # ---- Recovery day ----
  if ("recovery_day" %in% indicators &&
      !is.null(recovery_ref) && !is.null(recovery_anom)) {

    p <- p +
      geom_point(
        data = recovery_ref,
        aes(yday, 4, shape = "recovery_ref"),
        fill = col_ref, size = 3
      ) +
      geom_point(
        data = recovery_anom,
        aes(yday, 4, shape = "recovery_anom"),
        fill = col_anom, size = 3
      )
  }

  # ---- Anomalous line ----
  p <- p +
    geom_line(
      data = df_anom,
      aes(yday, value1, color = "anom"),
      linewidth = 0.8
    )

  # ---- Scales ----
  p <- p +
    scale_x_continuous(
      breaks = yday_to_show,
      limits = c(365, 732)
    ) +
    labs(
      subtitle = lake_name,
      x = "Day of year",
      y = "Mean daily temperature (°C)"
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )

  return(p)
}
