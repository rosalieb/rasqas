#' Plot monthly value heatmap by depth or sensor
#'
#' This function creates a heatmap of monthly mean values across years,
#' with flexible depth handling:
#' - depth intervals (layer mode)
#' - specific sensor depths (sensor mode)
#'
#' @author
#' Rosalie Bruel
#'
#' @param df A data frame containing monthly temperature data.
#' @param station Character. Station identifier used for filtering.
#' @param station_col Column name for station (character).
#' @param date_col Column name for the date (character).
#' @param depth_col Column name for depth (character).
#' @param value_col Column name for value (character). Default plot is
#' set for temperature, so labels will reflect that. The output being
#' a ggplot object, the user can then modify the labels outside the
#' function.
#' @param nobs_col Column name for number of observations (character).
#' @param min_obs Minimum number of observations required (default = 25).
#' @param depth_breaks Numeric vector defining depth intervals
#' (e.g. c(0, 10, 30) → 0–10, 10–30, 30–max).
#' @param depth_breaks Numeric vector for layer mode
#' (e.g. c(0, 10, 30) → 0–10, 10–30, 30–max).
#' @param depth_values Numeric vector for sensor mode (optional)
#' (e.g. c(0.5, 1.5, 10.5) will visualize only sensors at these depths).
#' @param depth_tolerance Numeric tolerance for matching depths
#' @param facet_type "wrap" or "grid"
#' @param facet_rows Column name for facet rows (grid only)
#' @param facet_cols Column name for facet columns (grid only)
#' @param language Character. "fr" (default) or "en" for labels.
#'
#' @details
#' Depth intervals are automatically extended to the maximum observed depth.
#' Labels are adapted depending on the selected language.
#'
#' @return A ggplot object.
#'
#' @export
plot_monthly_value_heatmap <- function(df,
                                       station = NULL,
                                       station_col = "cd_station",
                                       date_col = "startDate",
                                       depth_col = "Profondeur_m",
                                       value_col = "Temperature_moyenne_mensuelle",
                                       nobs_col = "nombre_observation",
                                       min_obs = 25,
                                       depth_breaks = c(0, 10, 25),
                                       depth_values = NULL,
                                       depth_tolerance = 0.2,
                                       facet_type = "wrap",
                                       facet_rows = NULL,
                                       facet_cols = NULL,
                                       language = "fr") {

  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(scales)

  # Standardize names
  data <- df %>%
    rename(
      station_id = all_of(station_col),
      Date = all_of(date_col),
      depth = all_of(depth_col),
      value = all_of(value_col),
      nobs = all_of(nobs_col)
    )

  # Filter
  data <- data %>%
    dplyr::filter(nobs > min_obs,
           !is.na(depth))

  if (!is.null(station)) {
    data <- data %>% dplyr::filter(station_id == station)
  }

  if (nrow(data) == 0) stop("No data after filtering.")

  # ---- MODE SELECTION ----
  if (!is.null(depth_values)) {

    # SENSOR MODE
    data <- data %>%
      rowwise() %>%
      mutate(
        depth_match = depth_values[which.min(abs(depth - depth_values))]
      ) %>%
      ungroup() %>%
      dplyr::filter(abs(depth - depth_match) <= depth_tolerance)

    data <- data %>%
      mutate(depth_cat = paste0(depth_match, " m"))

  } else {

    # LAYER MODE
    max_depth <- max(data$depth, na.rm = TRUE)
    depth_breaks <- sort(unique(c(depth_breaks, max_depth)))

    labels <- paste0(head(depth_breaks, -1), "-", tail(depth_breaks, -1), " m")

    labels <- if (language == "fr") {
      paste0("couche ", labels)
    } else {
      paste0("layer ", labels)
    }

    data <- data %>%
      mutate(depth_cat = cut(depth,
                             breaks = depth_breaks,
                             labels = labels,
                             include.lowest = TRUE,
                             right = FALSE))
  }

  # Aggregate
  data <- data %>%
    mutate(
      year = year(Date),
      month = month(Date)
    ) %>%
    group_by(year, month, depth_cat, station_id) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

  # Labels
  month_labels <- if (language == "fr") {
    c("Jan", "Fév", "Mar", "Avr", "Mai", "Juin",
      "Juil", "Août", "Sep", "Oct", "Nov", "Déc")
  } else {
    month.abb
  }

  fill_label <- if (language == "fr") {
    "Valeur moyenne mensuelle"
  } else {
    "Monthly mean value"
  }

  subtitle_text <- if (language == "fr") {
    "Moyenne mensuelle de température par couches"
  } else {
    "Monthly mean temperature by depth layers"
  }

  # Midpoint
  midpoint_val <- median(data$value, na.rm = TRUE)
  midpoint_val <- ifelse(midpoint_val < 4, 4, midpoint_val)

  # Plot
  p <- ggplot(data, aes(month, year, fill = value)) +
    geom_tile() +
    geom_text(aes(label = round(value, 1)), size = 3) +
    scale_x_continuous(breaks = 1:12, labels = month_labels) +
    scale_y_reverse(breaks = function(x) {
      # ggplot’s default pretty breaks
      br <- pretty(x)
      # round them to integers
      br <- round(br)
      # remove duplicates (rounding can create repeated values)
      unique(br)
    }) +
    scale_fill_gradient2(
      name = fill_label,
      high = muted("red"),
      low = muted("lightblue"),
      midpoint = midpoint_val
    ) +
    labs(
      x = NULL,
      y = ifelse(language == "fr", "Année", "Year"),
      title = station,
      subtitle = subtitle_text
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(fill = guide_colourbar(title.position = "top",
                                  title.hjust = 0.5))

  # ---- FACETING ----
  if (facet_type == "wrap") {
    p <- p + facet_wrap(~depth_cat, ncol = 1)

  } else if (facet_type == "grid") {

    if (!is.null(facet_rows) & !is.null(facet_cols)) {
      p <- p + facet_grid(
        as.formula(paste(facet_rows, "~", facet_cols))
      )
    } else {
      stop("For facet_grid, provide facet_rows and facet_cols")
    }
  }

  return(p)
}
