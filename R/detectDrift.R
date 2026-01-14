#' Detect sensor drift
#'
#' Reads high-frequency sensor data from multiple files and detects sensor drift
#' either relative to a reference sensor or by comparing sensors against each other.
#'
#' @author Rosalie Bruel
#'
#' @param folderpath Path to the folder containing the data files (.txt or .csv).
#' @param reference_file Name of the reference sensor file to use for the drift test
#'   (including file extension, e.g. "ref_sensor.csv"). If NULL, the reference
#'   is calculated as the mean across all sensors.
#' @param test_start Start of the drift test period, format "YYYY-MM-DD HH:MM".
#'   The date-time must be provided in the same time zone as the data in the files.
#'   Note that using UTC is recommended.
#' @param test_end End of the drift test period, format "YYYY-MM-DD HH:MM".
#'   The date-time must be provided in the same time zone as the data in the files.
#'   Note that using UTC is recommended.
#' @param threshold_drift Numeric threshold above which a sensor is flagged as drifting.
#' @param patterns2searchfor Character vector of patterns to include when listing files
#'   in \code{folderpath} (e.g. station codes).
#' @param patterns2exclude Character vector of patterns to exclude when listing files
#'   in \code{folderpath}.
#' @param writeOutput Logical. If TRUE, summary tables and diagnostic plots are written
#'   to an output folder.
#' @param out_dir Optional path to the output directory where result files will be written
#'   when \code{writeOutput = TRUE}. If NULL (default), an output directory is
#'   automatically created inside \code{folderpath}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{data_list}: all sensor data read from files
#'   \item \code{drift}: a summary table with drift metrics per file
#'   \item \code{start_test}: start of the test period
#'   \item \code{end_test}: end of the test period
#' }
#'
#' @examples
#' drift <- detectDrift(
#'   folderpath = "monitoring/20250802/sensor_drift_test",
#'   reference_file = "MAL05_test.txt",
#'   test_start = "2025-08-22 14:53",
#'   test_end = "2025-08-22 14:58",
#'   threshold_drift = 0.4
#' )
#' @export
detectDrift <- function(folderpath, reference_file = NULL, test_start, test_end, threshold_drift = 0.4,
                        patterns2searchfor = NULL, patterns2exclude = c("00_drift_results","@", "pres_air|pres_eau|pres.txt|pres_cote|pression|Pres.csv", "aff.txt", "status.txt", "lire.txt", "Sorties.txt", "aff_quinson", "allos CR.txt", "00_ALIRE.txt"),
                        writeOutput = TRUE, out_dir = NULL) {
  ...
}

detectDrift <- function(folderpath, reference_file = NULL, test_start, test_end, tz = 0, threshold_drift = 0.4,
                        patterns2searchfor = NULL, patterns2exclude = c("00_drift_results","@", "pres_air|pres_eau|pres.txt|pres_cote|pression|Pres.csv", "aff.txt", "status.txt", "lire.txt", "Sorties.txt", "aff_quinson", "allos CR.txt", "00_ALIRE.txt"),
                        writeOutput = TRUE, out_dir = NULL) {
  # Get packages
  suppressMessages(suppressWarnings(require(dplyr)))
  suppressMessages(suppressWarnings(require(lubridate)))
  suppressMessages(suppressWarnings(require(ggplot2)))

  # 1. Verification ####
  # Orders pour parse_date_time()
  custom_orders = c("d/m/y H:M:S", "y/m/d H:M:S","d-m-y H:M:S", "m/d/Y H:M:S","m/d/y H:M:S","d/m/Y H:M:S",
                    "Y-m-d H:M:S", "Y/m/d H:M:S", "d/m/y", "y/m/d", "Y/m/d", "d/m/Y",
                    "d/m/y H:M", "y/m/d H:M","d-m-y H:M", "m/d/Y H:M","m/d/y H:M","d/m/Y H:M",
                    "Y-m-d H:M", "Y/m/d H:M",
                    "d/m/y I:M:S p", "y/m/d I:M:S p","d-m-y I:M:S p", "m/d/Y I:M:S p","m/d/y I:M:S p","d/m/Y I:M:S p",
                    "Y-m-d I:M:S p", "Y/m/d I:M:S p", "d/m/y", "y/m/d", "Y/m/d", "d/m/Y",
                    "d/m/y I:M p", "y/m/d I:M p","d-m-y I:M p", "m/d/Y I:M p","m/d/y I:M p","d/m/Y I:M p",
                    "Y-m-d I:M p", "Y/m/d I:M p")

  test_start = parse_date_time(test_start, orders = custom_orders)
  test_end = parse_date_time(test_end, orders = custom_orders)

  if (is.na(test_start) | is.na(test_end)) {
    stop("test_start or test_end could not be parsed. Please check the date format.")
  }

  # 2. Read files #####
  read_data <- readAssembleData(folderpath = folderpath, patterns2searchfor, patterns2exclude)

  # The data read is in data_list
  data_all <- bind_rows(read_data$data_list)

  # Keep only the focus period
  data_all <- data_all %>% filter(DateTime_UTC >= test_start,
                                  DateTime_UTC <= test_end,
                                  !is.na(TmpWtr_degC))

  # Get reference data
  if(!is.null(reference_file)) {
    data_ref = data_all %>% filter(fichier == reference_file)
    if(nrow(data_ref) == 0) {
      message("The reference sensor file was not found. Calculating the reference across sensors.")
      data_ref = data_all
    }
  } else {
    data_ref = data_all
  }

  # Get reference temperature
  ref_mean <- mean(data_ref$TmpWtr_degC)

  # Build the table with drift data
  data_drift <- data_all %>% group_by(fichier) %>% reframe(
    mean_TmpWtr_degC = mean(TmpWtr_degC),
    n_values = n(),
    min_DateTime_raw = min(DateTime_raw),
    max_DateTime_raw = max(DateTime_raw)) %>%
    mutate(ref_TmpWtr_degC = ref_mean,
           difference = mean_TmpWtr_degC-ref_TmpWtr_degC,
           drift = abs(difference) >= threshold_drift,
           threshold_drift = threshold_drift,
           .before = n_values)

  read_data$drift <- data_drift
  read_data$start_test <- test_start
  read_data$end_test <- test_end

  if(writeOutput) {
    # Create folder if it does not already exist
    if(is.null(out_dir)) out_dir <- file.path(folderpath, "00_drift_test_results")
    if (!dir.exists(out_dir)) dir.create(out_dir)

    # Table
    write.table(data_drift, file = paste0(out_dir, "/00_drift_results_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"),
                row.names = FALSE, sep = ";")

    # Plot
    p1 <- ggplot(data_drift, aes(fichier, mean_TmpWtr_degC, fill = drift)) +
      geom_segment(aes(fichier, y = ref_TmpWtr_degC-threshold_drift, yend = ref_TmpWtr_degC+threshold_drift), col = "grey") +
      geom_hline(yintercept = ref_mean) +
      #geom_point(aes(fichier, ref_TmpWtr_degC), pch = 22, fill = "black") +
      geom_point(pch = 21) +
      scale_fill_manual(values = c(`TRUE` = "tomato", `FALSE` = "grey"), labels = c(`TRUE` = "Drift/d\u233viance", `FALSE` = "No drift/pas de d\u233viance")) +
      coord_flip() +
      labs(y = "Mean value", x = "Sensor") +
      theme_bw()

    ggsave(p1, file = paste0(out_dir, "/00_drift_results_plot_drift_", format(Sys.time(), "%Y%m%d_%H%M"), ".png"),
                width = 7, height = 7)

    p2 <- ggplot(data_all, aes(DateTime_raw, TmpWtr_degC, col = fichier)) +
      geom_line() +
      labs(y = "Value", x = "DateTime") +
      theme_bw()

    ggsave(p2, file = paste0(out_dir, "/00_drift_results_plot_values_", format(Sys.time(), "%Y%m%d_%H%M"), ".png"),
           width = 7, height = 9)
  }

  return(read_data)
  }
