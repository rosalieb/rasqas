#' @title Correct starting datetimes in MiniDot files
#'
#' @description
#' MiniDot compiled files often contain errors in their recorded datetimes.
#' This function provides two ways to correct them:
#'
#' 1. **Using `firstdatetime`:** specify the true datetime of the first observation.
#'    The function will then incrementally correct all subsequent rows based on the
#'    expected sampling interval.
#'
#' 2. **Using `lagdatetime`:** supply a pair of datetimes, where the first is the
#'    incorrect value recorded by the sensor, and the second is the correct value
#'    (e.g., as determined by the computer during download). The function applies
#'    this offset to all records in the file.
#'
#' The function preserves the original file structure and header rows, and writes
#' out a corrected version with a modified file name.
#'
#' @author Rosalie Bruel, OFB, Pôle R&D ECLA
#'
#' @param file Path to the MiniDot file to correct.
#' @param firstdatetime The true datetime of the first observation (used when
#'   correcting sequentially).
#' @param lagdatetime A character vector of length two: the sensor-recorded datetime
#'   and the corresponding correct datetime.
#' @param nskip Number of header rows to skip before the data section. Default is 9,
#'   consistent with the MiniDot file template.
#' @param append String to append to the output file name to avoid overwriting the
#'   original. Default is "_datetimecorrected".
#' @param overwrite Logical. If `FALSE` (default), the function will stop if the
#'   output file already exists. If `TRUE`, the existing file will be overwritten.
#' @examples
#' correctStartDatetimeMinidot(file = "/Users/robruel/Desktop/Travail/ScriptR/RNT/Input/Data/SCL06/sonde/20250724/SCL06_00500_O2.TXT",
#'     lagdatetime = c("2024-04-25 10:06:47", "2025-07-24 12:05:47"), overwrite = TRUE)
#' @examples
#' correctStartDatetimeMinidot(file = "/Users/robruel/Desktop/Travail/ScriptR/RNT/Input/Data/SCL06/sonde/20250724/SCL06_00070_O2.TXT",
#' lagdatetime = c("2023-06-30 22:18:17", "2025-07-24 11:55:17"),
#' overwrite = TRUE)
#' @export
detectDrift <- function(file, firstdatetime = NULL, lagdatetime = NULL, nskip = 9, append = "_datetimecorrected", overwrite = FALSE) {
  ...
}

correctStartDatetimeMinidot <- function(file, firstdatetime = NULL, lagdatetime = NULL, nskip = 9, append = "_datetimecorrected", overwrite = FALSE) {
  library(tools)
  library(dplyr)

  # 1. Verification #####
  if(is.null(firstdatetime) & is.null(lagdatetime)) stop("You need to fill in one of the two arguments: firstdattime or lagdatetime.")
  if(!is.null(firstdatetime) & !is.null(lagdatetime)) stop("You need to fill in only one of the two arguments: firstdattime or lagdatetime.")

  # 2. Extract component of filename ####
  path2file <- dirname(file)  # "/Users/.../20250724"
  filename  <- tools::file_path_sans_ext(basename(file))  # "SCL06_00500_O2"
  extension <- paste0(".", tools::file_ext(file))  # ".TXT"

  # Custom append string
  if(nchar(append) == 0) {
    message("The modified file will be rewritten with the same name - we do not recommand this and force an append. Modify the script itself if you do not like this feature.")
    append = "_datetimecorrected"
  }

  # Rebuild output file path
  output_file_name <- paste0(path2file, "/", filename, append, extension)

  # 3. Read file ####
  all_lines <- readLines(file)
  header_lines <- all_lines[1:nskip]       # skipped lines you want to keep
  data_lines   <- all_lines[(nskip+1):length(all_lines)]

  dt <- read.table(text = data_lines, stringsAsFactors = FALSE)

  # Build datetime columns
  dt <- dt %>%
    mutate(
      datetime_utc   = as.POSIXct(paste(V2, gsub(",", "", V3)), tz = "UTC"),
      datetime_local = as.POSIXct(paste(V4, gsub(",", "", V5)), tz = "UTC")
    )


  # 4. Correct file ####
  if (!is.null(firstdatetime)) {
    message("Correcting using firstdatetime logic...")

    n <- nrow(dt)
    step <- median(diff(as.numeric(dt$datetime_utc)), na.rm = TRUE)

    corrected <- seq(from = as.POSIXct(firstdatetime, tz = "UTC"),
                     by = step,
                     length.out = n)

    dt$datetime_utc   <- corrected
    dt$datetime_local <- corrected
  }

  if (!is.null(lagdatetime)) {
    message("Correcting using lagdatetime logic...")

    if (length(lagdatetime) != 2) stop("lagdatetime must be a vector of length 2: c(sensor_datetime, correct_datetime)")

    sensor_datetime  <- as.POSIXct(lagdatetime[1], tz = "UTC")
    correct_datetime <- as.POSIXct(lagdatetime[2], tz = "UTC")

    offset <- as.numeric(difftime(correct_datetime, sensor_datetime, units = "secs"))

    dt$datetime_utc   <- dt$datetime_utc + offset
    dt$datetime_local <- dt$datetime_local + offset
  }

  # 5. Restore structure ####
  dt <- dt %>%
    mutate(
      V2 = format(datetime_utc, "%Y-%m-%d"),
      V3 = paste0(format(datetime_utc, "%H:%M:%S"), ","),
      V4 = format(datetime_local, "%Y-%m-%d"),
      V5 = paste0(format(datetime_local, "%H:%M:%S"), ",")
    )

  dt$datetime_utc <- NULL
  dt$datetime_local <- NULL

  # Convert corrected data back into lines
  data_lines_corrected <- apply(dt, 1, paste, collapse = " ")

  # Merge header + corrected data
  all_lines_out <- c(header_lines, data_lines_corrected)

  # 6. Write out file ####
  if (file.exists(output_file_name) & !overwrite) {
    stop("Output file already exists. Use overwrite = TRUE if you want to replace it.")
  }

  writeLines(all_lines_out, con = output_file_name)

  message("File written: ", output_file_name)
  return(invisible(output_file_name))


  }

