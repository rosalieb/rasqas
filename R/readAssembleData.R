#' Read and assemble high-frequency sensor data from a folder
#'
#' Reads high-frequency data from multiple sensor files contained in one or more
#' folders, automatically detecting file formats, column separators, headers,
#' and date-time formats. The function cleans and standardizes the data, converts
#' date-times to UTC, and extracts key variables such as temperature, pressure,
#' and dissolved oxygen when available.
#'
#' Multiple sensor types and manufacturers (e.g. HOBO, MiniDOT, Tinytag) are
#' supported through flexible parsing and heuristic-based detection.
#'
#' @details
#' This function relies on heuristic rules to identify file structure, sensor
#' metadata, and date-time formats. While it is designed to handle a wide range
#' of common sensor exports, unusual file formats may require code adaptation.
#'
#' @author Rosalie Bruel
#'
#' @param folderpath Character vector of one or more paths to folders containing
#'   sensor data files (.txt or .csv).
#' @param patterns2searchfor Optional character vector of patterns to include when
#'   selecting files to read (e.g. station or site codes).
#' @param patterns2exclude Character vector of patterns used to exclude files
#'   when selecting files to read.
#' @param language Language used for progress and informational messages
#'   printed to the console. Default is French ("fr"); English ("en") is also supported.
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item \code{data_list}: a list of data frames, one per successfully read file,
#'     containing standardized sensor data
#'   \item \code{skipped_files}: names of files that were skipped during processing
#'   \item \code{read_files}: names of files that were successfully read
#'   \item \code{log_progress}: a data frame summarizing metadata for each file
#'     (sensor type, manufacturer, serial number, date range, and available variables)
#' }
#'
#' @examples
#' \dontrun{
#' data <- readAssembleData(
#'   folderpath = "monitoring/20250802/sensor_data",
#'   patterns2searchfor = "MAL05",
#'   patterns2exclude = c("status", "aff")
#' )
#' }
#' @export
readAssembleData <- function(folderpath, patterns2searchfor = NULL, patterns2exclude = c("00_drift_results","@", "pres_air|pres_eau|pres.txt|pres_cote|pression|Pres.csv", "aff.txt", "status.txt", "lire.txt", "Sorties.txt", "aff_quinson", "allos CR.txt", "00_ALIRE.txt")) {
  ...
}

readAssembleData <- function(folderpath, patterns2searchfor = NULL, patterns2exclude = c("00_drift_results","@", "pres_air|pres_eau|pres.txt|pres_cote|pression|Pres.csv", "aff.txt", "status.txt", "lire.txt", "Sorties.txt", "aff_quinson", "allos CR.txt", "00_ALIRE.txt"),
                             language = "fr") {

  # 1. Preparation -------------------------------------------------------------

  # Load required packages
  # NOTE: Ideally, packages should be loaded outside the function
  # or checked using DESCRIPTION / Imports
  suppressMessages(suppressWarnings(require(dplyr)))
  suppressMessages(suppressWarnings(require(lubridate)))

  # Date-time parsing orders used to automatically detect multiple formats
  custom_orders = c("d/m/y H:M:S", "y/m/d H:M:S","d-m-y H:M:S", "m/d/Y H:M:S","m/d/y H:M:S","d/m/Y H:M:S",
                    "Y-m-d H:M:S", "Y/m/d H:M:S", "d/m/y", "y/m/d", "Y/m/d", "d/m/Y",
                    "d/m/y H:M", "y/m/d H:M","d-m-y H:M", "m/d/Y H:M","m/d/y H:M","d/m/Y H:M",
                    "Y-m-d H:M", "Y/m/d H:M",
                    "d/m/y I:M:S p", "y/m/d I:M:S p","d-m-y I:M:S p", "m/d/Y I:M:S p","m/d/y I:M:S p","d/m/Y I:M:S p",
                    "Y-m-d I:M:S p", "Y/m/d I:M:S p", "d/m/y", "y/m/d", "Y/m/d", "d/m/Y",
                    "d/m/y I:M p", "y/m/d I:M p","d-m-y I:M p", "m/d/Y I:M p","m/d/y I:M p","d/m/Y I:M p",
                    "Y-m-d I:M p", "Y/m/d I:M p")

  # Supported file extensions
  which_file_extension = c(".txt", ".csv", ".TXT", ".CSV")

  # Optional manual column separator (used if auto-detection fails)
  whichsep_manual = NULL

  # Time zone used when parsing date-time values
  whichtz = "UTC"

  # Safety bounds for date values
  date_min <- "2000-01-01"
  date_max <- "2100-01-01"

  # Objects used to track processing status
  # - skipped_files: files that could not be read
  # - read_files: files successfully read
  # - log_progress: summary metadata for each processed file
  skipped_files = read_files = log_progress = NULL


  # 2. Read files --------------------------------------------------------------

  ## 2.1 Loop over all provided folders ----------------------------------------
  for(i in folderpath) {

    # Track processing time for the first folder
    if (i == folderpath[1]) temps = Sys.time()

    # Initialize date parsing orders for this folder
    custom_orders_to_use = custom_orders

    # List all files in the current folder
    files2read = list.files(i)

    # Keep only files with supported extensions
    sensor_files <- unlist(lapply(which_file_extension, function(x) files2read[grep(x, files2read)]))

    # Apply inclusion filters (e.g. station codes)
    if(!is.null(patterns2searchfor)) {
      for(p in patterns2searchfor) {
        sensor_files <- sensor_files[grep(p, sensor_files)]
      }
    }

    # Apply exclusion filters (e.g. status files, pressure-only files)
    if(!is.null(patterns2exclude)) {
      for(p in patterns2exclude) {
        sensor_files <- sensor_files[grep(p, sensor_files, invert = TRUE)]
      }
    }

    # Display progress message for the current folder
    if(language == "fr") {
      cat(paste0("\n\n(", which(folderpath == i), "/", length(folderpath),
                 ") Dans le dossier ", i, ", \nil y a ",
                 paste(ifelse(length(sensor_files)>0,
                              paste0(length(sensor_files), " fichiers \u224 lire"),
                              " 0 fichier \u224 lire")),
                 " \u224 lire.\n"))
    } else {
      cat(paste0("\n\n(", which(folderpath == i), "/", length(folderpath),
                 ") In the folder ", i, ", \nthere are ",
                 paste(ifelse(length(sensor_files)>0,
                              paste0(length(sensor_files), " files"),
                              " 0 file")),
                 " to read.\n"))
    }


    ## 2.2 Read each sensor file ------------------------------------------------

    # Initialize a list to store one data frame per file
    list_data <- vector(mode = "list", length = length(sensor_files))

    # Loop over all sensor files
    # Three main steps:
    #   1) Read the file
    #   2) Clean and standardize the data
    #   3) Store results and metadata
    for (j in seq_along(sensor_files)) {

      # Reset date parsing orders for each file
      custom_orders_to_use = custom_orders

      # Print file name and index
      cat(paste0("\n(", j, ") ", sensor_files[j], "  |  "))

      # 1) File reading --------------------------------------------------------

      # Attempt to detect the column separator by testing common delimiters
      # The correct separator should result in more than one column
      whichsep = NULL
      for(s in c(";", "\t", ",", " ")) {
        if(is.null(whichsep)) {
          test_read_file <- read.delim(paste(i, sensor_files[j], sep = "/"),
                                       # Skipping by default 10 rows in case there is a big header
                                       sep = s, skip = 10, nrows = 100,
                                       header = FALSE, fileEncoding="latin1",
                                       encoding = "latin1")
          if(ncol(test_read_file) > 1) whichsep = s
        }
      }

      # If automatic detection failed, use manual separator
      if(is.null(whichsep)) whichsep = whichsep_manual

      # If still NULL, stop and ask for user input
      if(is.null(whichsep)) {
        if(language == "fr")
          stop("Le s\u233parateur n'a pas \u233t\u233 d\u233tect\u233 automatiquement. Sp\u233cifiez-le via l'argument whichsep_manual.") else
          stop("The column separator could not be detected automatically. Please provide it using whichsep_manual.")

      }

      # Detect number of header rows to skip by scanning for column names
      # or numeric values indicating the start of data
      nrow2skip = NULL
      for (r in c(min(nrow(test_read_file), 100)):0) {
        if(is.null(nrow2skip)) {

          test_read_file <- read.delim(paste(i, sensor_files[j], sep = "/"),
                                       sep = whichsep, skip = r, nrows = 60,
                                       header = FALSE, fileEncoding="latin1")

          # Clean characters that may interfere with parsing
          test_read_file <- str_remove_all(test_read_file[1, ], "C")
          test_read_file <- str_remove_all(test_read_file, "<b0>")
          test_read_file <- str_remove_all(test_read_file, "\u176")
          test_read_file <- str_remove_all(test_read_file, " ")
          test_read_file <- str_replace_all(test_read_file, ",", ".")

          # Detect header row
          if(any(c("Date", "Heure", "GMT", "Temp") %in% test_read_file)) {
            nrow2skip = r
          } else {
            test_read_file <- as.numeric(paste(str_replace_all(test_read_file, ",", ".")))
            if(all(is.na(test_read_file))) nrow2skip = r
          }
        }
      }

      # Read the full file using detected parameters
      cat(paste0("Reading file using '", whichsep,
                 "' as column separator and skipping ",
                 nrow2skip,
                 ifelse(nrow2skip > 1, " rows.", " row.")))

      test_read_file <- read.delim(paste(i, sensor_files[j], sep = "/"),
                                   sep = whichsep, skip = 0,
                                   header = FALSE, fileEncoding = "latin1")

      # Remove header rows and assign column names
      list_data[[j]] <- test_read_file[-c(1:c(nrow2skip+1)),]
      names(list_data[[j]]) <- test_read_file[nrow2skip+1,]

      # Special case: file read as a single column
      if(ncol(test_read_file) == 1) {
        list_data[[j]] <- read.delim(paste(i, sensor_files[j], sep = "/"),
                                     sep = whichsep, skip = nrow2skip-1,
                                     header = TRUE, fileEncoding = "latin1")
      }

      read_files = c(read_files, sensor_files[j])

      # Remove first row if it only contains units
      if(all(is.na(as.numeric(paste(str_replace_all(list_data[[j]][1, ], ",", ".")))))) {
        list_data[[j]] <- list_data[[j]][-1, ]
      }


      # 2) Sensor identification -----------------------------------------------

      # Attempt to extract serial number, sensor type, and manufacturer
      num_serie = NA
      sensor = NA
      fabriquant_outil = NA

      if(length(grep(pattern = "S.N.|S/N", x = names(list_data[[j]]))) > 0) {
        #Found the pattern for serial number
        #Default hobo sensor?
        num_serie <- unlist(strsplit(names(list_data[[j]])[grep(pattern = "S.N.|S/N", x = names(list_data[[j]]))], split = "\\."))
        num_serie <- str_remove_all(num_serie, pattern = "[^0-9]")
        num_serie <- as.numeric(num_serie)
        num_serie <- num_serie[!is.na(num_serie)]
        if(length(num_serie)>3) num_serie <- num_serie[-1] # Ok gros bidouillage mais souvent il y a un probleme sur le premier avec le hobo..
        if(max(nchar(num_serie)) == 2*min(nchar(num_serie))) num_serie = num_serie[which(nchar(num_serie) == min(nchar(num_serie)))]
        if(length(unique(num_serie)) == 1) { # checking for repetition of the SN twice
          if(str_sub(unique(num_serie), 1, nchar(unique(num_serie))/2) == str_sub(unique(num_serie), nchar(unique(num_serie))/2+1, nchar(unique(num_serie)))) num_serie = str_sub(unique(num_serie), 1, nchar(unique(num_serie))/2)
        }
        if(length(num_serie)==0) num_serie = NA
        if(length(num_serie)>1) {
          if(length(unique(num_serie)) == 1) {
            num_serie = unique(num_serie)
          } else {
            print("Can't find the serial number, multiple options. Check code")
            num_serie = NA

          }
        }
        if(nchar(num_serie) == 8) {
          # Most likely hobo
          sensor = "HOBO"
          fabriquant_outil = "Onset"
        } else {
          sensor = NA
          fabriquant_outil = NA
        }
      } else {
        # Serial number on the first line
        if(length(grep(pattern = "Num\u233ro de s\u233rie", x = read.delim(paste(i, sensor_files[j], sep = "/"), sep = whichsep, skip = 0, nrows = 1, header = FALSE, encoding = "utf-8")))>0) {
          num_serie <- read.delim(paste(i, sensor_files[j], sep = "/"), sep = whichsep, skip = 0, nrows = 1, header = FALSE, encoding = "utf-8")
          num_serie <- str_remove_all(num_serie, pattern = "[^0-9]")
          num_serie <- as.numeric(num_serie)
          num_serie <- num_serie[!is.na(num_serie)]
          if(length(num_serie)==0) num_serie = NA
          if(length(num_serie)>1) {
            if(length(unique(num_serie)) == 1) {
              num_serie = unique(num_serie)
            } else {
              print("Can't find the serial number, multiple options. Check code")
              num_serie = NA
            }
          }
          if(nchar(num_serie) == 8) {
            # Most likely hobo
            sensor = "HOBO"
            fabriquant_outil = "Onset"
          } else {
            sensor = NA
            fabriquant_outil = NA
          }
        } else {
          # Serial number for TinyTags
          if(length(grep(pattern = "S.N.|S/N", x = test_read_file[1:10,1], ignore.case = TRUE)) > 0) {
            num_serie = test_read_file[grep(pattern = "S.N.|S/N", x = test_read_file[1:10,1], ignore.case = TRUE), ncol(test_read_file)]
            sensor = test_read_file[grep(pattern = "Type", x = test_read_file[1:10,1], ignore.case = TRUE), ncol(test_read_file)]
            fabriquant_outil = "Gemini"
          } else {
            num_serie = NA
            sensor = NA
            fabriquant_outil = NA
          }
        }
      }

      # miniDot
      if(any(grepl("minidot", test_read_file[1:10,1], ignore.case = TRUE))) {
        num_serie = str_remove(string = test_read_file[2,1], pattern = "Sensor:")
        num_serie = str_remove_all(string = num_serie, pattern = " ")
        sensor = "MiniDOT"
        fabriquant_outil = "PME"
      }

      cat(paste0(" Sensor: ", sensor, " (", fabriquant_outil,", SN: ", num_serie,"). "))


      # 3) Data cleaning and standardization -----------------------------------

      # Remove row index column if detected
      if(all(!is.na(as.numeric(list_data[[j]][,1]))))
        if(all(list_data[[j]][,1] %in% 1:nrow(list_data[[j]])) |
           all(diff(as.numeric(paste(list_data[[j]][,1]))) == 1) |
           length(unique(diff(as.numeric(paste(list_data[[j]][,1]))))) < .9*nrow(list_data[[j]])) {
          list_data[[j]] <- list_data[[j]][, -1]
        }

      # Handling dates
      # Ici, on verifie que la (nouvelle) premiere colonne a bien des dates
      # Echantillon de dates
      dates_brutes <- list_data[[j]][,1][1:min(c(nrow(list_data[[j]]), 200))]
      if(all(nchar(dates_brutes) <= 2)) {
        dates_brutes <- row.names(list_data[[j]])[1:min(c(nrow(list_data[[j]]), 200))]
        datescol = "row"
      } else {
        datescol = "col1"
      }
      # Il y a parfois des virgules apres les secondes : les supprimer
      dates_brutes <- str_remove( dates_brutes, ",0")
      # On converti
      nouvelles_dates <- parse_date_time(x = dates_brutes,
                                         orders = custom_orders_to_use, tz=whichtz)

      # On verifie que les dates ont ete converties:
      if(length(nouvelles_dates[is.na(nouvelles_dates)])>0) {
        message(paste0("Dossier ",unlist(lapply(strsplit(i, split = "/"), tail, 1)),"\nProbleme de conversion des heures avec le fichier ", sensor_files[j], ". Verifier le code et/ou le fichier source."))
      }

      # Dans certains cas, l'ordre ne permet pas de bien avoir la date (ca converti en d/m/y au lieu de y/m/d).
      # On echange alors l'ordre et on fait tourner le meme code.
      if(any(abs(diff(nouvelles_dates)/3600/24) > 2)) { # Ici on verifie qu'on n'a pas plus de 2 jours de difference entre deux valeurs; en realite, si erreur, l'anomalie est plutot de 365 jours
        custom_orders_to_use = c("y/m/d H:M:S", "y/m/d H:M", "d/m/y H:M:S", "d/m/y H:M",
                                 "d-m-y H:M:S",  "d-m-y H:M",
                                 "m/d/Y H:M:S","m/d/Y H:M",
                                 "m/d/y H:M:S","m/d/y H:M",
                                 "d/m/Y H:M:S","d/m/Y H:M",
                                 "Y-m-d H:M:S", "Y-m-d H:M",
                                 "Y/m/d H:M:S", "Y/m/d H:M",
                                 "y/m/d", "d/m/y", "Y/m/d", "d/m/Y")

        # Echantillon de dates
        if(datescol == "col1") dates_brutes <- list_data[[j]][,1][1:min(c(nrow(list_data[[j]]), 200))]
        if(datescol == "row") dates_brutes <- row.names(list_data[[j]])[1:min(c(nrow(list_data[[j]]), 200))]
        # Il y a parfois des virgules apres les secondes : les supprimer
        dates_brutes <- str_remove( dates_brutes, ",0")
        # On converti
        nouvelles_dates <- parse_date_time(x = dates_brutes,
                                           orders = custom_orders_to_use, tz=whichtz)

        # On verifie que les dates ont ete converties:
        if(length(nouvelles_dates[is.na(nouvelles_dates)])>0) {
          message(paste0("Dossier ",unlist(lapply(strsplit(i, split = "/"), tail, 1)),"\nProbleme de conversion des heures avec le fichier ", sensor_files[j], ". Verifier le code et/ou le fichier source."))
        }
      }

      # Deuxieme verif, avec encore un autre systeme (pour les tinytags recalcitrants)
      if(any(abs(diff(nouvelles_dates)/3600/24) > 2)) { # Ici on verifie qu'on n'a pas plus de 2 jours de difference entre deux valeurs; en realite, si erreur, l'anomalie est plutot de 365 jours
        custom_orders_to_use = c("d/m/y H:M")

        # Echantillon de dates
        if(datescol == "col1") dates_brutes <- list_data[[j]][,1][1:min(c(nrow(list_data[[j]]), 200))]
        if(datescol == "row") dates_brutes <- row.names(list_data[[j]])[1:min(c(nrow(list_data[[j]]), 200))]
        # Il y a parfois des virgules apres les secondes : les supprimer
        dates_brutes <- str_remove( dates_brutes, ",0")
        # On converti
        nouvelles_dates <- parse_date_time(x = dates_brutes,
                                           orders = custom_orders_to_use, tz=whichtz)

        # On verifie que les dates ont ete converties:
        if(length(nouvelles_dates[is.na(nouvelles_dates)])>0) {
          message(paste0("Dossier ",unlist(lapply(strsplit(i, split = "/"), tail, 1)),"\nProbleme de conversion des heures avec le fichier ", sensor_files[j], ". Verifier le code et/ou le fichier source."))
        }
      }

      if(any(abs(diff(nouvelles_dates)/3600/24) > 2)) { # Ici on verifie qu'on n'a pas plus de 2 jours de difference entre deux valeurs; en realite, si erreur, l'anomalie est plutot de 365 jours
        stop("Probleme de lecture de date : verifier le code et ajouter un champ au 'custom_orders_to_use' dans la boucle")
      }


      # On le fait pour tout
      if(datescol == "col1") dates_brutes <- list_data[[j]][,1]
      if(datescol == "row") dates_brutes <- row.names(list_data[[j]])

      # Il y a parfois des virgules apres les secondes : les supprimer
      dates_brutes <- str_remove( dates_brutes, ",0")
      # On converti
      nouvelles_dates <- parse_date_time(x = dates_brutes,
                                         orders = custom_orders_to_use,
                                         tz=whichtz)


      # Si nchar(nouvelles_dates) == 10, c'est que les heures sont dans la colonne suivante
      if(datescol == "col1") {
        if(all(nchar(as.character(c(nouvelles_dates %>% paste() %>% head(3)))) == 10)) {
          heures_brutes <- list_data[[j]][,2]
          heures_brutes <- str_remove(heures_brutes, ",0")
          dates_brutes <- paste(as.character(nouvelles_dates), heures_brutes)

          nouvelles_dates <- parse_date_time(x = dates_brutes,
                                             orders = custom_orders_to_use, tz=whichtz)

          # Donnees temperatures sont dans la colonne suivant
          whichcoltemp = 3
          temp <- str_replace(list_data[[j]][,whichcoltemp], ",", ".")
          temp <- str_remove_all(temp, "C")
          temp <- str_remove_all(temp, "<b0>")
          temp <- str_remove_all(temp, "\u233")
          temp <- str_remove_all(temp, " ")
          temp <- str_replace_all(temp, ",", ".")
          temp <- as.numeric(paste(temp))
          temp_r <- round(temp,0)
          if(all(temp[!is.na(temp)]-temp_r[!is.na(temp)] == 0)) { # Decimals are in the next column
            temp <- as.numeric(paste0(temp, ".", list_data[[j]][,4]))
          }
          rm(temp_r)

        } else {
          # Donnees temperatures sont dans la colonne suivant
          whichcoltemp = 2
          temp <- str_replace(list_data[[j]][,whichcoltemp], ",", ".")
          temp <- str_remove_all(temp, "C")
          temp <- str_remove_all(temp, "<b0>")
          temp <- str_remove_all(temp, "\u233")
          temp <- str_remove_all(temp, " ")
          temp <- str_replace_all(temp, ",", ".")
          temp <- as.numeric(paste(temp))
          temp_r <- round(temp,0)
          if(all(temp[!is.na(temp)]-temp_r[!is.na(temp)] == 0)) { # Decimals are in the next column
            temp <- as.numeric(paste0(temp, ".", list_data[[j]][,3]))
          }
          rm(temp_r)
        }
      }
      if(datescol == "row") {
        # Donnees temperatures sont dans la colonne suivant
        whichcoltemp = 2
        temp <- as.numeric(str_replace(list_data[[j]][,1], ",", "."))
        temp_r <- round(temp,0)
        if(all(temp[!is.na(temp)]-temp_r[!is.na(temp)] == 0)) { # Decimals are in the next column
          temp <- as.numeric(paste0(temp, ".", list_data[[j]][,whichcoltemp]))
        }
        rm(temp_r)
      }

      head(nouvelles_dates);tail(nouvelles_dates)

      # Specific case: oxygen and/or pressure also available####
      # First, check easy error temp>40 - impossible
      pres = NA
      o2concentration = NA
      o2saturation = NA
      if(length(grep(pattern = "pres|saturation|%|mg/l", names(list_data[[j]]), ignore.case = TRUE))>=1) {
        whichcoltemp = which(grepl(pattern = "temp", names(list_data[[j]]), ignore.case = TRUE))
        whichcolpres = which(grepl(pattern = "pres", names(list_data[[j]]), ignore.case = TRUE))
        whichcolo2concentration = which(grepl(pattern = "oxygen|mg/l|mg.l", names(list_data[[j]]), ignore.case = TRUE) & grepl(pattern = "saturation", names(list_data[[j]]), ignore.case = TRUE))
        whichcolo2sat = which(grepl(pattern = "saturation|%", names(list_data[[j]]), ignore.case = TRUE))

        if(length(whichcoltemp) != 1) stop("Did not find temperature column - check code.") else {
          temp <- str_replace(list_data[[j]][,whichcoltemp], ",", ".")
          temp <- str_remove_all(temp, "C")
          temp <- str_remove_all(temp, "<b0>")
          temp <- str_remove_all(temp, "\u233")
          temp <- str_remove_all(temp, " ")
          temp <- str_replace_all(temp, ",", ".")
          temp <- as.numeric(paste(temp))
          temp_r <- round(temp,0)
          if(all(temp[!is.na(temp)]-temp_r[!is.na(temp)] == 0)) { # Decimals are in the next column
            temp <- as.numeric(paste0(temp, ".", list_data[[j]][,3]))
          }
          rm(temp_r)
        }

        if(length(whichcolpres) != 1) cat("Found 0 or several column that could have pressure - check code.") else {
          pres <- str_replace(list_data[[j]][,whichcolpres], ",", ".")
          pres <- str_remove_all(pres, " ")
          pres <- str_replace_all(pres, ",", ".")
          pres <- as.numeric(paste(pres))
        }

        if(length(whichcolo2concentration) != 1) cat("Found 0 or several column that could have pressure - check code.") else {
          o2concentration <- str_replace(list_data[[j]][,whichcolo2concentration], ",", ".")
          o2concentration <- str_remove_all(o2concentration, " ")
          o2concentration <- str_replace_all(o2concentration, ",", ".")
          o2concentration <- as.numeric(paste(o2concentration))
        }

        if(length(whichcolo2sat) != 1) cat("Found 0 or several column that could have pressure - check code.") else {
          o2saturation <- str_replace(list_data[[j]][,whichcolo2sat], ",", ".")
          o2saturation <- str_remove_all(o2saturation, " ")
          o2saturation <- str_replace_all(o2saturation, ",", ".")
          o2saturation <- as.numeric(paste(o2saturation))
        }
      }
      # Add a name if column name is empty
      names(list_data[[j]])[names(list_data[[j]]) == ""] <- paste0("sans_nom_", LETTERS[1:length(names(list_data[[j]])[names(list_data[[j]]) == ""])])
      names(list_data[[j]])[duplicated(names(list_data[[j]]))] <- paste0("nom_duplique_", LETTERS[1:length(names(list_data[[j]])[duplicated(names(list_data[[j]]))])])

      # Assemble standardized output columns
      # Identify date column and parse date-time values
      # Extract temperature, pressure, and oxygen variables if present
      # Handle different sensor layouts and decimal formats
      list_data[[j]] <- list_data[[j]] %>% mutate(
        DateTime_raw = nouvelles_dates,
        DateTime_UTC = with_tz(nouvelles_dates, tz="UTC"),
        TmpWtr_degC = temp,
        Pres = pres,
        O2conc_mgl = o2concentration,
        O2sat_per = o2saturation
      ) %>%
        select(DateTime_raw, DateTime_UTC, TmpWtr_degC, Pres, O2conc_mgl, O2sat_per)


      # Add sensor metadata
      list_data[[j]] <- list_data[[j]] %>%
        dplyr::mutate(
          fabriquant = fabriquant_outil,
          outil = sensor,
          SN = as.character(num_serie),
          .before = 1)

      # Add file-level metadata
      list_data[[j]]$fichier <- sensor_files[j]
      list_data[[j]]$dossier <- i
      list_data[[j]]$fichier_index <- j


      # 4) Save processing metadata --------------------------------------------

      log_progress <- rbind(
        log_progress,
        data.frame(
          chemin = i,
          fichier = sensor_files[j],
          outil = sensor,
          fabriquant = fabriquant_outil,
          start_datetime = min(list_data[[j]][,"DateTime_UTC"]),
          end_datetime = max(list_data[[j]][,"DateTime_UTC"]),
          foundpres = ifelse(all(is.na(pres)), FALSE, TRUE),
          foundO2concentration = ifelse(all(is.na(o2concentration)), FALSE, TRUE),
          foundO2sat = ifelse(all(is.na(o2saturation)), FALSE, TRUE)
        )
      )

      # Append data to global list
      if(i == folderpath[1]) {
        data_all <- list_data
      } else {
        data_all <- c(data_all, list_data)
      }
    }
  }

  # Print total processing time
  Sys.time() - temps

  # Return assembled data and metadata
  return(list(
    data_list = data_all,
    skipped_files = skipped_files,
    read_files = read_files,
    log_progress = log_progress
  ))
}

