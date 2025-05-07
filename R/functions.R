#' Importing data from DIME dataset
#'
#' @param file_path Path to the CSV file
#'
#' @returns Returns a dataframe
#'
import_dime <- function(file_path) {
  data <- file_path |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = 100
    )

  return(data)
}


#' Import all DIME csv files in a folder into one data frame.
#'
#' @param folder_path the path to the folder that has the csv files
#'
#' @returns A single data frame/tibble
#'
import_csv_files <- function(folder_path) {
  files <- folder_path |>
    fs::dir_ls(glob = "*.csv")

  data <- files |>
    purrr::map(import_dime) |>
    purrr::list_rbind(names_to = "file_path_id")
  return(data)
}

#' Extracting ID from file_path_id
#'
#' @param data the assigned data sheet
#'
#' @returns A data frame with the file_path_id removed and replaced by only the sample ID.
#'
get_participant_id <- function(data) {
  data <- data |>
    dplyr::mutate(
      id = stringr::str_extract(
        file_path_id, "[:digit:]+\\.csv$"
      ) |>
        stringr::str_remove("\\.csv$") |>
        as.integer(),
      .before = file_path_id
    ) |>
    select(-file_path_id)
  return(data)
}

# Using REGEX for ID extraction
# mutate() creates a new column in the dataframe, this will be put into a new column named id with the id_information from "[:digit:]+"...

#' Clearning up dates and timestamps
#'
#' @param data
#' @param column
#'
#' @returns
#'
prepared_dates <- function(data, column) {
  prepared_dates <- data |>
    dplyr::mutate(
      date = lubridate::as_date({{ column }}),
      hour = lubridate::hour({{ column }}),
      .before = {{ column }}
    )
  return(prepared_dates)
}



#' Clean and prepared the CGM data for joining
#'
#' @param data The CGM dataset
#'
#' @returns A cleaner dataframe
#'
clean_cgm <- function(data) {
  cleaned <- data |>
    get_participant_id() |>
    prepared_dates(device_timestamp) |>
    dplyr::rename(glucose = historic_glucose_mmol_l)
  return(cleaned)
}
