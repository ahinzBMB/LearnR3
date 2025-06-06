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
      name_repair = snakecase::to_snake_case
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
    dplyr::rename(glucose = historic_glucose_mmol_l) |>
    summarize_column(glucose, list(mean = mean, sd = sd))
  return(cleaned)
}


clean_sleep <- function(data) {
  cleaned_sleep <- data |>
    get_participant_id() |>
    rename(datetime = date) |>
    prepared_dates(datetime) |>
    summarize_column(seconds, list(sum = sum)) |>
    sleep_types_to_wider()
  return(cleaned_sleep)
}

#' Summarize a single column based on one or more functions
#'
#' @param data Either the CGM or Sleep data in DIME
#' @param column The column we want to summarize
#' @param functions One or more functions to apply to the column. If more than one added, use list()
#'
#' @returns
summarize_column <- function(data, column, functions) {
  summarized_data <- data |>
    dplyr::select(
      -tidyselect::contains("timestamp"),
      -tidyselect::contains("datetime")
    ) |>
    dplyr::group_by(dplyr::pick(-{{ column }})) |>
    dplyr::summarize(
      dplyr::across(
        {{ column }},
        functions
      ),
      .groups = "drop"
    )
  return(summarized_data)
}

#' Convert the participant details data to long format and clean it up
#'
#' @param data The DIME participant details data
#'
#' @returns A data frame
#'
clean_participant_details <- function(data) {
  cleaned <- data |>
    tidyr::pivot_longer(tidyselect::ends_with("date"), names_to = NULL, values_to = "date") |>
    dplyr::group_by(dplyr::pick(-date)) |>
    tidyr::complete(
      date = seq(min(date), max(date), by = "1 day")
    )
  return(cleaned)
}

sleep_types_to_wider <- function(data) {
  wider <- data |>
    tidyr::pivot_wider(
      names_from = sleep_type,
      names_prefix = "seconds_",
      values_from = seconds_sum
    )
  return(wider)
}

