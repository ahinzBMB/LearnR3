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
