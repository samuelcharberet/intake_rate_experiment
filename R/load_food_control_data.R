#' load_food_control_data
#' 
#' @return a clean data tibble containing information on food controls chemical analysis from the intake rate experiment
#'
load_food_control_data = function(path){
  
  ##########  0. Load data  ##########
  
  data_irn_food_control <- readr::read_delim(path)
  
  ##########  1. Structuration  ##########
  
  # Decide column classes
  
  character_columns = c(
    "tube_food_control_unit"
  )
  date_columns = c(
    "date"
  )
  
  factor_columns = c(
    "tube_food_control_ID"
  )
  numeric_columns = c(
    "empty_tube_food_control_mass",
    "ww_filled_tube_food_control_mass",
    "dw_filled_tube_food_control_mass",
    "food_ww",
    "food_dw",
    "food_water_content"
  )
  
  # We define the type of each column
  
  data_irn_food_control <- data_irn_food_control |>
    dplyr::mutate(across(character_columns, as.character)) |>
    dplyr::mutate(across(date_columns, ~ as.POSIXct(.x, format = "%d/%m/%Y"))) |>
    dplyr::mutate(across(factor_columns, as.factor)) |>
    dplyr::mutate(across(numeric_columns, as.numeric))
  
  return(data_irn_food_control)
}