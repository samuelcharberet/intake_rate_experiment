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
    "fw_filled_tube_food_control_mass",
    "dw_filled_tube_food_control_mass",
    "food_fw",
    "food_dw",
    "food_water_content"
  )
  
  # We define the type of each column
  
  data_irn_food_control <- data_irn_food_control |>
    dplyr::mutate(across(tidyselect::all_of(character_columns), as.character)) |>
    dplyr::mutate(across(tidyselect::all_of(date_columns), ~ as.POSIXct(.x, format = "%d/%m/%Y"))) |>
    dplyr::mutate(across(tidyselect::all_of(factor_columns), as.factor)) |>
    dplyr::mutate(across(tidyselect::all_of(numeric_columns), as.numeric))
  
  ##### Food water content #####
  
  data_irn_food_control$food_fw = data_irn_food_control$fw_filled_tube_food_control_mass -
    data_irn_food_control$empty_tube_food_control_mass
  
  data_irn_food_control$food_dw = data_irn_food_control$dw_filled_tube_food_control_mass -
    data_irn_food_control$empty_tube_food_control_mass
  
  data_irn_food_control$food_water_content = (data_irn_food_control$food_fw - data_irn_food_control$food_dw) /
    data_irn_food_control$food_fw
  
  data_irn_food_control$food_C = 0.5
  data_irn_food_control$food_N = 0.1
  data_irn_food_control$food_P = 0.05
  
  
  
  return(data_irn_food_control)
}