#' load_group_data
#' 
#' @return a clean data tibble containing information on group chemical analysis from the intake rate experiment
#'
load_group_data = function(path){
  
  ##########  0. Load data  ##########
  
  data_irn_groups <- readr::read_delim(path)
  
  ##########  1. Structuration  ##########
  
  # Decide column classes
  
  character_columns = c(
    "food_intake_unit",
    "frass_group_mass_dw_unit"
  )
  
  factor_columns = c(
    "group_ID",
    "treatment_ID",
    "tube_frass_ID"
  )
  
  numeric_columns = c(
    "food_intake ",
    "frass_group_mass_dw",
    "nb_days_collection",
    "C_frass",
    "N_frass",
    "P_frass",
    "S_frass",
    "Na_frass",
    "Mg_frass",
    "K_frass",
    "Ca_frass",
    "d13C_frass",
    "d15N_frass",
    "C_larvae",
    "N_larvae",
    "P_larvae",
    "S_larvae",
    "Na_larvae",
    "Mg_larvae",
    "K_larvae",
    "Ca_larvae",
    "d13C_larvae",
    "d15N_larvae"
  )
  
  # We define the type of each column
  
  data_irn_groups <- data_irn_groups |>
    dplyr::mutate(across(tidyselect::all_of(character_columns), as.character)) |>
    dplyr::mutate(across(tidyselect::all_of(factor_columns), as.factor)) |>
    dplyr::mutate(across(tidyselect::all_of(numeric_columns), as.numeric))
  
  return(data_irn_groups)
  
  }