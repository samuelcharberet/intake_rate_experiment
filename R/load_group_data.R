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
    "egestion_group_mass_unit"
  )
  
  factor_columns = c(
    "group_ID",
    "treatment_ID",
    "tube_egestion_ID"
  )
  
  numeric_columns = c(
    "food_intake ",
    "egestion_group_mass",
    "nb_days_collection",
    "egestion_C",
    "egestion_N",
    "egestion_P",
    "egestion_K",
    "egestion_Mg",
    "egestion_Ca",
    "larvae_C",
    "larvae_N",
    "larvae_P",
    "larvae_K",
    "larvae_Mg",
    "larvae_Ca"
  )
  
  # We define the type of each column
  
  data_irn_groups <- data_irn_groups |>
    dplyr::mutate(across(character_columns, as.character)) |>
    dplyr::mutate(across(factor_columns, as.factor)) |>
    dplyr::mutate(across(numeric_columns, as.numeric))
  
  return(data_irn_groups)
  
  }