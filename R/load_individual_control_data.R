#' load_individual_control_data
#'
#' @return a clean data tibble containing information on control individuals from the intake rate experiment
#'
load_individual_control_data = function(path) {
  ##########  0. Load data  ##########
  
  data_irn_individuals_controls <- readr::read_delim(path)
  
  ##########  1. Structuration  ##########
  
  # Decide column classes
  
  date_columns = c("date")
  
  factor_columns = c("indiv_ID")
  
  numeric_columns = c("bodymass_7th_instar_j0_fw", "bodymass_7th_instar_j0_dw", "indiv_water_content")
  
  # We define the type of each column
  
  data_irn_individuals_controls <- data_irn_individuals_controls |>
    dplyr::mutate(across(tidyselect::all_of(date_columns), ~ as.POSIXct(.x, format = "%d/%m/%Y"))) |>
    dplyr::mutate(across(tidyselect::all_of(factor_columns), as.factor)) |>
    dplyr::mutate(across(tidyselect::all_of(numeric_columns), as.numeric))
  
  data_irn_individuals_controls$indiv_water_content = 1-data_irn_individuals_controls$bodymass_7th_instar_j0_dw/data_irn_individuals_controls$bodymass_7th_instar_j0_fw
  
  
  return(data_irn_individuals_controls)
}